###########################################
## model time
###########################################

rm(list = ls())

## load experimental data
files <- fs::dir_ls("~/Downloads/Florida")
load(files[1])
load(files[2])
load(files[3])

## fips
state <- "12"
name <- "Florida"

## packages
library(tidyverse)
library(tidymodels)
library(spatialsample)
library(sf)

## data preparation
all_variables <- 
  regression |> 
  mutate_at(vars(starts_with("cut")), factor) |> 
  mutate(built_change = factor(built_change)) |>
  left_join(infill) |>
  mutate(county = str_sub(GEOID, 1, 5),
         impervious_change_lag3 = case_when(is.infinite(impervious_change_lag3) ~ 1,
                                            TRUE ~ impervious_change_lag3),
         impervious_change = case_when(is.infinite(impervious_change) ~ 1,
                                       TRUE ~ impervious_change)) |>
  st_as_sf()

## geometries
blocks <- select(all_variables, GEOID)

## train/test split
# options: 
# - random split
# - holdout county

# start with a random split...
set.seed(42)

# go with a 50/50 split since we have the full sample, try 70/30 when charged and ready
split <- 
  all_variables |>
  drop_na() |>
  initial_split(0.5)

train <- training(split)
test <- testing(split)

## spatial k-fold
# folds <- rsample::group_vfold_cv(train, group = "county")
# folds <- rsample::vfold_cv(train, 10)
folds <- spatialsample::spatial_clustering_cv(st_as_sf(train), cluster_function = 'hclust', v = 10)
autoplot(folds)

drop <- c("GEOID", "county", "maxhd_micro", "built_change", "cut00to19", "cut10to19")

## lasso regression
lasso_recipe <- 
  recipe(maxhd_macro ~ ., 
         data = select(train, -geometry)) |>
  step_rm(any_of(drop)) |> 
  step_string2factor(matches("cut[0-9][0-9]to19$")) |>
  step_scale(all_numeric_predictors()) |>
  step_center(all_numeric_predictors()) |>
  # step_pca(matches("^slope"), prefix = "slope_", num_comp = 1) |>
  # step_pca(matches("^ND.*"), prefix = "spectral_indices_", num_comp = 5) |>
  # step_pca(matches("^d_.*_school"), prefix = "nearest_school_", num_comp = 5) |>
  # step_pca(matches("^d_.*_airport"), prefix = "nearest_airport_", num_comp = 5) |>
  # step_pca(matches("^d_college"), prefix = "nearest_college_", num_comp = 5) |>
  # step_pca(matches("^d_hospital"), prefix = "nearest_hospital_", num_comp = 5) |>
  # step_pca(matches("d_transit|d_amtrak"), prefix = "nearest_transport_", num_comp = 5) |>
  # step_pca(matches("_price|_value"), prefix = "home_value_", num_comp = 5) |>
  # step_pca(matches("nightlights_"), prefix = "lights_", num_comp = 5) |>
  # step_pca(matches("pop_"), prefix = "population_", num_comp = 5) |>
  # step_pca(matches("impervious"), prefix = "impervious_", num_comp = 5) |>
  # step_pca(matches("development"), prefix = "development_", num_comp = 5) |>
  # step_pca(matches("prop_4way|intersection|orientation"), prefix = "topology_", num_comp = 5) |>
  # step_pca(matches("building_size|isoperi_mean|built_intensity"), prefix = "morphology_", num_comp = 5) |>
  # step_pca(matches("pct_|household_size|median_income"), prefix = "demography_", num_comp = 5) |>
  # step_pca(matches("dist_"), prefix = "proximity_", num_comp = 2) |>
  # step_pca(starts_with("density"), prefix = "density_", num_comp = 2) |>
  step_rm(ends_with("19_lag3")) |>
  step_dummy(all_nominal()) |> 
  step_log(all_outcomes(), offset = 1)

lasso_prep <- prep(lasso_recipe)
lasso_proc <- juice(lasso_prep) |> select(maxhd_macro, everything())

lasso_spec <- 
  linear_reg(penalty = tune(), mixture = tune()) |> 
  set_engine("glmnet")

lasso_flow <- 
  workflow() |>
  add_recipe(lasso_recipe) |>
  add_model(lasso_spec)

tuning_param <- parameters(penalty(range = c(0, 1), trans = NULL), mixture(range = c(0, 1), trans = NULL))
tuning_space <- grid_max_entropy(tuning_param, size = 216)

perf_metrics <- metric_set(mae)

doParallel::registerDoParallel(cores = 4)
tictoc::tic()
tuning_lasso <- 
  tune_grid(
    lasso_flow,
    resamples = folds,
    grid = tuning_space,
    metrics = perf_metrics
  )
tictoc::toc()

plot_lasso_hyperparameters(tuning_lasso, "mae", state)
plot_lasso_features(tuning_lasso, lasso_recipe, lasso_spec, train, test, state)

relevant_features <- get_lasso_features(tuning_lasso, lasso_recipe, lasso_spec, train, test)

lasso_best <- finalize_model(lasso_spec, select_best(tuning_lasso, "mae"))

lasso_fit <- 
  lasso_best |>
  set_engine("glmnet") |>
  fit(maxhd_macro ~ .,
      # this is the original data, so we fit on the training (no leakage)
      data = lasso_proc
  )

lasso_out <- juice(prep(shop(test, drop)))
lasso_in <- juice(prep(shop(train, drop)))

bind_rows(predict(lasso_fit, lasso_in) |>
            bind_cols(lasso_in) |>
            transmute(predicted = exp(.pred), observed = exp(maxhd_macro), sample = "in") |>
            mutate(error = abs(observed - predicted), 
                   group = ntile(observed, 10)) |>
            group_by(group = as.character(group), sample) |>
            summarise(predicted = mean(predicted),
                      observed = mean(observed),
                      error = mean(error)), 
          predict(lasso_fit, lasso_out) |>
            bind_cols(lasso_out) |>
            transmute(predicted = exp(.pred), observed = exp(maxhd_macro), sample = "out") |>
            mutate(error = abs(observed - predicted), 
                   group = ntile(observed, 10) + 0.25) |>
            group_by(group = as.character(group), sample) |>
            summarise(predicted = mean(predicted),
                      observed = mean(observed),
                      error = mean(error))) |>
  mutate(group = parse_number(group),
         position = case_when(predicted > observed ~ predicted,
                              TRUE ~ observed)) |>
  ggplot() +
  geom_segment(aes(x = group, xend = group, y = predicted, yend = observed, colour = sample), 
               size = 5) + 
  geom_text(aes(x = group, y = position + 3, label = round(error, 1), colour = sample), angle = 90) + 
  scale_colour_brewer(palette = 'Set1', name = "Sample...") +
  # scale_x_discrete(labels = c("rural", "exurban", "suburban", "urban")) +
  # scale_y_log10() +
  scale_x_discrete(breaks = c(), labels = rep("", 10)) +
  labs(title = glue::glue("Comparing Predicted and Obversed in {name}"),
       subtitle = "Allowable Housing Density", 
       x = "< less dense | more dense >", y = "housing density") +
  theme_minimal() +
  theme(plot.title = element_text(face = 'bold'),
        plot.background = element_rect(fill = '#ffffff'),
        legend.position = c(0.2, 0.8))

ggsave(plot = last_plot(), filename = glue::glue("lasso_comparison_{state}.png"), height = 6, width = 8, dpi = 300)

## pca
pca_recipe <- 
  recipe(maxhd_macro ~ ., 
         data = select(train, -geometry)) |>
  step_rm(any_of(drop)) |> 
  step_rm(matches("cut[0-9][0-9]to19$")) |>
  step_scale(all_numeric_predictors()) |>
  step_center(all_numeric_predictors()) |>
  step_pca(all_numeric_predictors(), num_comp = 20) |>
  step_dummy(all_nominal()) |> 
  step_log(all_outcomes(), offset = 1)

pca_prep <- prep(pca_recipe)
pca_proc <- juice(pca_prep) |> select(maxhd_macro, everything())

corrplot::corrplot(cor(pca_proc))

lm(maxhd_macro ~ .,
   data = pca_proc) |>
  summary()

lm(maxhd_macro ~ .,
   data = lasso_out |> select(maxhd_macro, all_of(relevant_features))) |> 
  summary()

drop <- c("GEOID", "maxhd_micro", "built_change")

## random forest
rf_recipe <- shop(train, drop) |> step_select(c("maxhd_macro", all_of(relevant_features)))

rf_prep <- prep(rf_recipe)
rf_proc <- juice(rf_prep) |> select(maxhd_macro, everything())

rf_spec <- 
  rand_forest(trees = tune(),
              mtry = tune(),
              min_n = tune()) |>
  set_mode("regression") |>
  set_engine("ranger")

rf_flow <- 
  workflow() |>
  add_recipe(rf_recipe) |>
  add_model(rf_spec)

tuning_param <- parameters(trees(range = c(500, 1000), trans = NULL), 
                           mtry(range = c(0, 10), trans = NULL),
                           min_n(range = c(0, 10), trans = NULL))

tuning_space <- grid_max_entropy(tuning_param, size = 216)

perf_metrics <- metric_set(mae)

doParallel::registerDoParallel(cores = 6)
tictoc::tic()
tuning_rf <- 
  tune_grid(
    rf_flow,
    resamples = folds,
    grid = tuning_space,
    metrics = perf_metrics
  )
tictoc::toc()

rf_best <- finalize_model(rf_spec, select_best(tuning_rf, "mae"))

importance_in <- 
  map_df(1:10,
         ~rf_best |>
           set_engine("ranger", importance = "permutation") |>
           fit(maxhd_macro ~ .,
               data = rf_proc) |>
           vip::vi() |>
           janitor::clean_names() |>
           mutate(run = .x)) 

rf_fit <- 
  rf_best %>%
  set_engine("ranger") %>%
  fit(maxhd_macro ~ .,
      # this is the original data, so we fit on the training (no leakage)
      data = rf_proc
  )

rf_in <-juice(prep(shop(train, drop)))
rf_out <- juice(prep(shop(test, drop)))

importance_out <- 
  map_df(1:10,
         ~rf_best |>
           set_engine("ranger", importance = "permutation") |>
           fit(maxhd_macro ~ .,
               data = rf_out) |>
           vip::vi() |>
           janitor::clean_names() |>
           mutate(run = .x)) 

ggplot(bind_rows(mutate(importance_in, sample = "in"),
                 mutate(importance_out, sample = "out")) |>
         mutate(variable = str_replace_all(variable, "_", " ")),
       aes(x = reorder(variable, importance), y = importance, colour = sample, group = variable)) +
  geom_point(shape = 'I', size = 7) +
  scale_colour_brewer(type = 'qual', guide = 'none') + 
  labs(x = "", y = "", title = "Permutation Importance", subtitle = name, caption = "Numbers indicate the principle component, colors indicate the interation of computation (n = 100 runs)") + 
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(face = 'bold'),
        plot.background = element_rect(fill = '#ffffff'))

ggsave(plot = last_plot(), filename = glue::glue("VIP_{state}.png"), height = 16, width = 8)

plot_comparison(test_model, rf_in, rf_out, state, name, 10)

## save
model_12 <- rf_fit
saveRDS(model_12, file = glue::glue("model_{state}_{rf_fit$spec$engine}.RDS"))
test_mod <- readRDS(glue::glue("model_{state}_{rf_fit$spec$engine}.RDS"))

## see if test and original are the same
model_12
test_mod

## light gbm
bt_spec <- 
  boost_tree(trees = 1000, 
             # complexity
             tree_depth = tune(),
             min_n = tune(), 
             loss_reduction = tune(),
             # randomness
             sample_size = tune(), 
             mtry = tune(),
             # step size
             learn_rate = tune()) %>% 
  set_engine("xgboost") %>% 
  set_mode("regression")

bt_flow <- 
  workflow() |>
  add_recipe(rf_recipe) |>
  add_model(bt_spec)

tuning_grid <-
  grid_latin_hypercube(
    tree_depth(),
    min_n(),
    loss_reduction(),
    sample_size = sample_prop(),
    finalize(mtry(), train),
    learn_rate(),
    size = 100
  )

doParallel::registerDoParallel(cores = 6)
tictoc::tic()
tuning_bt <- 
  tune_grid(
    bt_flow,
    resamples = folds,
    grid = tuning_grid,
    metrics = perf_metrics,
    control = control_grid(save_pred = TRUE)
  )
tictoc::toc()

bt_best <- finalize_model(bt_spec, select_best(tuning_bt, "mae"))

bt_fit <- 
  bt_best %>%
  set_engine("xgboost") %>%
  fit(maxhd_macro ~ .,
      # this is the original data, so we fit on the training (no leakage)
      data = rf_proc
  )

plot_comparison(bt_fit, rf_in, rf_out, state, name, 10)
plot_comparison(rf_fit, rf_in, rf_out, state, name, 10)

## diagnostics
dag <- 
  lasso_proc |> 
  dplyr::select(c("maxhd_macro", all_of(relevant_features))) |>
  tidyr::drop_na() |>
  data.table::as.data.table() |> 
  sparsebnUtils::sparsebnData(type = 'continuous') |> 
  sparsebn::estimate.dag() 

solution <- sparsebnUtils::select(dag, edges = 100)

plot(solution,
     layout = igraph::layout.circle, 
     # layout = igraph::layout_(sparsebnUtils::to_igraph(solution$edges), igraph::in_circle()),
     vertex.size = 5,
     vertex.label.color = c('#000000', rep('#7c7c7c', length(igraph::V(sparsebnUtils::to_igraph(solution$edges))) - 1)),
     vertex.color = gray(0.9),
     edge.color = gray(0),
     edge.arrow.size = 0.45
)
