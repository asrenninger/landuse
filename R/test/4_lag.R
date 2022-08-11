## NLURI MODEL ----

#load("model.RData")
## What is being called?
## bring in useful terms before called?
## what if it calls in the wrong state?

# 0. Inputs ----

## packages
library(tidyverse)
library(tidymodels)
library(tigris)
library(sf)
library(glue)

STATE_STR = 'nj'
STATE_CAP = toupper(STATE_STR)

outcome.var <- "maxhd_macro"

# continous variables
continuous.var <-
  c(
    "isoperi_mean", "building_size", "intersection_density",
    "prop_4way", "orientation_order", "ndvi_mean",
    "built_intensity", "dist_towns", "dist_city",
    "pct_nonwhite", "density", "household_size
    median_income"
    )
lag.var <- str_c(continuous.var, "_lag3")
dummy.var <- c("built_change")
admin.var <- c("geometry", "GEOID")

all.var <- c(outcome.var, continuous.var, lag.var, dummy.var, admin.var)

## with or without lags
load("R/regression_nodense.rda")
reg.data <-
  reg.data %>%
  select(any_of(all.var))

infinites =
  reg.data %>%
  # remove GEOID, & geometry
  select(-any_of(admin.var)) %>%
  # turn outcome into log
  mutate({{outcome.var}} := log(.data[[outcome.var]])) %>%
  filter_all(all_vars(!is.infinite(.))) %>%
  nrow()
diff = nrow(reg.data) - infinites
glue("{diff} ({round(100*diff/nrow(reg.data), 1)}%)") %>%
  paste0("Number of Block Groups with an Infinite Outcome: ", .) %>%
  print()


# 1. LINEAR MODEL ----

form = function(outcome_str = outcome.var)
  glue("{outcome_str} ~ .") %>% as.formula()
model_formula = form()

## baseline
model.linear <-
  lm(
    model_formula,
    data =
      reg.data %>%
        # remove GEOID, & geometry
        select(-any_of(admin.var)) %>%
        # turn outcome into log
        mutate({{outcome.var}} := log(.data[[outcome.var]])) %>%
        filter_all(all_vars(!is.infinite(.))) %>%
        mutate_at(dummy.var, as.factor)
    )

model.linear %>%
  summary()


# 2. Train/Test Split 1 ----

## preparing the data
set.seed(43)

# hold out one county for testing
holdout <-
  reg.data %>%
    pull(GEOID) %>%
    str_sub(1, 5) %>%
    unique() %>%
    sample(1)

blocks.data %>%
  transmute(holdout = factor(if_else(str_sub(GEOID, 1, 5) == holdout, 1, 0))) %>%
  plot()

#crosswalk <-
  reg.data %>%
    mutate({{outcome.var}} := log(.data[[outcome.var]])) %>%
    filter_all(all_vars(!is.infinite(.))) %>%
    filter(str_sub(GEOID, 1, 5) != holdout) %>%
    drop_na() %>%
  left_join(blocks.data) %>%
  #select(-any_of(admin.var)) %>%
  select(-area_total, -GEOID) %>%
  st_as_sf() %>%
  plot()
#plot(crosswalk)

set.seed(42)


split.data <-
  reg.data %>%
  filter(str_sub(GEOID, 1, 5) != holdout) %>%
  drop_na() %>%
  select(-GEOID) %>%
  # dummy as factor
  mutate_at(dummy.var, as.factor) %>%
  # log transform the dependent variable
  mutate({{outcome.var}} := log(.data[[outcome.var]])) %>%
  filter_all(all_vars(!is.infinite(.))) %>%
  # go with a 70/30 split since we have a limited sample, try  50/50 as well to avoid overfitting
  initial_split(0.7)

# create separate datasets
train.data <- training(split.data)
test.data <- testing(split.data)

bind_rows(mutate(train.data, split = "train"),
          mutate(test.data, split = "test")) %>%
  ggplot(aes(exp(maxhd_macro), fill = split)) +
  geom_histogram() +
  scale_x_log10() +
  #facet_wrap(~ split) +
  theme_minimal()



# 3. Lasso (Penalty & Features) ----

lasso1.recipe <-
  recipe(
    data = train.data,
    formula = model_formula) %>%
  # normalize numeric data to sd of 1
  step_scale(any_of(c(continuous.var, lag.var))) %>%
  # nominal to binary
  step_dummy(all_of(dummy.var))

lasso1.prep <- prep(lasso1.recipe)
lasso1.juice <- juice(lasso1.prep)

## LASSO to determine best penalty
## penalty aka L1-norm, the sum of the absolute coefficients
lasso1.model <-
  linear_reg(
    # tune the model to multiple penalty options
    penalty = tune(),
    # set as lasso
    mixture = 1
    ) %>%
  set_engine("glmnet")

## FORMATTING FUNCTIONS FOR PLOT
trail_zero = function(x) sub("0+$", "", x)
lead_zero = function(x) ifelse(as.numeric(x)==0,"0", sub("^0+", "", x))
no_zero <- function(x) {
  ifelse(as.numeric(x)==0,"0",
         lead_zero(x) %>%
           trail_zero(.))
}
cnum = function(x, digits=3) x %>% round(digits) %>% no_zero(.)

## Tune grid & Cross-Validation to find MAE & RMSE
lasso1.tuned =
    workflow() %>%
    add_recipe(lasso1.recipe) %>%
    add_model(lasso1.model) %>%
    tune_grid(
      resample = vfold_cv(train.data),
      grid = grid_regular(
        penalty(range = c(0, .1), trans = NULL),
        levels = 20),
      metrics = metric_set(mae, rmse, rsq)
      )

lasso1.penalty <-
  lasso1.tuned %>%
    select_best(metric="mae") %>%
    pull(penalty)
# lasso1.penalty <- 0.025

lasso1.penalty %>%
  round(., 3) %>%
  paste0("Penalty of ", .)

lasso1.tuned %>%
  collect_metrics() %>%
  ggplot(aes(x = penalty, y = mean)) +
  geom_point(size = 2) +
  geom_line() +
  geom_vline(
    xintercept = lasso1.penalty,
    color='red') +
  scale_x_continuous(labels = no_zero) +
  facet_wrap(~.metric, scales = 'free') +
  labs(
    title = glue("{STATE_CAP} Lasso: Penalty Selection"),
    subtitle = glue("Grid Search, Optimal Penalty of {lasso1.penalty %>%
  round(., 3)}")) +
  theme_minimal()

## LASSO to actually pull terms
## .025 penalty as it is generally the lowest error between MAE & RMSE
lasso2.model <-
  linear_reg(
    penalty = lasso1.penalty,
    mixture = 1) %>%
  set_engine("glmnet")

useful_terms <-
  workflow() %>%
  add_recipe(lasso1.recipe) %>%
  add_model(lasso2.model) %>%
  fit(data = train.data) %>%
  pull_workflow_fit() %>%
  tidy() %>%
  filter(estimate != 0) %>%
  slice(2:n()) %>%
  pull(term) %>%
  # for some reason, it adds "X1" to built hange
  gsub('_X1', '', .)

useful_terms.count =
  glue("{length(useful_terms)} of {nrow(lasso1.recipe$var_info)} variables used")
useful_terms.count %>%
  print()


## filtering out the problematic variables
# regression_lags <-
#   regression_lags %>%
#   select(
#     GEOID, maxhd_macro,
#     any_of(useful_terms), built_change)

# 4. Train/Test Split 2 ----

## preparing the data
set.seed(43)

# hold out one county for testing
holdout <-
  reg.data %>%
  pull(GEOID) %>%
  str_sub(1, 5) %>%
  unique() %>%
  sample(1)

set.seed(42)

split.data <-
  reg.data %>%
  filter(str_sub(GEOID, 1, 5) != holdout) %>%
  drop_na() %>%
  # dummy as factor
  mutate_at(dummy.var, as.factor) %>%
  select(outcome.var, useful_terms) %>%
  # log transform the dependent variable
  mutate({{outcome.var}} := log(.data[[outcome.var]])) %>%
  filter_all(all_vars(!is.infinite(.))) %>%
  # go with a 70/30 split since we have a limited sample, try  50/50 as well to avoid overfitting
  initial_split(0.7)

# create separate datasets
train.data <- training(split.data)
test.data <- testing(split.data)



# 5. Random Forest: Fit ----

set.seed(42)

rf.recipe <-
  recipe(
    data = train.data,
    formula = model_formula
    ) %>%
  # normalize numeric data to sd of 1
  step_scale(all_numeric()) %>%
  # nominal to binary
  step_dummy(!!all_of(dummy.var))

rf.prep <- prep(rf.recipe)
rf.juice <- juice(rf.prep)

rf.model <-
  rand_forest(trees = 1000,
              mtry = tune(),
              min_n = tune()) %>%
  set_mode("regression") %>%
  set_engine("ranger")

rf.workflow <-
  workflow() %>%
  add_recipe(rf.recipe) %>%
  add_model(rf.model)

doParallel::registerDoParallel(cores = 6)

# create a regular grid to search through
rf.grid <-
  grid_regular(
    mtry(range = c(2, 10)),
    min_n(range = c(2, 10)),
    levels = 5
  )

rf.folds <-
  vfold_cv(
    train.data,
    v = 10
    )

# tune the next (six cores, ~4 mins)
tictoc::tic()
rf.tuned <-
  tune_grid(
    rf.workflow,
    resamples = rf.folds,
    grid = rf.grid,
    metrics = metric_set(mae, rsq)
  )
tictoc::toc()

rf.best <- select_best(rf.tuned, "mae")

rf.tuned %>%
  collect_metrics() %>%
  #filter(.metric == 'mae') %>%
  ggplot(aes(x = mtry, y = mean)) +
  geom_point(aes(size = min_n)) +
  geom_hline(
    aes(yintercept = mean),color='red',
    data =.%>% filter(mtry == rf.best$mtry, min_n == rf.best$min_n)) +
  scale_x_continuous(labels = no_zero) +
  facet_wrap(~.metric, scales = 'free') +
  xlab("Num of Predictors Randomly Sampled as Candidates") +
  labs(
    title = "Random Forest Parameter Selection",
    subtitle = glue("Grid Search, mtry={rf.best$mtry}, min_n={rf.best$min_n}"),
    size="Minimum Data Points\nper Node (min_n)") +
  theme_minimal()

rf.final <-
  finalize_model(
    rf.model,
    rf.best
  )

######PLOT VARIABLES

rf.fit =
  rf.final %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(
    formula = model_formula,
    data = rf.juice
  )

rf.fit %>%
  vip::vip(
    geom = "point",
    num_features = rf.fit$fit$num.independent.variables
  ) +
  labs(
    title = glue("{STATE_CAP} Random Forest: Variables Importance"),
    subtitle =
      paste(
        glue('mtry={cnum(rf.fit$fit$mtry)},  min_n={cnum(rf.fit$fit$min.node.size)}'),
        glue('MSE={cnum(rf.fit$fit$prediction.error)},  R^2={cnum(rf.fit$fit$r.squared)}'),
        sep=',  '
      )
  ) +
  theme_minimal()
filename = glue("vip_{STATE_STR}_lag_lasso.png")
ggsave(plot = last_plot(), filename = filename, height = 6, width = 6, dpi = 300)

########FIT

# FIT rf
rf.results <-
  workflow() %>%
  add_recipe(rf.recipe) %>%
  add_model(rf.final) %>%
  last_fit(split.data)

# good for an an r-squared of 0.74
rf.predictions <-
  rf.results %>%
  collect_predictions() %>%
  mutate(
    error = abs(exp(.data[[outcome.var]]) - exp(.pred))
  ) %>%
  transmute(
    predicted = exp(.pred),
    observed = exp(.data[[outcome.var]]),
    error = error #exp(error)
  )

# 6. Random Forest: Predict ----

########PLOT

#ERRORS
log_breaks = c(0, 0.1, 1, 10, 100)
log_limits = c(0.01, 100)

rf.predictions %>%
  ggplot(aes(observed, predicted, colour = error)) +
  geom_point(alpha = 0.5) +
  geom_abline(linetype = 2, alpha = 0.5) +
  #scale_x_log10(breaks = log_breaks, limits = log_limits, labels=lead_zero) +
  #scale_y_log10(breaks = log_breaks, limits = log_limits, labels=lead_zero) +
  scale_colour_gradientn(colours = RColorBrewer::brewer.pal(n = 9, name = 'YlOrRd'),
                         name = 'Error',
                         breaks = c(5, 10, 15), limits = c(0, 20),
                         oob = scales::squish) +
  labs(title = glue("{STATE_CAP} Random Forest: Prediction Errors"),
       subtitle = "Max Allowable Housing Units per Acre",
       x = "Observed Density", # (log10)
       y = "Predicted Density") +
  theme_minimal()

filename = glue("rf_in_{STATE_STR}_lag_lass.png")
ggsave(plot = last_plot(), filename = filename, height = 6, width = 8, dpi = 300)


###
rf.predictions %>%
  mutate(group = ntile(observed, 4)) %>%
  pivot_longer(cols = c(observed, predicted)) %>%
  group_by(group, name) %>%
  summarise(value = mean(value)) %>%
  mutate(error = abs(value - lag(value))) %>%
  ungroup() %>%
  fill(error, .direction = "up") %>%
  ggplot(aes(x = factor(group), y = value, colour = name, style = name)) +
    geom_point() +
    scale_colour_brewer(palette = 'Set1', name = "") +
    scale_x_discrete(labels = c("Rural", "Exurban", "Suburban", "Urban")) +
    scale_y_log10() +
    labs(title = glue("{STATE_CAP} Random Forest: Comparing Predictions"),
         subtitle = "Quartiles of Housing Density",
         y = "Allowable Housing Units per Acre", x = "") +
    theme_minimal()



filename = glue("rf_in_quantiles_{STATE_STR}_lag_lasso.png")
ggsave(plot = last_plot(), filename = filename, height = 6, width = 8, dpi = 300)


# 7. Random Forest: Holdout ----


## out of sample
updated.data <-
  reg.data %>%
  drop_na() %>%
  # dummy as factor
  mutate_at(dummy.var, as.factor) %>%
  # log transform the dependent variable
  mutate({{outcome.var}} := log(.data[[outcome.var]])) %>%
  filter_all(all_vars(!is.infinite(.)))

all.recipe <-
  recipe(formula = model_formula,
         data = updated.data%>%
           select(outcome.var, useful_terms)) %>%
  step_scale(all_numeric()) %>%
  step_dummy(all_of(dummy.var))

all.prep <- prep(all.recipe)
all.juice <- juice(all.prep)

all.fit <-
  rf.final %>%
  set_engine("ranger") %>%
  fit(formula = model_formula,
      # this is the original data
      data = all.juice
  )

## MAE of 2.58 on 419 block groups

updated.data$prediction = predict(all.fit, all.juice)$.pred

updated.data %>%
  filter(str_sub(GEOID, 1, 5) == holdout) %>%
  summarise(MAE = mean(maxhd_macro),
            n = n()) %>%
  mutate(MAE = exp(MAE))

ggsave(
  ggplot(blocks.data %>%
           mutate(held = factor(if_else(str_sub(GEOID, 1, 5) == holdout, 0, 1)))) +
    geom_sf(aes(fill = held), size = 0, colour = NA, show.legend = FALSE) +
    scale_fill_brewer(palette = 'Set1') +
    theme_void(),
  filename = glue("map_{STATE_STR}.png"), height = 6, width = 10, dpi = 300)

# 8. Random Forest: Test ----

test.juice <-
  recipe(model_formula,
         data = test.data %>%
           select(useful_terms, outcome.var)) %>%
  step_scale(all_numeric()) %>%
  step_dummy(all_of(dummy.var)) %>%
  prep() %>%
  juice()

# out of sample is way worse with a MAE of 71.9 across 547 block groups
predict(all.fit, test.juice) %>%
  mutate(outcome = .data[[outcome.var]],
         error = exp(abs(maxhd_macro - .pred))) %>%
  summarise(MAE = mean(error),
            n = n())

predict(holdout_fit, test_juice) %>%
  mutate(maxhd_macro = test[[outcome.var]],
         error = abs(maxhd_macro - .pred)) %>%
  mutate(error = exp(abs(maxhd_macro - .pred))) %>%
  ggplot(aes(exp(maxhd_macro), exp(.pred), colour = error)) +
  geom_point(alpha = 0.5) +
  geom_abline(linetype = 2, alpha = 0.5) +
  scale_x_log10(breaks = c(0, 0.1, 1, 10, 100), limits = c(0.01, 100)) +
  scale_y_log10(breaks = c(0, 0.1, 1, 10, 100), limits = c(0.01, 100)) +
  scale_colour_gradientn(colours = RColorBrewer::brewer.pal(n = 9, name = 'YlOrRd'),
                         breaks = c(5, 10, 15), limits = c(0, 20), oob = scales::squish) +
  labs(title = "Random Forest Predictions",
       subtitle = "Errors outside sample",
       x = "max allowable density",
       y = "prediction") +
  theme_minimal()

filename = glue("rf_out_{STATE_STR}_lag_lasso.png")
ggsave(plot = last_plot(), filename = filename, height = 6, width = 8, dpi = 300)

predict(holdout_fit, test_juice) %>%
  mutate(maxhd_macro = test[[outcome.var]],
         error = abs(maxhd_macro - .pred)) %>%
  transmute(predicted = exp(.pred), observed = exp(maxhd_macro), error = exp(error)) %>%
  mutate(group = ntile(observed, 4)) %>%
  pivot_longer(cols = c(observed, predicted)) %>%
  group_by(group, name) %>%
  summarise(value = mean(value)) %>%
  mutate(error = abs(value - lag(value))) %>%
  ungroup() %>%
  fill(error, .direction = "up") %>%
  ggplot(aes(x = factor(group), y = value, colour = name, style = name)) +
  geom_point() +
  scale_colour_brewer(palette = 'Set1', name = "Housing Density...") +
  scale_x_discrete(labels = c("rural", "exurban", "suburban", "urban")) +
  scale_y_log10() +
  labs(title = "Comparing Predictions (out of sample)",
       subtitle = "Quartiles of housing density",
       x = "", "", y = "") +
  theme_minimal()

filename = glue("rf_out_quantiles_{STATE_STR}_lag_lasso.png")
ggsave(plot = last_plot(), filename = filename, height = 6, width = 8, dpi = 300)


plot_title = glue("Comparing Predictions: {toupper(STATE_STR)} with {toupper(STATE_STR)}")
## good for a mean of about 2.1 when we trim the outliers (which we need to figure out)
predict(holdout_fit, test_juice) %>%
  mutate(maxhd_macro = test[[outcome.var]],
         median_income_lag3 = test$median_income_lag3) %>%
  left_join(select(regression_lags, median_income_lag3, GEOID)) %>%
  transmute(predicted = exp(.pred), observed = exp(maxhd_macro)) %>%
  mutate(error = abs(observed - predicted),
         group = factor(ntile(observed, 10))) %>%
  pivot_longer(cols = c(observed, predicted)) %>%
  group_by(group, name) %>%
  summarise(value = mean(value)) %>%
  mutate(error = abs(value - lag(value))) %>%
  ungroup() %>%
  fill(error, .direction = "up") %>%
  ggplot(aes(x = factor(group), y = value, colour = name, style = name)) +
  geom_point() +
  scale_colour_brewer(palette = 'Set1', name = "Housing Density...") +
  # scale_x_discrete(labels = c("rural", "exurban", "suburban", "urban")) +
  # scale_y_log10() +
  labs(title = plot_title,
       subtitle = "Deciles of housing density",
       x = "< less dense | more dense >", y = "housing density") +
  theme_minimal()


filename = glue("predict_{STATE_STR}_with_{STATE_STR}.png")
ggsave(plot = last_plot(), filename = filename, height = 6, width = 10)