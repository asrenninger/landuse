###############################
## modelling
###############################

load("model.RData")

## packages
library(tidyverse)
library(tidymodels)
library(tigris)
library(sf)

## baseline
mod_linear <- 
  lm(log(maxhd_macro) ~ .,
     data = 
       regression %>% 
       left_join(infill) %>%
       select(-GEOID, -maxhd_micro, -geometry) %>% 
       filter(!is.infinite(log(maxhd_macro))))

summary(mod_linear)

## with or without lags
regression_lags <- left_join(select(regression, -maxhd_micro), select(infill, -geometry))
regression_lags <- select(regression_lags, GEOID, maxhd_macro, any_of(useful_terms), built_change)

## preparing the data
set.seed(43)

# hold out one county for testing
holdout <- 
  regression_lags %>% 
  pull(GEOID) %>% 
  str_sub(1, 5) %>%
  unique() %>%
  sample(1)

blocks %>% 
  transmute(holdout = factor(if_else(str_sub(GEOID, 1, 5) == holdout, 1, 0))) %>% 
  plot()

crosswalk <-
  regression %>% 
  filter(str_sub(GEOID, 1, 5) != holdout) %>%
  drop_na() %>% 
  transmute(GEOID, 
            maxhd_macro = log(maxhd_macro)) %>%
  filter(!is.infinite(maxhd_macro)) %>% 
  left_join(blocks) %>% 
  select(-area_total) %>%
  st_as_sf()

plot(crosswalk)

set.seed(42)
split <- 
  regression_lags %>% 
  filter(str_sub(GEOID, 1, 5) != holdout) %>%
  drop_na() %>%
  select(-GEOID) %>%
  # log transform the dependent variable
  mutate(maxhd_macro = log(maxhd_macro)) %>%
  filter(!is.infinite(maxhd_macro)) %>% 
  # go with a 70/30 split since we have a limited sample, try  50/50 as well to avoid overfitting
  initial_split(0.7)

# create separate datasets
train <- training(split)
test <- testing(split)

bind_rows(mutate(train, split = "train"),
          mutate(test, split = "test")) %>%
  ggplot(aes(exp(maxhd_macro), fill = split)) +
  geom_histogram() +
  scale_x_log10() +
  facet_wrap(~ split) +
  theme_minimal()

# rescale continous variables
continuous <- c("isoperi_mean", "building_size", "intersection_density", "prop_4way", "orientation_order", "ndvi_mean", "built_intensity", "dist_towns", "dist_city", "pct_nonwhite", "density", "household_size", "median_income")
lags <- str_c(continuous, "_lag3")
dummy <- c("built_change")

prediction_recipe <- 
  recipe(maxhd_macro ~ ., 
         data = train) %>%
  # add_role(GEOID, new_role = "ID") %>%
  step_scale(all_of(c(continuous, lags))) %>%
  step_dummy(all_of(dummy))

prediction_prep <- prep(prediction_recipe)
prediction_juice <- juice(prediction_prep)

## LASSO real quick to figure out the features
lasso_spec <-
  linear_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet")

workflow() %>%
  add_recipe(prediction_recipe) %>%
  add_model(lasso_spec) %>%
  tune_grid(resample = vfold_cv(train),
            grid = grid_regular(penalty(range = c(0, 0.1), trans = NULL),
                                levels = 10),
            metrics = metric_set(mae, rmse)) %>%
  collect_metrics() %>%
  ggplot(aes(x = penalty, y = mean)) +
  geom_point(size = 2) +
  facet_wrap(~.metric, scales = 'free') +
  theme_minimal()

lasso_spec <-
  linear_reg(penalty = 0.025, mixture = 1) %>%
  set_engine("glmnet")

useful_terms <-
  workflow() %>%
  add_recipe(prediction_recipe) %>%
  add_model(lasso_spec) %>%
  fit(data = train) %>%
  pull_workflow_fit() %>%
  tidy() %>%
  filter(estimate != 0) %>%
  slice(2:n()) %>%
  pull(term)

## filtering out the problematic variables
regression_lags <- select(regression_lags, GEOID, maxhd_macro, any_of(useful_terms), built_change)

## preparing the data
set.seed(43)

# hold out one county for testing
holdout <- 
  regression_lags %>% 
  pull(GEOID) %>% 
  str_sub(1, 5) %>%
  unique() %>%
  sample(1)

crosswalk <-
  regression %>% 
  filter(str_sub(GEOID, 1, 5) != holdout) %>%
  drop_na() %>% 
  transmute(GEOID, 
            maxhd_macro = log(maxhd_macro)) %>%
  filter(!is.infinite(maxhd_macro)) %>% 
  left_join(blocks) %>% 
  select(-area_total) %>%
  st_as_sf()

set.seed(42)
split <- 
  regression_lags %>% 
  filter(str_sub(GEOID, 1, 5) != holdout) %>%
  drop_na() %>%
  select(-GEOID) %>%
  # log transform the dependent variable
  mutate(maxhd_macro = log(maxhd_macro)) %>%
  filter(!is.infinite(maxhd_macro)) %>% 
  # go with a 70/30 split since we have a limited sample, try  50/50 as well to avoid overfitting
  initial_split(0.7)

# create separate datasets
train <- training(split)
test <- testing(split)

prediction_recipe <- 
  recipe(maxhd_macro ~ ., 
         data = train) %>%
  # add_role(GEOID, new_role = "ID") %>%
  step_scale(any_of(useful_terms)) %>%
  step_dummy(all_of(dummy))

prediction_prep <- prep(prediction_recipe)
prediction_juice <- juice(prediction_prep)

## model it
set.seed(42)

# random forest
mod_rf <-
  rand_forest(trees = 1000,
              mtry = tune(),
              min_n = tune()) %>%
  set_mode("regression") %>% 
  set_engine("ranger")

wflow_rf <- 
  workflow() %>% 
  add_recipe(prediction_recipe) %>%
  add_model(mod_rf) 

# this will take a while without parallel processing
doParallel::registerDoParallel(cores = 6)

# create a regular grid to search through
grid <- 
  grid_regular(
    mtry(range = c(2, 10)),
    min_n(range = c(2, 10)),
    levels = 5
  )

## tune on 10 folds
set.seed(42)

# default is 10 folds
folds <- vfold_cv(train)

# tune the next (six cores, 260.34 seconds)
tictoc::tic()
tuned_rf <- 
  tune_grid(
    wflow_rf,
    resamples = folds,
    grid = grid,
    metrics = metric_set(mae, rsq)
  )
tictoc::toc()

best_rf <- select_best(tuned_rf, "mae") 

final_rf <- 
  finalize_model(
    mod_rf,
    best_rf
  )

# plot the most important variables
final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(maxhd_macro ~ .,
      data = prediction_juice) %>%
  vip::vip(geom = "point") +
  theme_minimal()

ggsave(plot = last_plot(), filename = "vip_nj_lag_lasso.png", height = 6, width = 6, dpi = 300)

# fit rf
fflow_rf <- 
  workflow() %>%
  add_recipe(prediction_recipe) %>%
  add_model(final_rf)

results_rf <- 
  fflow_rf %>%
  last_fit(split)

results_rf %>%
  collect_predictions() %>%
  ggplot(aes(exp(maxhd_macro), exp(.pred), colour = exp(abs(maxhd_macro - .pred)))) + 
  geom_point(alpha = 0.5) +
  geom_abline(linetype = 2, alpha = 0.5) +
  scale_x_log10(breaks = c(0, 0.1, 1, 10, 100), limits = c(0.01, 100)) +
  scale_y_log10(breaks = c(0, 0.1, 1, 10, 100), limits = c(0.01, 100)) +
  scale_colour_gradientn(colours = RColorBrewer::brewer.pal(n = 9, name = 'YlOrRd'), 
                         name = 'error',
                         breaks = c(5, 10, 15), limits = c(0, 20), oob = scales::squish) +
  labs(title = "Random Forest Predictions",
       subtitle = "Errors within sample",
       x = "max allowable density",
       y = "prediction") + 
  theme_minimal()

ggsave(plot = last_plot(), filename = "rf_in_nj_lag_lass.png", height = 6, width = 8, dpi = 300)

# good for an an r-squared of 0.74
summary_rf <- 
  results_rf %>%
  collect_predictions() %>%
  mutate(error = abs(maxhd_macro - .pred)) %>%
  transmute(predicted = exp(.pred), observed = exp(maxhd_macro), error = exp(error))

results_rf %>%
  collect_predictions() %>%
  mutate(error = abs(maxhd_macro - .pred)) %>%
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
  labs(title = "Comparing Predictions",
       subtitle = "Quartiles of housing density", 
       x = "", "", y = "") +
  theme_minimal()

ggsave(plot = last_plot(), filename = "rf_in_quantiles_nj_lag_lasso.png", height = 6, width = 8, dpi = 300)

## out of sample
updated_data <- 
  regression_lags %>% 
  drop_na() %>%
  select(-GEOID) %>%
  # log transform the dependent variable
  mutate(maxhd_macro = log(maxhd_macro)) %>%
  filter(!is.infinite(maxhd_macro))

holdout_recipe <- 
  recipe(maxhd_macro ~ ., 
         data = updated_data) %>%
  step_scale(any_of(useful_terms)) %>%
  step_dummy(all_of(dummy))

holdout_prep <- prep(holdout_recipe)
holdout_juice <- juice(holdout_prep)

holdout_fit <- 
  final_rf %>%
  set_engine("ranger") %>%
  fit(maxhd_macro ~ .,
      # this is the original data
      data = prediction_juice
  )

## MAE of 2.58 on 419 block groups
regression_lags %>% 
  drop_na() %>%
  mutate(maxhd_macro = log(maxhd_macro)) %>%
  filter(!is.infinite(maxhd_macro)) %>%
  transmute(GEOID, 
            maxhd_macro,
            # this is the county we held out
            prediction = predict(holdout_fit, holdout_juice)$.pred) %>%
  filter(str_sub(GEOID, 1, 5) == holdout) %>%
  summarise(MAE = mean(maxhd_macro),
            n = n()) %>% 
  mutate(MAE = exp(MAE))

ggsave(
  ggplot(blocks %>%
         mutate(held = factor(if_else(str_sub(GEOID, 1, 5) == holdout, 0, 1)))) +
  geom_sf(aes(fill = held), size = 0, colour = NA, show.legend = FALSE) +
  scale_fill_brewer(palette = 'Set1') +
  theme_void(),
  filename = "map_nj.png", height = 6, width = 10, dpi = 300)

test_juice <- 
  recipe(maxhd_macro ~ ., 
         data = test) %>%
  step_scale(any_of(useful_terms)) %>%
  step_dummy(all_of(dummy)) %>%
  prep() %>% 
  juice()

# out of sample is way worse with a MAE of 71.9 across 547 block groups
predict(holdout_fit, test_juice) %>%
  mutate(maxhd_macro = test$maxhd_macro,
         error = exp(abs(maxhd_macro - .pred))) %>% 
  summarise(MAE = mean(error),
            n = n())

predict(holdout_fit, test_juice) %>%
  mutate(maxhd_macro = test$maxhd_macro,
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

ggsave(plot = last_plot(), filename = "rf_out_nj_lag_lasso.png", height = 6, width = 8, dpi = 300)

predict(holdout_fit, test_juice) %>%
  mutate(maxhd_macro = test$maxhd_macro,
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

ggsave(plot = last_plot(), filename = "rf_out_quantiles_nj_lag_lasso.png", height = 6, width = 8, dpi = 300)

## good for a mean of about 2.1 when we trim the outliers (which we need to figure out)
predict(holdout_fit, test_juice) %>%
  mutate(maxhd_macro = test$maxhd_macro,
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
  labs(title = "Comparing Predictions: NJ with NJ",
       subtitle = "Deciles of housing density", 
       x = "< less dense | more dense >", y = "housing density") +
  theme_minimal()

ggsave(plot = last_plot(), filename = "predict_md_with_md.png", height = 6, width = 10)

predict(holdout_fit, test_juice) %>%
  mutate(maxhd_macro = test$maxhd_macro,
         median_income_lag3 = test$median_income_lag3) %>%
  left_join(select(regression_lags, median_income_lag3, GEOID)) %>%
  transmute(GEOID,
            predicted_nj = exp(.pred),
            observed_nj = exp(maxhd_macro)) %>% 
  left_join(nj_md) %>%
  # transmute(predicted = ((predicted * 0.3) + (predicted_nj * 0.7)),
  #           observed = ((observed * 0.5) + (observed_nj * 0.5))) %>%
  mutate(error = abs(observed - predicted),
         group = factor(ntile(observed, 10))) %>%
  drop_na() %>%
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
  scale_y_continuous(breaks = c(0, 10, 20, 30)) +
  labs(title = "Comparing Predictions: NJ with MD",
       subtitle = "Deciles of housing density", 
       x = "< less dense | more dense >", y = "housing density") +
  theme_minimal()

ggsave(plot = last_plot(), filename = "predict_nj_with_nj+md_out_linear_lag_lasso.png", height = 6, width = 10)

ggsave(
  predict(holdout_fit, test_juice) %>%
    mutate(maxhd_macro = test$maxhd_macro,
           error = abs(maxhd_macro - .pred),
           density = test$density) %>% 
    left_join(select(regression, density, GEOID)) %>% 
    left_join(blocks) %>%
    st_as_sf() %>% 
    transmute(predicted = exp(.pred), observed = exp(maxhd_macro), error = exp(error), GEOID) %>% 
    ggplot() +
    geom_sf(aes(fill = error), size = 0, colour = NA) +
    scale_fill_gradientn(colours = RColorBrewer::brewer.pal(n = 9, name = 'YlOrRd'), 
                         breaks = c(5, 10, 15), limits = c(0, 20), oob = scales::squish) +
    theme_void(),
  filename = "map_out.png", height = 6, width = 10, dpi = 300)

ggsave(
  results_rf %>%
    collect_predictions()  %>%
    mutate(error = abs(maxhd_macro - .pred),
           density = train$density) %>%
    left_join(select(regression, density, GEOID)) %>% 
    left_join(blocks) %>%
    st_as_sf() %>%
    transmute(predicted = exp(.pred), observed = exp(maxhd_macro), error = exp(error), GEOID) %>% 
    ggplot() +
    geom_sf(aes(fill = error), size = 0, colour = NA) +
    scale_fill_gradientn(colours = RColorBrewer::brewer.pal(n = 9, name = 'YlOrRd'), 
                         breaks = c(5, 10, 15), limits = c(0, 20), oob = scales::squish) +
    theme_void(),
  filename = "map_in.png", height = 6, width = 10, dpi = 300)

results_out <- 
  predict(holdout_fit, test_juice) %>%
  mutate(maxhd_macro = test$maxhd_macro,
         median_income_lag3 = test$median_income_lag3) %>%
  left_join(select(regression_lags, median_income_lag3, GEOID)) %>%
  transmute(GEOID,
            predicted_nj = exp(.pred),
            observed_nj = exp(maxhd_macro)) %>% 
  left_join(nj_md) %>%
  # transmute(predicted = ((predicted * 0.3) + (predicted_nj * 0.7)),
  #           observed = ((observed * 0.5) + (observed_nj * 0.5))) %>%
  mutate(error = abs(observed - predicted),
         group = factor(ntile(observed, 10))) %>%
  drop_na() %>%
  pivot_longer(cols = c(observed, predicted)) %>%
  group_by(group, name) %>%
  summarise(value = mean(value)) %>%
  pivot_wider(id_cols = c(name, group))
