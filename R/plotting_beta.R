plot_lasso_hyperparameters <- function(tuning, metric, state){
  
  plot <- 
    tuning %>% 
    collect_metrics() %>%
    filter(.metric == metric) %>%
    ggplot(aes(x = penalty, y = mixture, colour = mean)) +
    geom_point(size = 2) +
    scale_colour_gradientn(colours = RColorBrewer::brewer.pal(n = 9, name = 'YlOrRd'),
                           name = 'error') +
    theme_minimal() +
    theme(plot.title = element_text(face = 'bold'),
          plot.background = element_rect(fill = '#ffffff'),
          legend.position = c(0.2, 0.8))
  
  ggsave(plot, filename = glue::glue("lasso_tuning_{state}.png"), height = 6, width = 8, dpi = 300)
  
  return(plot)
  
}

plot_lasso_features <- function(tuning, lasso_recipe, lasso_specification, training, testing, state){
  
  plot <- 
    bind_rows(workflow() %>%
                add_recipe(lasso_recipe) %>% 
                add_model(finalize_model(lasso_specification, 
                                         select_best(tuning))) %>%
                fit(data = testing) %>%
                extract_fit_parsnip() %>%
                tidy() %>%
                mutate(dataset = "train"),
              workflow() %>%
                add_recipe(lasso_recipe) %>% 
                add_model(finalize_model(lasso_specification, 
                                         select_best(tuning))) %>%
                fit(data = training) %>%
                extract_fit_parsnip() %>%
                tidy() %>%
                mutate(dataset = "test")) %>% 
    group_by(term) %>% 
    mutate(total = sum(abs(estimate))) %>%
    filter(total != 0,
           term != "(Intercept)") %>% 
    mutate(term = str_replace_all(term, "_", " "),
           term = str_remove_all(term, "3")) %>% 
    ggplot(aes(reorder(term, estimate), estimate, fill = dataset)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_brewer(type = 'qual') + 
    labs(x = "", y = "") + 
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          plot.title = element_text(face = 'bold'),
          plot.background = element_rect(fill = '#ffffff'),
          legend.position = c(0.2, 0.8))
  
  ggsave(plot, filename = glue::glue("lasso_features_{state}.png"), height = 6, width = 8, dpi = 300)
  
  return(plot)
  
}

get_lasso_features <- function(tuning, lasso_recipe, lasso_specification, training, testing){
  
  features <- 
    bind_rows(workflow() |>
                add_recipe(lasso_recipe) |> 
                add_model(finalize_model(lasso_specification, 
                                         select_best(tuning))) |>
                fit(data = testing) |>
                extract_fit_parsnip() |>
                tidy() |>
                mutate(dataset = "train"),
              workflow() |>
                add_recipe(lasso_recipe) |> 
                add_model(finalize_model(lasso_specification, 
                                         select_best(tuning))) |>
                fit(data = training) |>
                extract_fit_parsnip() |>
                tidy() |>
                mutate(dataset = "test")) |>
    group_by(term) |> 
    mutate(total = sum(abs(estimate))) |>
    filter(total != 0,
           term != "(Intercept)") |>
    select(-total) |>
    pull(term) |>
    unique()
  
  return(features)  
  
}

plot_rf_hyperparameters <- function(tuning, metric, state = state){
  
  plot <- 
    tuning %>% 
    collect_metrics() %>%
    filter(.metric == metric) %>%
    ggplot(aes(x = mtry, y = min_n, colour = mean)) +
    geom_point(size = 2) +
    scale_colour_gradientn(colours = RColorBrewer::brewer.pal(n = 9, name = 'YlOrRd'),
                           name = 'error') +
    theme_minimal()
  
  ggsave(plot, filename = glue::glue("rf_tuning_{state}.png"), height = 6, width = 8, dpi = 300)
  
  return(plot)
  
}

shop <- function(dataset, drop = drop){
  
  steps <- 
    recipe(maxhd_macro ~ ., 
           data = select(dataset, -geometry)) |>
    step_rm(any_of(drop)) |> 
    step_string2factor(matches("cut[0-9][0-9]to19$")) |>
    step_scale(all_numeric_predictors()) |>
    step_center(all_numeric_predictors()) |>
    step_rm(ends_with("19_lag3")) |>
    step_dummy(all_nominal()) |> 
    step_log(all_outcomes(), offset = 1)
  
  return(steps)
  
}

plot_comparison <- function(fit, in_sample, out_sample, state = state, name = name, quantiles = 10){
  
  ## fit (model): a model stored after last fit
  ## in_sample (df): training data 
  ## out_sample (df): testing data
  ## state (chr): the state fips code
  ## name (chr): the state name
  ## quantiles (int): the number of quantiles
  
  if(quantiles > 10) { text_size = 0.0000001 } else { text_size = 50 / quantiles}
  
  plot <- 
    bind_rows(predict(fit, in_sample) |>
                bind_cols(in_sample) |>
                transmute(predicted = exp(.pred), observed = exp(maxhd_macro), sample = "in") |>
                mutate(error = abs(observed - predicted), 
                       group = ntile(observed, quantiles)) |>
                group_by(group = as.character(group), sample) |>
                summarise(predicted = mean(predicted),
                          observed = mean(observed),
                          error = mean(error)), 
              predict(fit, out_sample) |>
                bind_cols(out_sample) |>
                transmute(predicted = exp(.pred), observed = exp(maxhd_macro), sample = "out") |>
                mutate(error = abs(observed - predicted), 
                       group = ntile(observed, quantiles) + 0.25) |>
                group_by(group = as.character(group), sample) |>
                summarise(predicted = mean(predicted),
                          observed = mean(observed),
                          error = mean(error))) |>
    mutate(group = parse_number(group),
           position = case_when(predicted > observed ~ predicted,
                                TRUE ~ observed)) |>
    ggplot() +
    geom_segment(aes(x = group, xend = group, y = predicted, yend = observed, colour = sample), 
                 size = 50 / quantiles) + 
    geom_text(aes(x = group, y = position + 3, label = round(error, 1), colour = sample), angle = 90, size = text_size) + 
    scale_colour_brewer(palette = 'Set1', name = "Sample...") +
    scale_x_discrete(breaks = c(), labels = rep("", quantiles)) +
    labs(title = glue::glue("Comparing Predicted and Obversed in {name}"),
         subtitle = "Allowable Housing Density", 
         x = "< less dense | more dense >", y = "housing density") +
    theme_minimal() +
    theme(plot.title = element_text(face = 'bold'),
          plot.background = element_rect(fill = '#ffffff'),
          legend.position = c(0.2, 0.8))
  
  ggsave(plot = plot, filename = glue::glue("comparison_{fit$spec$engine}_{state}_{quantiles}.png"), height = 6, width = 8, dpi = 300)
  
  return(plot)
  
}
