# Elastic Net Tuning

#load packages
library(tidyverse)
library(tidymodels)
library(patchwork)
library(tictoc)

#handle common conflicts
tidymodels_prefer()

# set seed
set.seed(2468)

# load required objects
load("initial_setup/tuning_setup.rda")

# define model ----
en_spec <-
  linear_reg(penalty = tune(), mixture = tune()) %>% 
  set_engine("glmnet") %>% 
  set_mode("regression")

en_params <- hardhat::extract_parameter_set_dials(en_spec)

# define grid
en_grid <- grid_regular(en_params, levels = 5)

## basic recipe----
# workflow
en_wflow <-
  workflow() %>% 
  add_model(en_spec) %>% 
  add_recipe(basic_recipe)

# tuning/fitting
tic.clearlog()
tic("Elastic Net")

en_tune <-
  en_wflow %>% 
  tune_grid(
    resamples = life_folds,
    grid = en_grid,
    control = keep_pred,
    metrics = life_metrics
  )

toc(log = TRUE)

# save runtime info
time_log <- tic.log(format = FALSE)

en_tictoc <- tibble::tibble(
  model = time_log[[1]]$msg,
  start_time = time_log[[1]]$tic,
  end_time = time_log[[1]]$toc,
  runtime = end_time - start_time
)

save(en_tune, en_wflow, en_tictoc, file = "results/en_tune.rda")

## filtered recipe----
# workflow
en_wflow <-
  workflow() %>% 
  add_model(en_spec) %>% 
  add_recipe(filtered_recipe)

# tuning/fitting
tic.clearlog()
tic("Elastic Net - Filtered")

en_tune <-
  en_wflow %>% 
  tune_grid(
    resamples = life_folds,
    grid = en_grid,
    control = keep_pred,
    metrics = life_metrics
  )

toc(log = TRUE)

# save runtime info
time_log <- tic.log(format = FALSE)

en_tictoc <- tibble::tibble(
  model = time_log[[1]]$msg,
  start_time = time_log[[1]]$tic,
  end_time = time_log[[1]]$toc,
  runtime = end_time - start_time
)

save(en_tune, en_wflow, en_tictoc, file = "results/en_tune_filter.rda")