# Random Forest Tuning

# load packages
library(tidyverse)
library(tidymodels)
library(patchwork)
library(tictoc)
library(doMC)

# handle common conflicts
tidymodels_prefer()

# set seed
set.seed(4444)

# set up parallel processing 
registerDoMC(cores = 8) 

# load required objects
load("initial_setup/tuning_setup.rda")

# define model ----
rf_spec <- 
  rand_forest(mtry = tune(),
              min_n = tune()) %>%
  set_engine("ranger") %>% 
  set_mode("regression")

rf_params <- hardhat::extract_parameter_set_dials(rf_spec) %>% 
  update(
    mtry = mtry(range = c(1, 25))
  )

# define grid
rf_grid <- grid_regular(rf_params, levels = 5)

## basic recipe----
# workflow
rf_wflow <-
  workflow() %>% 
  add_model(rf_spec) %>% 
  add_recipe(basic_recipe)

# tuning
tic.clearlog()
tic("Random Forest")

rf_tune <-
  rf_wflow %>% 
  tune_grid(
    resamples = life_folds,
    grid = rf_grid,
    control = keep_pred,
    metrics = life_metrics
  )

# pace tuning code in hear
toc(log = TRUE)

# save runtime info
time_log <- tic.log(format = FALSE)

rf_tictoc <- tibble(
  model = time_log[[1]]$msg,
  start_time = time_log[[1]]$tic,
  end_time = time_log[[1]]$toc,
  runtime = end_time - start_time
)

# write out results
save(rf_tune, rf_wflow, rf_tictoc, file = "results/rf_tune.rda")

## filtered recipe----
# workflow
rf_wflow <-
  workflow() %>% 
  add_model(rf_spec) %>% 
  add_recipe(filtered_recipe)

# tuning
tic.clearlog()
tic("Random Forest - Filtered")

rf_tune <-
  rf_wflow %>% 
  tune_grid(
    resamples = life_folds,
    grid = rf_grid,
    control = keep_pred,
    metrics = life_metrics
  )

toc(log = TRUE)

# save runtime info
time_log <- tic.log(format = FALSE)

rf_tictoc <- tibble(
  model = time_log[[1]]$msg,
  start_time = time_log[[1]]$tic,
  end_time = time_log[[1]]$toc,
  runtime = end_time - start_time
)

# write out results
save(rf_tune, rf_wflow, rf_tictoc, file = "results/rf_tune_filter.rda")
