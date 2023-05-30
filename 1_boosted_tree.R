# Boosted Trees Tuning

# load packages
library(tidyverse)
library(tidymodels)
library(patchwork)
library(tictoc)

# handle common conflicts
tidymodels_prefer()

# set seed
set.seed(2468)

# load required objects
load("initial_setup/tuning_setup.rda")

# define model ----
bt_spec <-
  boost_tree(mtry = tune(),
             learn_rate = tune(), 
             min_n = tune()) %>%
  set_engine('xgboost') %>%
  set_mode('regression')

# setup tuning grid 
bt_params <- hardhat::extract_parameter_set_dials(bt_spec) %>% 
  update(
    mtry = mtry(range = c(1, 18)),
    learn_rate = learn_rate(range = c(-5, -0.2))
  )

# define grid
bt_grid <- grid_regular(bt_params, levels = 5)

## basic recipe----
# workflow
bt_wflow <-
  workflow() %>% 
  add_model(bt_spec) %>% 
  add_recipe(basic_recipe) 

# tuning/fitting 
tic.clearlog()
tic("Boosted Tree")

# tuning
bt_tune <-
  bt_wflow %>% 
  tune_grid(
    resamples = life_folds,
    grid = bt_grid,
    control = keep_pred,
    metrics = life_metrics
  )

toc(log = TRUE)

# save runtime info
time_log <- tic.log(format = FALSE)

bt_tictoc <- tibble::tibble(
  model = time_log[[1]]$msg,
  start_time = time_log[[1]]$tic,
  end_time = time_log[[1]]$toc,
  runtime = end_time - start_time
)

# write out results
save(bt_tune, bt_wflow, bt_tictoc, file = "results/bt_tune.rda")

## filter recipe ---

# workflow
bt_wflow <-
  workflow() %>% 
  add_model(bt_spec) %>% 
  add_recipe(filtered_recipe) 

# tuning/fitting 
tic.clearlog()
tic("Boosted Tree - Filtered")

# tuning
bt_tune <-
  bt_wflow %>% 
  tune_grid(
    resamples = life_folds,
    grid = bt_grid,
    control = keep_pred,
    metrics = life_metrics
  )

toc(log = TRUE)

# save runtime info
time_log <- tic.log(format = FALSE)

bt_tictoc <- tibble::tibble(
  model = time_log[[1]]$msg,
  start_time = time_log[[1]]$tic,
  end_time = time_log[[1]]$toc,
  runtime = end_time - start_time
)

# write out results
save(bt_tune, bt_wflow, bt_tictoc, file = "results/bt_tune_filter.rda")
