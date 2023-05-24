# Neural Network Tuning

# load package(s) ----
library(tidymodels)
library(tidyverse)
library(tictoc)
library(doMC)

# handle common conflicts 
tidymodels_prefer()

# load required objects
load("initial_setup/tuning_setup.rda")

# set up parallel processing 
registerDoMC(cores = 10)

# define model ----
nn_spec <-
  mlp(hidden_units = tune(),
      penalty= tune()) %>%
  set_engine('nnet') %>%
  set_mode('regression')

# check tuning parameters
hardhat::extract_parameter_set_dials(nn_spec) 

# setup tuning grid 
nn_params <- hardhat::extract_parameter_set_dials(nn_spec) 

# define tuning grid
nn_grid <- grid_regular(nn_params, levels = 5)

## basic recipe----
# workflow
nn_wflow <-
  workflow() %>% 
  add_model(nn_spec) %>% 
  add_recipe(basic_recipe)

# tuning / fitting
tic.clearlog()
tic("NN")

nn_tune <- nn_wflow %>% 
  tune_grid(
    resamples = life_folds, 
    grid = nn_grid,
    control = keep_pred,
    metrics = life_metrics
  )

toc(log = TRUE)

# write out results & workflow
time_log <- tic.log(format = FALSE)

nn_tic_toc <- tibble(model = time_log[[1]]$msg,
                     start_time = time_log[[1]]$tic,
                     end_time = time_log[[1]]$toc,
                     runtime = end_time - start_time
)

# write out results & workflow
save(nn_tune, nn_wflow, nn_tic_toc, file = "results/nn_tune.rda")

## filtered recipe----
# workflow
nn_wflow <-
  workflow() %>% 
  add_model(nn_spec) %>% 
  add_recipe(filtered_recipe)

# tuning / fitting
tic.clearlog()
tic("NN - Filtered")

nn_tune <- nn_wflow %>% 
  tune_grid(
    resamples = life_folds, 
    grid = nn_grid,
    control = keep_pred,
    metrics = life_metrics
  )

toc(log = TRUE)

# write out results & workflow
time_log <- tic.log(format = FALSE)

nn_tic_toc <- tibble(model = time_log[[1]]$msg,
                     start_time = time_log[[1]]$tic,
                     end_time = time_log[[1]]$toc,
                     runtime = end_time - start_time
)

# write out results & workflow
save(nn_tune, nn_wflow, nn_tic_toc, file = "results/nn_tune_filter.rda")