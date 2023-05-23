## elastic net

#load packages
library(tidyverse)
library(tidymodels)
library(patchwork)
library(tictoc)

#handle common conflicts
tidymodels_prefer()


# set seed
set.seed(2468)

# load required objects ----
load("initial_setup/tuning_setup.rda")


# elastic net

elastic_net_spec <-
  linear_reg(penalty = tune(), mixture = tune()) %>% 
  set_engine("glmnet") %>% 
  set_mode("regression")


elastic_net_params <- hardhat::extract_parameter_set_dials(elastic_net_spec)


# define grid
elastic_net_grid <- grid_regular(elastic_net_params, levels = 5)

elastic_net_wflow <-
  workflow() %>% 
  add_model(elastic_net_spec) %>% 
  add_recipe(filtered_recipe)

# Tuning/fitting ----
tic.clearlog()
tic("Elastic Net - Filtered")

elastic_net_tune <-
  elastic_net_wflow %>% 
  tune_grid(
    resamples = life_folds,
    grid = elastic_net_grid,
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

save(elastic_net_tune, elastic_net_wflow, en_tictoc, file = "results/tune_en_filter.rda")