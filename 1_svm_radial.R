# SVM Radial tuning ----

# Load package(s) ----
library(tidyverse)
library(tidymodels)
library(tictoc)
library(doMC)

# handle common conflicts
tidymodels_prefer()

# set seed
set.seed(4444)

# set up parallel processing 
registerDoMC(cores = 8) 

# load required objects ----
load("initial_setup/tuning_setup.rda")

# Define model ----
svm_radial_spec <- svm_rbf(
  mode = "regression", 
  cost = tune(),
  rbf_sigma = tune()
) %>% 
  set_engine("kernlab")

# set-up tuning grid ----
svm_radial_params <- hardhat::extract_parameter_set_dials(svm_radial_spec)

# define tuning grid
svm_radial_grid <- grid_regular(svm_radial_params, levels = 5)

# workflow ----
svm_radial_wflow <-
  workflow() %>% 
  add_model(svm_radial_spec) %>% 
  add_recipe(filtered_recipe)

# Tuning/fitting ----
tic.clearlog()
tic("SVM Radial - Filtered")

svm_radial_tune <- 
  svm_radial_wflow %>% 
  tune_grid(
    resamples = life_folds,
    grid = svm_radial_grid,
    control = keep_pred,
    metrics = life_metrics
  )

# Pace tuning code in hear
toc(log = TRUE)

# save runtime info
time_log <- tic.log(format = FALSE)

svm_radial_tictoc <- tibble(
  model = time_log[[1]]$msg,
  start_time = time_log[[1]]$tic,
  end_time = time_log[[1]]$toc,
  runtime = end_time - start_time
)

# Write out results & workflow
save(svm_radial_tune, svm_radial_tictoc, file = "results/svm_radial_tune_filter.rda")