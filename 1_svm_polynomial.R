# SVM (polynomial) tuning ----

# Load package(s) ----
library(tidyverse)
library(tidymodels)
library(tictoc)
library(doMC)

# handle common conflicts
tidymodels_prefer()

#Setup parallel processing
registerDoMC(cores = 4)

# load required objects ----
load("initial_setup/tuning_setup.rda")

# Define model ----
svm_poly_spec <- svm_poly(
  mode = "regression",
  cost = tune(),
  degree = tune(),
  scale_factor = tune()
) %>%
  set_engine("kernlab")

# set-up tuning grid ----
svm_poly_params <- hardhat::extract_parameter_set_dials(svm_poly_spec)

# define tuning grid
svm_poly_grid <- grid_regular(svm_poly_params, levels = c(3, 3, 3))

# workflow ----
svm_poly_workflow <-
  workflow() %>% 
  add_model(svm_poly_spec) %>%
  add_recipe(basic_recipe)

# Tuning/fitting ----
tic.clearlog()
tic("Polynomial SVM")

svm_poly_tune <-
  svm_poly_workflow %>% 
  tune_grid(
    resamples = life_folds,
    grid = svm_poly_grid,
    control = keep_pred,
    metrics = life_metrics
  )

# Pace tuning code in hear
toc(log = TRUE)

# save runtime info
time_log <- tic.log(format = FALSE)

svm_poly_tictoc <- tibble::tibble(
  model = time_log[[1]]$msg,
  start_time = time_log[[1]]$tic,
  end_time = time_log[[1]]$toc,
  runtime = end_time - start_time
)

# Write out results & workflow
save(svm_poly_tune, svm_poly_tictoc, file = "results/svm_poly_tune.rda")