# Multivariate Adaptive Regression Splines (MARS) tuning ----

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
mars_spec <- mars(
  mode = "regression",
  num_terms = tune(),
  prod_degree = tune()
) %>%
  set_engine("earth")

# set-up tuning grid ----
mars_params <- hardhat::extract_parameter_set_dials(mars_spec) %>% 
  update(
    num_terms = num_terms(range = c(1, 45))
  )

# define tuning grid
mars_grid <- grid_regular(mars_params, levels = c(5, 5))

# workflow ----
mars_workflow <-
  workflow() %>% 
  add_model(mars_spec) %>%
  add_recipe(basic_recipe)

# Tuning/fitting ----
tic.clearlog()
tic("Mars")

mars_tune <-
  mars_workflow %>% 
  tune_grid(
    resamples = life_folds,
    grid = mars_grid,
    control = keep_pred,
    metrics = life_metrics
  )

# Pace tuning code in hear
toc(log = TRUE)

# save runtime info
time_log <- tic.log(format = FALSE)

mars_tictoc <- tibble::tibble(
  model = time_log[[1]]$msg,
  start_time = time_log[[1]]$tic,
  end_time = time_log[[1]]$toc,
  runtime = end_time - start_time
)

# Write out results & workflow
save(mars_tune, mars_tictoc, file = "results/mars_tune.rda")
