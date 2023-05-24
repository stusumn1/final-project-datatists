# KNN Tuning

# load packages
library(tidyverse)
library(tidymodels)
library(patchwork)
library(tictoc)
library(doMC)

# handle common conflicts
tidymodels_prefer()

# set seed
set.seed(2468)

# load required objects
load("initial_setup/tuning_setup.rda")

# define model ----
knn_spec <-
  nearest_neighbor(
    neighbors = tune()) %>%
  set_engine("kknn") %>%
  set_mode('regression')

# setup tuning grid 
knn_params <- hardhat::extract_parameter_set_dials(knn_spec) %>% 
  update(
    neighbors = neighbors(range = c(1, 20))
  )

# define grid
knn_grid <- grid_regular(knn_params, levels = 5)

## basic recipe----
# workflow
knn_wflow <-
  workflow() %>% 
  add_model(knn_spec) %>% 
  add_recipe(basic_recipe)

# tuning
tic.clearlog()
tic("KNN")

knn_tune <-
  knn_wflow %>% 
  tune_grid(
    resamples = life_folds,
    grid = knn_grid,
    control = keep_pred,
    metrics = life_metrics
  )

# pace tuning code in hear
toc(log = TRUE)

# save runtime info
time_log <- tic.log(format = FALSE)

knn_tictoc <- tibble(
  model = time_log[[1]]$msg,
  start_time = time_log[[1]]$tic,
  end_time = time_log[[1]]$toc,
  runtime = end_time - start_time
)

# write out results
save(knn_tune, knn_tictoc, knn_wflow, file = "results/knn_tune.rda")

## filtered recipe----
# workflow
knn_wflow <-
  workflow() %>% 
  add_model(knn_spec) %>% 
  add_recipe(filtered_recipe)

# tuning
tic.clearlog()
tic("KNN - Filtered")

knn_tune <-
  knn_wflow %>% 
  tune_grid(
    resamples = life_folds,
    grid = knn_grid,
    control = keep_pred,
    metrics = life_metrics
  )

toc(log = TRUE)

# save runtime info
time_log <- tic.log(format = FALSE)

knn_tictoc <- tibble(
  model = time_log[[1]]$msg,
  start_time = time_log[[1]]$tic,
  end_time = time_log[[1]]$toc,
  runtime = end_time - start_time
)

# write out results
save(knn_tune, knn_tictoc, knn_wflow, file = "results/knn_tune_filter.rda")