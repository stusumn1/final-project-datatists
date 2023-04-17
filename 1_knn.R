# knn

# load packages

library(tidyverse)
library(tidymodels)
library(patchwork)

# handle common conflicts
tidymodels_prefer()

# set seed
set.seed(2468)

# load required objects ----
load("initial_setup/tuning_setup.rda")



# define model ----

knn_spec <-
  nearest_neighbor(
    neighbors = tune()) %>%
  set_engine("kknn") %>%
  set_mode('regression')

# check tuning parameters
# hardhat::extract_parameter_set_dials(knn_spec) 

# setup tuning grid 
knn_params <- hardhat::extract_parameter_set_dials(knn_spec) %>% 
  update(
    neighbors = neighbors(range = c(1,20))
  )

# define grid
knn_grid <- grid_regular(knn_params, levels = 5)

# workflow ----

knn_workflow <-
  workflow() %>% 
  add_model(knn_spec) %>% 
  add_recipe(life_expec_recipe)

# tuning
knn_tune <-
  knn_workflow %>% 
  tune_grid(
    resamples = life_expec_fold,
    grid = knn_grid
  )

# write out results
save(knn_tune, knn_workflow, file = "results/tune_knn.rda")
