#rf

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

rf_spec <- 
  rand_forest(mtry = tune(),
              min_n = tune()) %>%
  set_engine("ranger") %>% 
  set_mode("regression")


rf_params <- hardhat::extract_parameter_set_dials(rf_spec) %>% 
  update(
    # using a range of one to 4 because there are seven available columns
    mtry = mtry(range = c(1, 4))
  )


# define grid
rf_grid <- grid_regular(rf_params, levels = 5)


# workflow ----

rf_wflow <-
  workflow() %>% 
  add_model(rf_spec) %>% 
  add_recipe(life_expec_recipe)


# tuning
rf_tune <-
  rf_wflow %>% 
  tune_grid(
    resamples = life_expec_fold,
    grid = rf_grid
  )



# write out results
save(rf_tune, rf_wflow, file = "results/tune_rf.rda")


