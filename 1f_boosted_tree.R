#bt

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

bt_spec <-
  boost_tree(mtry = tune(),
             learn_rate = tune(), 
             min_n = tune()) %>%
  set_engine('xgboost') %>%
  set_mode('regression')

# check tuning parameters
hardhat::extract_parameter_set_dials(bt_spec) 

# setup tuning grid 
bt_params <- hardhat::extract_parameter_set_dials(bt_spec) %>% 
  update(
    mtry = mtry(range = c(1, 4)),
    learn_rate = learn_rate(range = c(-5, -0.2))
  )

# define grid
bt_grid <- grid_regular(bt_params, levels = 5)

# workflow ----

bt_workflow <-
  workflow() %>% 
  add_model(bt_spec) %>% 
  add_recipe(life_expec_recipe) 

# tuning
bt_tune <-
  bt_workflow %>% 
  tune_grid(
    resamples = life_expec_fold,
    grid = bt_grid
  )


# write out results
save(bt_tune, bt_workflow, file = "results/tune_bt.rda")
