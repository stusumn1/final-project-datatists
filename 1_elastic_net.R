## elastic net

#load packages
library(tidyverse)
library(tidymodels)
library(patchwork)

#handle common conflicts
tidymodels_prefer()


# set seed
set.seed(2468)

# load required objects ----
load("initial_setup/tuning_setup.rda")
load("initial_setup/life_expec_split.rda")


# elastic net

elastic_net_spec <-
  logistic_reg(penalty = tune(), mixture= tune()) %>% 
  set_engine("glmnet") %>% 
  set_mode("regression")

elastic_net_wflow <-
  workflow() %>% 
  add_model(elastic_net_spec) %>% 
  add_recipe(life_expec_recipe)

elastic_net_params <- hardhat::extract_parameter_set_dials(elastic_net_spec)

# define grid
elastic_net_grid <- grid_regular(elastic_net_params, levels = 5)


elastic_net_tune <-
  elastic_net_wflow %>% 
  tune_grid(
    resamples = fraud_fold,
    grid = elastic_net_grid
  )

save(elastic_net_tune, elastic_net_wflow, file = "results/tune_en.rda")

