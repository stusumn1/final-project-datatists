## null model

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

null_spec <- 
  null_model() %>% 
  set_engine("parsnip") %>% 
  set_mode("regression")


null_wflow <- 
  workflow() %>% 
  add_model(null_spec) %>% 
  add_recipe(life_expec_recipe)

null_tune <- 
  null_wflow %>% 
  tune_grid(
    resamples = life_expec_fold
  )

# write out results

save(null_tune, null_wflow, file = "results/fit_null.rda")
