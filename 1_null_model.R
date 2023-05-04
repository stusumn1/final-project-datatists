## null model

#load packages
library(tidyverse)
library(tidymodels)
tidymodels_prefer()

# set seed
set.seed(4444)

# load required objects ----
load("initial_setup/tuning_setup.rda")

null_spec <- 
  null_model() %>% 
  set_engine("parsnip") %>% 
  set_mode("regression")

null_wflow <- 
  workflow() %>% 
  add_model(null_spec) %>% 
  add_recipe(basic_recipe)


null_tune <- fit_resamples(
  null_wflow,
  resamples = life_folds,
  control = control_resamples(save_pred = TRUE)
)

null_results <- null_tune %>% 
  collect_metrics()

# save results
save(null_wflow, null_tune, null_wflow, file = "results/null_fit.rda")
