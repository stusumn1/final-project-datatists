## model performance

# Model comparison, selection, & evaluation ----

# Load package(s) ----
library(tidymodels)
library(tidyverse)
library(gt)


# handle common conflicts 
tidymodels_prefer()

# basic recipe ----
# load required objects 

load("results/bt_tune.rda")
load("results/en_tune.rda")
load("results/knn_tune.rda")
load("results/mars_tune.rda")
load("results/rf_tune.rda")
load("results/svm_poly_tune.rda")
load("results/svm_radial_tune.rda")
load("results/nn_tune.rda")


load("initial_setup/tuning_setup.rda")

# compare models
model_set <- 
  as_workflow_set( 
    "elastic net" = elastic_net_tune,
    "random forest" = rf_tune,
    "k nearest neighbor" = knn_tune,
    "boosted tree" = bt_tune,
    "neural network" = nn_tune,
    "svm polynomial" = svm_poly_tune,
    "svm radial" = svm_radial_tune,
    "mars" = mars_tune
  )

#Should not be like this? The code below this is giving me error.

# temp <- model_set %>%
#   collect_metrics() %>%
#   filter(.metric == "rmse") %>%
#   group_by(wflow_id) %>%
#   slice_min(order_by = mean)

View(
  rank_results(model_set, rank_metric = "rmse") %>% 
  filter(.metric == "rmse") %>% 
  group_by(wflow_id) %>%
  mutate(within_wflow_rank = row_number()) %>% 
  filter(within_wflow_rank == 1)
  )

temp <- model_set %>% 
  collect_metrics() %>% 
  filter(.metric == "rmse") %>% 
  slice_min(order_by = mean, by = wflow_id) %>% 
  mutate(mean = round(mean, 3), std_err = round(std_err, 3)) %>% 
  select(-2, -3, -4, -6, -8) %>% 
  DT::datatable()

temp %>% 
  select(wflow_id, mean) %>% 
  mutate(mean = round(mean, 3)) %>% 
  (order_by = mean) %>% 
  DT::datatable() 

bind_cols()

model_set_2 <- 
  as.data.frame( 
    "elastic net" = en_tic_toc,
    "random forest" = rf_tic_toc,
    "k nearest neighbor" = knn_tic_toc,
    "boosted tree" = bt_tic_toc,
    "neural network" = nn_tic_toc,
    "svm polynomial" = svm_poly_tic_toc,
    "svm radial" = svm_tic_toc,
    "mars" = mars_tic_toc
  )


model_type <- c("elastic net",
                "random forest",
                "k nearest neighbor",
                "boosted tree",
                "neural network",
                "svm polynomial",
                "svm radial",
                "mars")

times <- c("18.0",
           "247",
           "14.1",
           "79.0",
           "45.8",
           "54.0",
           "52.6",
           "5.42")

time_tibble <- bind_cols(model_type, times)
time_df <- as.data.frame(time_tibble)

names(time_df) = c("Model Type", "Run Time (seconds)")

time_df %>% 
  DT::datatable()



nn_spec <-
  mlp(hidden_units = tune(),
      penalty= tune()) %>%
  set_engine('nnet') %>%
  set_mode('classification')


# workflow ----

nn_wflow <-
  workflow() %>% 
  add_model(nn_spec) %>% 
  add_recipe(wf_base_recipe)

nn_best <- show_best(nn_tune, metric = "roc_auc") %>% head(1)

nn_workflow_tuned <- nn_wflow %>% 
  finalize_workflow(select_best(nn_tune, metric = "roc_auc"))

nn_fit <- fit(nn_workflow_tuned, wf_train)

# filtered recipe ----
# load required objects
load("results/mars_tune_filter.rda")
load("results/knn_tune_filter.rda")
load("results/nn_tune_filter.rda")
load("results/rf_tune_filter.rda")
load("results/svm_poly_tune_filter.rda")
load("results/svm_radial_tune_filter.rda")
load("results/bt_tune_filter.rda")
load("results/en_tune_filter.rda")

# compare models
model_set <- 
  as_workflow_set( 
    "elastic net" = elastic_net_tune,
    "random forest" = rf_tune,
    "k nearest neighbor" = knn_tune,
    "boosted tree" = bt_tune,
    "neural network" = nn_tune,
    "svm polynomial" = svm_poly_tune,
    "svm radial" = svm_radial_tune,
    "mars" = mars_tune
  )

filter_models <- model_set %>% 
  collect_metrics() %>% 
  filter(.metric == "rmse") %>% 
  slice_min(order_by = mean, by = wflow_id) %>% 
  mutate(mean = round(mean, 3), std_err = round(std_err, 3)) %>% 
  select(-2, -3, -4, -6, -8) %>% 
  DT::datatable()

filter_models

filter_tictoc <- bind_rows(en_tictoc, 
                           rf_tictoc, 
                           knn_tictoc, 
                           bt_tictoc, 
                           nn_tic_toc, 
                           svm_poly_tictoc, 
                           svm_radial_tictoc,
                           mars_tictoc)

gt(filter_tictoc) %>% 
  gt::cols_hide(c(2, 3)) %>% 
  gt::cols_label(model = "Model", runtime = "Runtime")

