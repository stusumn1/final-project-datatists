## model performance

# Model comparison, selection, & evaluation ----

# Load package(s) ----
library(tidymodels)
library(tidyverse)


# handle common conflicts 
tidymodels_prefer()

# load required objects 
# list.files("results", pattern = "*.rda", full.names = TRUE)
#   for(i in "results") {
#     load(i)
#   }

load("results/tune_bt.rda")

load("results/tune_en.rda")

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

#Should not be like this? The code bellow this is giving me error.

# temp <- model_set %>%
#   collect_metrics() %>%
#   filter(.metric == "rmse") %>%
#   group_by(wflow_id) %>%
#   slice_max(order_by = mean) %>%
#   DT::datatable()


temp <- model_set %>% 
  collect_metrics() %>% 
  filter(.metric == "rmse") %>% 
  slice_max(order_by = mean, by = wflow_id) %>% 
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

times <- c("22.4",
           "31.1",
           "4.73",
           "458",
           "38.3",
           "173",
           "72.9",
           "13")

time_tibble <- bind_cols(model_type, times)
time_df <- as_data_frame(time_tibble)

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
