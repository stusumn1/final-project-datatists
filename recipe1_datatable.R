library(tidymodels)
library(tidyverse)


# handle common conflicts 
tidymodels_prefer()

# load data
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
    "elastic net" = en_tune,
    "random forest" = rf_tune,
    "k nearest neighbor" = knn_tune,
    "boosted tree" = bt_tune,
    "neural network" = nn_tune,
    "svm polynomial" = svm_poly_tune,
    "svm radial" = svm_radial_tune,
    "mars" = mars_tune
  )


temp <- model_set %>% 
  collect_metrics() %>% 
  filter(.metric == "rmse") %>% 
  slice_min(order_by = mean, by = wflow_id) %>% 
  DT::datatable() 

temp
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

save(temp, time_df, file = "datatable1.rda")
