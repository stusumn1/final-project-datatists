#Model comparison, selection, and evaluation

#Load packages
library(tidyverse)
library(tidymodels)

#Handle common conflicts
tidymodels_prefer()

#Set seed
set.seed(86420)

#Load require objects
load("results/rf_tune_filter.rda")

load("initial_setup/tuning_setup.rda")

#Quickly access the tuning parameters of the winning model

rf_tune %>% 
  select_best(metric = "rmse") 

#Specifically setting the winning model

rf_spec_win <- rand_forest(mode = "regression", 
                   mtry = 7, 
                   min_n = 2) %>% 
  set_engine("ranger")

#Updating the workflow

win_workflow <- rf_tune %>%
  extract_workflow() %>%
  update_model(rf_spec_win)

save(win_workflow, file = "results/rf_tune_win.rda")

#Fitting the winning model

#Splitting data

#We already have the data splitted.

#Fitting the winning rf model in the training dataset.

rf_fit <-
  parsnip::fit(win_workflow, data = life_train)

#Model's performance

#RMSE

final_prediction <- life_test %>%    
  bind_cols(predict(rf_fit, new_data = life_test, type = "numeric")) %>% 
  select(following_life_expect, .pred)

metrics_rmse <- metric_set(rmse)

#Final result as a table

final_table <- tibble(metrics_rmse(final_prediction, 
                                      truth = following_life_expect, .pred))

# #Plotting (NOT REALLY SURE HOW TO DO THIS -- EMI)
# 
# final_plot <- autoplot(rmse(final_prediction, 
#                                  truth = following_life_expect, 
#                                  estimate = .pred))

save(final_table, file = "tables/final_table.rda")
