set.seed(4444)

## load packages
library(tidyverse)
library(tidymodels)
tidymodels_prefer()

## data source: https://www.kaggle.com/datasets/lashagoch/life-expectancy-who-updated
life_expec <- read_csv("data/raw/life_expectancy_initial.csv")
skimr::skim(life_expec)

## create current and previous year data by country, by year
colnames <- c("Country", "Following_year", "Following_life_expect")

following_year <- life_expec %>% 
  group_by(Country, Year) %>% 
  select(Country, Year, Life_expectancy)
colnames(following_year) <- colnames

following_year$Year <- following_year$Following_year - 1

life <- full_join(life_expec, following_year, c("Country", "Year")) %>% 
  janitor::clean_names() %>% 
  mutate(
    region = factor(region),
    developed = factor(economy_status_developed, levels = c(1, 0))
  )

## split data using 2015 data as test
life_test <- life %>% 
  filter(year == 2014) %>% 
  select(-1, -3, -19, -20, -22)

life_train <-  life %>% 
  filter(year != 2014 & year != 2015) %>% 
  select(-1, -3, -19, -20, -22)

## create folds
life_folds <- vfold_cv(life_train, v = 10, repeats = 3)

## create initial recipe
basic_recipe <- recipe(following_life_expect ~ ., data = life_train) %>% 
  step_rm(thinness_ten_nineteen_years) %>% 
  step_dummy(region, developed) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_nzv() %>% 
  prep()

bake(basic_recipe, new_data = NULL)

## save some initial setup
keep_pred <- control_grid(save_pred = T, save_workflow = T)
life_metrics <- yardstick::metric_set(rmse,
                                      mae,
                                      rsq)

save(life_test, 
     life_train, 
     life_folds, 
     basic_recipe,
     keep_pred, 
     life_metrics, file = "initial_setup/tuning_setup.rda")
