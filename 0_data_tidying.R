library(tidyverse)

life <- read_csv("data/raw/life_expectancy_initial.csv")
skimr::skim(life)

colnames <- c("Country", "Following_year", "Following_life_expect")

following_year <- life %>% 
  group_by(Country, Year) %>% 
  select(Country, Year, Life_expectancy)
colnames(following_year) <- colnames

following_year$Year <- following_year$Following_year - 1

life %>% 
  arrange(desc(Year))

x <- full_join(life, following_year, c("Country", "Year"))

x %>% 
  filter(Country == "Jordan") %>% 
  select(1, 2, 3, "Life_expectancy", 23) %>% 
  arrange(desc(Year))

view(x)

x %>% 
  filter(Country == "Jordan") %>% 
  view()
