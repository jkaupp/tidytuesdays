library(tidyverse)
library(here)
library(janitor)

census_data <- dir(here("week5", "data"), full.names = TRUE) %>% 
  read_csv() %>% 
  clean_names()

# Lets look at commuting!
commuting_data <- census_data %>% 
  select(census_id, state, county, total_pop, drive:mean_commute)

