library(tidyverse)
library(here)
library(jkmisc)


# Read in the data
salary_data <- dir(here("week4","data"), pattern = "salary", full.names = TRUE) %>% 
  read_csv()
 