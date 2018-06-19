library(tidyverse)
library(lubridate)

trend_data <- read_csv("https://github.com/rfordatascience/tidytuesday/raw/master/data/week12_google_trends.csv", skip = 2, col_names = TRUE) %>% 
  set_names(str_extract(names(.), "(?<=Hurricane )(\\w+)|(Day)")) %>% 
  rename(Date = Day) %>% 
  mutate(source = "Google Trends")


mediacloud_data <- read_csv("https://github.com/rfordatascience/tidytuesday/raw/master/data/week12_mediacloud_hurricanes.csv", col_names = TRUE) %>% 
  mutate(source = "Mediacloud") %>% 
  mutate(Date = mdy(Date))

tv_data <- read_csv("https://github.com/rfordatascience/tidytuesday/raw/master/data/week12_tv_hurricanes.csv") %>% 
  mutate(source = "TV Shares") %>% 
  mutate(Date = mdy(Date))


bind_rows(trend_data, mediacloud_data, tv_data) %>% 
