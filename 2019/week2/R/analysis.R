library(tidyverse)
library(ggraph)
library(tidygraph)
library(jkmisc)
library(lubridate)
library(here)
library(patchwork)

set.seed(42)

source(here("2019", "week2", "R", "functions.R"))

# Read data from github repo
tv_data <- read_csv("https://github.com/rfordatascience/tidytuesday/raw/master/data/2019/2019-01-08/IMDb_Economist_tv_ratings.csv") %>% 
  rename(title_id = titleId,
         season_number = seasonNumber) %>% 
  mutate(year = year(date))

# Make this into a nodes tibble
list <- tv_data %>% 
  split(.$year) %>% 
  map(share_packed_circle)

out <- wrap_plots(list, ncol = 10, nrow = 3) +
  plot_annotation(title = "The Evolution and Differentiation of Dramas Across the Golden Age of Television",
                  subtitle = str_wrap("This chart presents a time series of circle-packed network representations of the television dramas.  The larger blue circle represents the year, pink represents the genre (Action, Comedy, etc.) and the yellow represents the individual program.", 200),
                  caption = "data: Economist | graphic: @jakekaupp",
                  theme = theme_jk())
 
ggsave(here("2019","week2", "tt_week2.png"), out, width = 16, height = 10)
