library(tidyverse)
library(ggraph)
library(tidygraph)
library(jkmisc)
library(lubridate)
library(here)
library(patchwork)
library(nord)

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
                  subtitle = str_wrap("This chart presents a time series of circle-packed network representations of the television dramas.  
                                      The larger dark blue circle represents the year, light blue represents the genre (Action, Comedy, etc.) and the pale pink represents the individual program. 
                                      The area of each circle (node) is porportional to the sum of the audience share of the smaller circles within (child nodes).", 180),
                  caption = "data: IMDb | graphic: @jakekaupp",
                  theme = theme_jk(plot_title_size  = 22, subtitle_size = 14) %+replace% theme(plot.background = element_rect(fill ="#2E3440"),
                                                      text = element_text(color = "white")))
 
ggsave(here("2019","week2", "tt_week2.png"), out, width = 16, height = 8)
