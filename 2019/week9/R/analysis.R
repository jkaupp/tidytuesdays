library(tidyverse)
library(lubridate)
library(here)
library(ggforce)
library(jkmisc)
library(glue)
library(colorspace)

full_trains <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-26/full_trains.csv")

#arrival_cols <- set_names(map_chr(c(0, 0.2, 0.4), ~lighten("#101935", .x)), as.character(2015:2017))
departure_cols <- set_names(map_chr(c(0, 0.2, 0.4), ~lighten("#7f2982", .x)), 2015:2017)

# Assign colors to delays and arrivals and lighten/darken by years
test <- full_trains %>% 
  filter(departure_station == "PARIS EST", service == "National") %>% 
  unite("route", departure_station, arrival_station, sep = "-") %>% 
  #mutate_at(c("avg_delay_late_at_departure"), function(x)  -x)  %>% 
  group_by(year, month, route) %>% 
  summarize_at(c("avg_delay_late_at_departure", "avg_delay_late_on_arrival"), mean) %>% 
  pivot_longer(4:5)



test %>% 
  ungroup() %>% 
  split(list(.$year, .$route, .$name))
  mutate(splines = map2(month, value, ~smooth.spline(.x, .y)))


fit_spline <- function(x, y) {

  spline <- smooth.spline(x, y)
  
  tibble(month = spline$x,
         value = spline$y)
  
  
}

s <- smooth.spline(test$month, test$avg_delay_late_at_departure)

ggplot(test) +
  geom_area(aes(x = month, y = avg_delay_late_at_departure, group = year, fill = factor(year)), position = position_identity(), alpha = 0.4) +
  geom_area(aes(x = month, y = avg_delay_late_on_arrival, group = year, fill = factor(year)), position = position_identity(), alpha = 0.4) +
  coord_polar() +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  scale_fill_manual(values = departure_cols) +
  facet_wrap(~route) +
  theme_jk(grid = "X") +
  theme(axis.text.y = element_blank())
  
  

  
