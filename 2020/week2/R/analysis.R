library(tidyverse)
library(jkmisc)
library(lubridate)
library(here)
library(jkmisc)
library(ggforce)
library(scales)
library(patchwork)
library(fs)
library(janitor)
library(glue)

rainfall <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/rainfall.csv')

temperature <- here("2020", "week2", "data") %>% 
  dir_ls() %>% 
  map_dfr(~read_csv(.x) %>% 
            mutate(name = basename(.x))) %>% 
  separate(name, c("station", "city_name", "drop"), sep = "-") %>% 
  select(-drop) %>% 
  clean_names()

#temperature <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/temperature.csv')

yearly_temp <- temperature %>% 
  group_by(year) %>% 
  summarize(temperature = mean(maximum_temperature_degree_c, na.rm = TRUE)) %>% 
  filter(year < 2020)

yearly_rain <- rainfall %>% 
  group_by(year, month) %>% 
  summarize(rainfall = mean(rainfall, na.rm = TRUE)) %>% 
  group_by(year) %>% 
  summarize(rainfall = sum(rainfall, na.rm = TRUE)) %>% 
  filter(year < 2020)

rain <- ggplot(yearly_rain, aes(x = year, y = rainfall)) +
  geom_point(color = "#afc3cc") +
  geom_path(color = "#afc3cc") +
  geom_mark_circle(aes(filter = year == 2019, label = glue("Total Rainfall: {round(rainfall,1)} mm"), description = "Annual rainfall at a 60 year low."), label.family = c("Oswald", "Lora"), label.buffer = unit(6, "cm"), label.fontsize = 10) +
  labs(x = NULL,
       y = NULL) +
  scale_x_continuous(limits = c(1900, 2020), breaks = seq(1900, 2020, 10)) +
  scale_y_continuous(limits = c(10, 50), breaks = seq(10, 50, 10), label = label_number_si(unit = "mm")) +
  theme_jk(grid = "XY")

temp <- ggplot(yearly_temp, aes(x = year, y = temperature)) +
  geom_point(color = "#AF111C") +
  geom_path(color = "#AF111C") +
  geom_mark_circle(aes(filter = year == 2019, label = glue("Avg. Max Temp.: {round(temperature, 1)}°C"), description = "Close to peak historic temperatures"), label.family = c("Oswald", "Lora"), label.buffer = unit(3, "cm"), label.fontsize = 10) +
  labs(x = NULL,
       y = NULL) +
  scale_x_continuous(limits = c(1900, 2020), breaks = seq(1900, 2020, 10)) +
  scale_y_continuous(limits = c(10, 30), breaks = seq(10, 30, 10), label = label_number_si(unit = "° C", sep = ""), position = "right") +
  theme_jk(grid = "XYy")

plot <- rain + temp +
  plot_annotation(title = "Historic Lows in Rainfall and Records High Temperatures A Factor In Australias Devastating Wildfires",
                  subtitle = glue('Illustrated below are the {highlight_text("total rainfall", "#afc3cc", "b")} and {highlight_text("average maximum temperature", "#AF111C", "b")} measured across Australian cities from 1900 to 2019. These extreme events driven by climate change,<br>have turned Australia grasslands and wooden areas into a tinderbox which fuels these devastating wildfires across the country.  To support relief efforts please visit:<br> **redcross.org.au/campaigns/disaster-relief-and-recovery-donate**'),
                  caption = "**Data**: Australian Bureau of Meterology | **Graphic**: @jakekaupp",
                  theme = theme_jk(markdown = TRUE))

ggsave(here("2020", "week2", "tw2_plot.png"), plot, width = 15, height = 7)

