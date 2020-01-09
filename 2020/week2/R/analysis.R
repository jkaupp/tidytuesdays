library(tidyverse)
library(here)
library(jkmisc)
library(lubridate)
library(jkmisc)
library(ggforce)
library(scales)
library(patchwork)

rainfall <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/rainfall.csv')
temperature <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/temperature.csv')

yearly_temp <- temperature %>% 
  mutate(year = year(date),
         month = month(date)) %>% 
  filter(temp_type == "max") %>% 
  group_by(year) %>% 
  summarize(temperature = mean(temperature, na.rm = TRUE)) %>% 
  filter(year < 2020)

yearly_rain <- rainfall %>% 
  group_by(year, month) %>% 
  summarize(rainfall = mean(rainfall, na.rm = TRUE)) %>% 
  group_by(year) %>% 
  summarize(rainfall = sum(rainfall, na.rm = TRUE)) %>% 
  filter(year < 2020, year >= min(yearly_temp$year))

rain <- ggplot(yearly_rain, aes(x = year, y = rainfall)) +
  geom_point(color = "#afc3cc") +
  geom_path(color = "#afc3cc") +
  geom_mark_circle(aes(filter = year == 2019, label = glue("Total Rainfall: {round(rainfall,1)} mm"), description = "Annual rainfall at a 60 year low."), label.family = c("Oswald", "Lora")) +
  labs(x = NULL,
       y = NULL) +
  scale_x_continuous(limits = c(1910, 2020), breaks = seq(1910, 2020, 10)) +
  scale_y_continuous(limits = c(10, 50), breaks = seq(10, 50, 10), label = label_number_si(unit = "mm")) +
  theme_jk(grid = "XY")

temp <- ggplot(yearly_temp, aes(x = year, y = temperature)) +
  geom_point(color = "#AF111C") +
  geom_path(color = "#AF111C") +
  geom_mark_circle(aes(filter = year == 2019, label = glue("Average Max Temperature: {round(temperature, 1)}°C"), description = "Record shattering temperatures ≈ 5°C above historic average."), label.family = c("Oswald", "Lora")) +
  labs(x = NULL,
       y = NULL) +
  scale_x_continuous(limits = c(1910, 2020), breaks = seq(1910, 2020, 10)) +
  scale_y_continuous(limits = c(10, 30), breaks = seq(10, 30, 10), label = label_number_si(unit = "° C", sep = ""), position = "right") +
  theme_jk(grid = "XY")

plot <- rain + temp +
  plot_annotation(title = "Historic Lows in Rainfall and Records High Temperatures A Factor In Australias Devastating Wildfires",
                  subtitle = glue('Illustrated below are the {highlight_text("total rainfall", "#afc3cc", "b")} and {highlight_text("average maximum temperature", "#AF111C", "b")} measured across Australian cities from 1910 to 2019. These extreme events driven by climate change, have turned Australia grasslands and wooden areas into a tinderbox which fuels these devastating wildfires across the country.  To support relief efforts visit: https://www.redcross.org.au/campaigns/disaster-relief-and-recovery-donate'),
                  caption = "**Data**: Australian Bureau of Meterology | **Graphic**: @jakekaupp",
                  theme = theme_jk(markdown = TRUE))

ggsave(here("2020", "week2", "tw2_plot.png"), plot, width = 14, height = 7)

