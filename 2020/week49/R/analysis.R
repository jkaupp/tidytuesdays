library(tidyverse)
library(lubridate)
library(here)
library(jkmisc)
library(glue)
library(fs)
library(janitor)
library(patchwork)
library(colorspace)

shelters <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-01/shelters.csv')


occupancy_rate_data <- shelters %>% 
  mutate(day = wday(occupancy_date, label = TRUE),
         yday = yday(occupancy_date),
         week = week(occupancy_date),
         month = month(occupancy_date, label = TRUE),
         year = year(occupancy_date)) %>% 
  filter(shelter_city == "Toronto") %>% 
  group_by(sector, year, yday) %>% 
  summarize(avg_rate = mean(occupancy/capacity,na.rm = TRUE)) %>% 
  arrange(sector, year, yday) %>% 
  group_by(sector) %>% 
  mutate(idx = row_number(),
         avg_rate = replace(avg_rate, is.infinite(avg_rate), 1))

remainder <- occupancy_rate_data %>% 
  mutate(top = if_else(avg_rate > 1, avg_rate, Inf),
         bottom = if_else(avg_rate >1, 1, avg_rate)) %>% 
  filter(!is.infinite(top))

gradients <- sort(c(max(occupancy_rate_data$avg_rate),
                    tail(occupancy_rate_data$avg_rate,1),
                    pretty(c(0, min(occupancy_rate_data$avg_rate)),
                           n = 20, min.n = 20)))


labels <- occupancy_rate_data %>% 
  distinct(sector) %>% 
  mutate(idx = 10,
         avg_rate = -0.05)

shades <- pmap(list(head(gradients,-1),
                    tail(gradients,-1),
                    "#BF211E",
                    seq(0, 0.9, length = length(gradients)-1)), ~geom_ribbon(aes(ymin = ..1, ymax = ..2), alpha = ..4, fill = ..3, color = "transparent", size = 0))

breaks <- occupancy_rate_data %>% 
  group_by(year) %>% 
  filter(idx == min(idx)) %>% 
  distinct(idx, year) 

occ_plot <- ggplot(occupancy_rate_data, aes(x = idx, y = avg_rate)) +
  geom_segment(data = breaks, aes(x = idx, xend = idx, y = 0.075, yend = 1)) +
  geom_text(data = breaks, aes(y = 0.075, label = year), hjust = 0, vjust = 1.2, family = "Oswald") +
  geom_path() +
  shades +
  geom_ribbon(data = remainder, aes(ymin = 1, ymax = top), fill = "#BF211E", color = "white", size = 0) +
  geom_ribbon(aes(ymin = avg_rate, ymax = Inf), fill = "white", color = "white", size = 0) +
  geom_segment(aes(x = 0, xend = 1095, y = 1, yend = 1), color = "#BF211E", size = 0.5) +
  geom_text(data = labels, aes(label = toupper(sector)), hjust = 0, family = "Anton", size = 20) +
  facet_wrap(~ sector, nrow = 1) +
  labs(x = NULL,
       y = NULL,
       title = "Since 2017 Toronto Shelters Have Been Operating Near Or Over Capacity",
       subtitle = "Illustrated below is the occupancy rate (occupancy/capacity) for Toronto shelters across all sectors from 2017 to 2019.  Occupancy rates across all sectors have been slowly increasing over time, which highlights the need for added shelters and increased support in Canada's largest city.",
       caption = "**Data**: open.toronto.ca via {opendatatoronto} | **Graphic**: @jakekaupp") +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0.02), breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(expand = c(0,0), breaks = c(0, breaks$idx), labels = c("", "2017", "2018", "2019")) +
  theme_jk(grid = FALSE,
           markdown = TRUE,
           plot_title_family = "Anton",
           plot_title_size = 40) +
  theme(strip.text = element_blank(),
        axis.text.x = element_blank())

ggsave(here('2020', "week49", "tw49_plot.png"), occ_plot, width = 22, height = 10, dev = ragg::agg_png(res = 300))
