library(tidyverse)
library(here)
library(jkmisc)
library(glue)
library(patchwork)

full_trains <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-26/full_trains.csv")

plot_data <- full_trains %>% 
  filter(service == "National") %>% 
  group_by(month, departure_station) %>% 
  summarize_at(c("avg_delay_late_at_departure", "avg_delay_late_on_arrival"), mean, na.rm = TRUE) 
  
plot <- ggplot(plot_data) +  
  geom_hline(yintercept = seq(0, 100, 25), color = "grey80", size = 0.1) +
  geom_col(aes(x = month, y = avg_delay_late_on_arrival), position = position_identity(), alpha = 0.8, color = "#006B38FF", fill = "#006B38FF") +
  geom_col(aes(x = month, y = avg_delay_late_at_departure), position = position_identity(), alpha = 0.8, color = "#101820FF", fill = "#101820FF") +
  coord_polar() +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  ylim(-10, 100) +
  labs(x = NULL,
       y = NULL,
       title = glue("Delays in {highlight_text('Arrivals', '#006B38FF', 'b')} and {highlight_text('Departures', '#101820FF', 'b')} in France's National Routes by Station."),
       subtitle = "Illustrated below is a circular bar chart by month and station, showing the average delay from 2015 to 2018. Eeach Ring mark 25 minutes of delay. During this period, delays in arrivals were longer<br>than departures for the majority of statement.",
       caption = "**Data:** SNCF Open Data | **Graphic:** @jakekaupp") +
  facet_wrap(~departure_station,  ncol = 12, labeller = label_wrap_gen()) +
  theme_jk(grid = FALSE,
           strip_text_size = 8,
           markdown = TRUE) +
  theme(axis.text = element_blank())
  
ggsave(here("2019", "week9", "tw9_plot.png"), plot, width = 16, height = 8)
  
weeks <- here("2019") %>% 
  fs::dir_ls() %>% 
  str_extract("week\\d+") %>% 
  parse_number() %>% 
  sort()

setdiff(1:52, weeks)

