library(tidyverse)
library(lubridate)
library(here)
library(jkmisc)
library(patchwork)


bird_collisions <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-30/bird_collisions.csv")


plot_data <- bird_collisions %>%
  filter(locality == "CHI") %>% 
  mutate(month = month(date),
         year = year(date)) %>% 
  unite("binomial_name", genus, species, sep = " ") %>% 
  count(year, month, binomial_name) %>% 
  complete(nesting(year, binomial_name), month = 1:12, fill = list(n = 0)) %>% 
  group_by(year, binomial_name) %>% 
  mutate(percent = n/sum(n)) %>% 
  mutate(percent = ifelse(is.nan(percent), 0, percent))

petals <- plot_data %>% 
  filter(n != 0) %>% 
  split(list(.$year, .$month, .$binomial_name), drop = TRUE) %>% 
  map(~complete(.x, year, binomial_name, month = 1:12, fill = list(n = 0, percent = 0))) %>% 
  map(~geom_area(data = .x, aes(color = binomial_name), size = 0.2, alpha = 0.1))


base_plot <- ggplot(plot_data, aes(x = month, y = percent, fill = binomial_name)) +
  scale_x_continuous(labels = month.abb, breaks = 1:12) +
  scale_y_continuous(limits = c(0,1), breaks = c(0.5, 0.1)) +
  scale_fill_viridis_d("Year", option = "plasma", direction = 1) +
  scale_color_viridis_d(option = "plasma", direction = 1) +
  labs(x = NULL,
       y = NULL) +
  coord_polar(theta = "x", start = 0) +
  theme_jk(dark = FALSE, grid = "XY", plot_title_size = 14) +
  theme(axis.text.y = element_blank(),
        legend.position = "none")

out <- base_plot + petals + facet_wrap(~binomial_name, labeller = label_wrap_gen(10), nrow = 7)


ggsave(here("2019","week18", "test.png"), plot = out)

