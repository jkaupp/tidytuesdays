library(tidyverse)
library(jkmisc)
library(ggtext)
library(ggrepel)
library(glue)
library(here)

landline <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-10/landline.csv')

plot_data <- landline %>% 
  filter(year %in% c(1990, 2017)) %>% 
  group_by(entity) %>% 
  filter(!any(is.na(landline_subs))) %>% 
  mutate(diff = diff(landline_subs)) %>% 
  mutate(color = if_else(diff >= -10, "#95a3b3", "#FA003F"),
         alpha = if_else(diff >= -10, 0.3, 1),
         size = if_else(diff >= -10, 0.5, 1))

plot <- ggplot(plot_data, aes(x = year, y = landline_subs)) +
  geom_path(aes(group = entity, color = color, alpha = alpha, size = size)) +
  geom_point(aes(group = entity, color = color, alpha = alpha)) +
  geom_text(data = filter(plot_data, year == last(year), size == 1) %>% arrange(landline_subs), aes(label = entity, color = color, x = 2017.5), hjust = 0, vjust = rev(c(0.5, 0.5, 0.5, 0, 0.5, 1, 0.5, 1, 0.5, 0.5, 0.5)), family = "Big Shoulders Display Bold") +
  scale_x_continuous(breaks = c(1990, 2017), limits = c(1990, 2019)) +
  scale_y_continuous(expand = c(0.01, 0)) +
  scale_color_identity() + 
  scale_alpha_identity() +
  scale_size_identity() +
  labs(x = NULL, 
       y = NULL,
       title = "Cord Cutting May Not Be As Widespread As We Thought",
       subtitle = glue("Shown below is a slopegraph comparing landline subscriptions per 100 people in 1990 and 2017. Only a few {highlight_text('countries saw a significant<br>drop', '#FA003F', 'b')} in landline subscriptions with {highlight_text('others showing stable or increased growth', '#95a3b3',  'b')}."),
       caption = "**Data**: OurWorldInData.org | **Graphic**: @jakekaupp") +
  theme_jk(base_family = "Big Shoulders Text Bold",
           plot_title_family = "Big Shoulders Text Bold",
           subtitle_family = "Big Shoulders Text",
           markdown = TRUE,
           grid = "XY") +
  theme(panel.grid.major =  element_line(linetype = "dashed", color = "grey50"))

ggsave(here("2020", "week46", "tw46_plot.png"), plot, width = 8, height = 16)

