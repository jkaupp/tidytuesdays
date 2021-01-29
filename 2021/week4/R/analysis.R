library(tidyverse)
library(jkmisc)
library(here)


source(here("2021", "week4", "R", "packed_bars.R"))

plastics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-26/plastics.csv')

summary_data <- plastics %>% 
  filter(!parent_company %in% c("Grand Total", "Unbranded", "null", "NULL", "Assorted")) %>% 
  group_by(parent_company) %>% 
  summarize(across(empty:grand_total, ~sum(.x, na.rm = TRUE))) %>% 
  arrange(desc(grand_total)) %>% 
  pivot_longer(empty:grand_total)



plot_data <- summary_data %>% 
  filter(name == "grand_total") %>% 
  pack_bars(number_rows = 10, value_column = value, fill_color = "#FE9920")

plot <- ggplot(plot_data) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill, color = colorspace::darken(fill)), size = 0.05) +
  geom_text(data = filter(plot_data, fill == "#FE9920"), aes(x = xmin, y = (ymin + ymax)/2, label = toupper(parent_company)), family = "Rockwell", color = "white", nudge_x = 100, hjust = 0) +
  scale_fill_identity() +
  scale_color_identity() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(labels = scales::comma,
                     breaks = seq(0,30000,5000),
                     expand = c(0,0)) +
  labs(x = "Number of Plastic Items Recovered",
       y = NULL,
       title = "Less 'Share a Coke', More 'Recycle Your Plastic Waste'.",
       subtitle = "Shown below in a packed bar chart are the top 10 companies whose plastic was picked up all over the world in clean up efforts in 2019 and 2020.",
       caption = "**Data**: Break Free From Plastic | **Graphic**: @jakekaupp") +
  theme_jk(dark = TRUE,
           grid = FALSE,
           markdown = TRUE,
           plot_title_family = "Rockwell",
           axis_title_family = "Rockwell",
           subtitle_family = "Rockwell",
           base_family = "Rockwell") +
  theme(axis.text.y = element_blank(),
        plot.title.position = "panel")

ggsave(here("2021", "week4", "tw4_plot.png"), plot, device = ragg::agg_png(), width = 14, height = 7)
