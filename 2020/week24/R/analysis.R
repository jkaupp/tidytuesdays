library(tidyverse)
library(jkmisc)
library(here)
library(glue)
library(gggibbous)

science <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-09/science.csv')

plot_data <- science %>% 
  replace_na(list(death = 2020)) %>% 
  filter(!is.na(birth)) %>% 
  arrange(birth) %>% 
  mutate(name = factor(name, name)) %>% 
  mutate(ratio = if_else(death == 2020, 0, 0.8)) 
  
  
plot <- ggplot(plot_data) +
  geom_segment(aes(x = birth, xend = death, y = name, yend = name), size = 1) +
  geom_moon(aes(x = birth, y = name), ratio = 0.2, right = TRUE, fill = "#2E1F27", size = 3) +
  geom_moon(aes(x = birth, y = name), ratio = 0.8, right = FALSE, size = 3) +
  geom_moon(aes(x = death, y = name, ratio = ratio), right = TRUE, size = 3, fill = "#2E1F27") +
  geom_moon(data = filter(plot_data, death != 2020), aes(x = death, y = name, ratio = 1 - ratio), right = FALSE, size = 3) +
  geom_text(aes(x = birth, y = name, label = name), nudge_x = -5, hjust = 1, family = "Oswald", color = "grey40") +
  labs(x = NULL,
       y = NULL,
       title = "Celebrating the Lives and History of African-American Scientists, Inventors and Engineers",
       subtitle = "The timeline below documents the lives many of the African-Americans who have invented a multitude of items or made discoveries,<br>ranging from practical everyday devices to applications and scientific discoveries in diverse fields.",
       caption = "**Data**: wikipedia | **Graphic**: @jakekaupp") +
  theme_jk(grid = FALSE,
           axis = FALSE,
           ticks = TRUE,
           markdown = TRUE) +
  scale_x_continuous(limits = c(1680, 2020)) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.background = element_rect(fill = "#FCF7F8"),
        plot.title.position = "panel")

ggsave(here("2020", "week24", "tw24_plot.png"), plot, width = 10, height = 20)
