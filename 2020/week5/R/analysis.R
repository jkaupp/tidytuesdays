library(tidyverse)
library(lubridate)
library(here)
library(jkmisc)
library(ggraph)
library(ggforce)
library(paletteer)
library(colorspace)
library(glue)

set.seed(42)

sf_trees <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-28/sf_trees.csv')
        
trees <- sf_trees %>%
  mutate(year = year(date)) %>% 
  count(species) 

pos <- pack_circles(trees$n) %>% 
  as_tibble()

layout <- tibble(x = pos$V1,
                 y = pos$V2,
                 r = sqrt(trees$n/pi))


plot_data <- bind_cols(trees, layout)

fill_pal <- colorRampPalette(palettes_d$rcartocolor$Emrld)(571)
color_pal <- darken(colorRampPalette(palettes_d$rcartocolor$Emrld)(571))

trunk <- tibble(x = c(-25, -25, -50, 50, 24, 25),
                y = c(-200, -350, -400, -400, -350, -200))


top <- plot_data %>% 
  ungroup() %>% 
  top_n(4, n) %>% 
  separate(species, c("one", "species"), sep = "::") %>% 
  select(-one) %>% 
  mutate(species = trimws(species)) %>% 
  mutate(species = if_else(species == '', "Unknown Species", species)) %>% 
  arrange(desc(n))

tree <- ggplot(plot_data) +
  geom_shape(data = trunk, aes(x = x, y = y), fill = "#5e4a3a", color = darken("#5e4a3a")) +
  geom_circle(aes(x0 = x, y0 = y, r = r, fill = species, color = species), show.legend = FALSE, inherit.aes = FALSE) +
  geom_text(data = top, aes(x = x, y = y, label = glue("{str_wrap(species, 10)}\n{n} planted")), family = "Oswald", color = "white") +
  scale_fill_manual(values = sample(fill_pal, 571)) +
  scale_color_manual(values = sample(color_pal, 571)) +
  coord_equal() +
  labs(x = NULL,
       y = NULL,
       title = "Trees Planted in San Francicso 1955-2020",
       subtitle = "Illustrated below is a bubble chart showing the total number of trees planted in San Francisco<br>on city streets by species from 1955 to 2020.",
       caption = "**Data**: data.sfgov.org | **Graphic**: @jakekaupp") +
  theme_jk(grid = FALSE,
           markdown = TRUE) +
  theme(axis.text = element_blank())

ggsave(here("2020", "week5", "tw5_plot.png"), tree, width = 10, height = 10)
