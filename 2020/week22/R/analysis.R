library(tidyverse)
library(here)
library(glue)
library(jkmisc)
library(tidygraph)
library(igraph)
library(ggraph)
library(graphlayouts)
library(nord)
library(colorspace)
library(patchwork)

cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/cocktails.csv')

edges <- cocktails %>%
  filter(category == "Cocktail") %>% 
  mutate(across(c(drink, ingredient), .fns = tolower)) %>% 
  distinct(to = drink, from = ingredient)  

edges <- split(edges, edges$from) %>%
  keep(~nrow(.x) > 1) %>% 
  bind_rows()


nodes <- tibble(name = c(unique(edges$from))) %>% 
  mutate(type = 'ingredient') %>% 
  bind_rows(tibble(name = edges$to)) %>% 
  replace_na(list(type = "cocktail"))
  
graph <- tbl_graph(nodes, edges, directed = FALSE) %>% 
  activate(nodes) %>% 
  filter(!node_is_isolated()) 
 
gin <- ggraph(graph, layout = "focus", focus = 17) +
  draw_circle(col = "#4F93B8", use = "focus", max.circle = 6) +
  geom_edge_link(aes(edge_alpha = ..index..), edge_colour = "white") +
  geom_node_point(aes(fill = type, color = type), shape = 21, size = 3) +
  annotate('text', x = -6, y = -5, color = "#D8DEE9", family = "Oswald Bold", label = "GIN", size = 20, hjust = 0) +
  scale_fill_manual(values = c('ingredient' = "#D8DEE9", 'cocktail' = "#F0C808")) +
  scale_color_manual(values = set_names(darken(c("#D8DEE9", "#F0C808")), c('ingredient', 'cocktail'))) +
  labs(x = NULL,
       y = NULL) +
  coord_fixed() +
  theme_jk(dark = TRUE,
           grid = FALSE)+
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.background = element_rect(color = NA))

vodka <- ggraph(graph, layout = "focus", focus = 44) +
  draw_circle(col = "#4F93B8", use = "focus", max.circle = 6) +
  geom_edge_link(aes(edge_alpha = ..index..), edge_colour = "white") +
  geom_node_point(aes(fill = type, color = type), shape = 21, size = 3) +
  annotate('text', x = -6, y = -5, color = "#D8DEE9", family = "Oswald Bold", label = "VODKA", size = 20, hjust = 0) +
  scale_fill_manual(values = c('ingredient' = "#D8DEE9", 'cocktail' = "#F0C808")) +
  scale_color_manual(values = set_names(darken(c("#D8DEE9", "#F0C808")), c('ingredient', 'cocktail'))) +
  labs(x = NULL,
       y = NULL) +
  coord_fixed() +
  theme_jk(dark = TRUE,
           grid = FALSE)+
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.background = element_rect(color = NA))

oj <- ggraph(graph, layout = "focus", focus = 33) +
  draw_circle(col = "#4F93B8", use = "focus", max.circle = 6) +
  geom_edge_link(aes(edge_alpha = ..index..), edge_colour = "white") +
  geom_node_point(aes(fill = type, color = type), shape = 21, size = 3) +
  annotate('text', x = -5.9, y = -3.8, color = "#D8DEE9", family = "Oswald Bold", label = "ORANGE", size = 5, hjust = 0) +
  annotate('text', x = -6, y = -5, color = "#D8DEE9", family = "Oswald Bold", label = "JUICE", size = 20, hjust = 0) +
  scale_fill_manual(values = c('ingredient' = "#D8DEE9", 'cocktail' = "#F0C808")) +
  scale_color_manual(values = set_names(darken(c("#D8DEE9", "#F0C808")), c('ingredient', 'cocktail'))) +
  labs(x = NULL,
       y = NULL) +
  coord_fixed() +
  theme_jk(dark = TRUE,
           grid = FALSE) +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.background = element_rect(color = NA))


plot <- vodka + gin + oj + plot_annotation(title = "Top 3 Ingredients in Cocktails Come As No Surprise to Anyone that Drinks Cocktails",
                                   subtitle = glue("Illustrated below is an concentrical layout undirected network of {highlight_text('cocktails','#F0C808', 'b')} and their {highlight_text('ingredients','#D8DEE9', 'b')}, focusing on the top 3."),
                                   caption = "**Data**: kaggle via @geomkaramanis | **Graphic**: @jakekaupp",
                                   theme = theme_jk(dark = TRUE,
                                                    markdown = TRUE))

ggsave(here("2020", "week22", "tw22_plot.png"), plot, width = 16, height = 10)

magick::image_read(here("2020", "week22", "tw22_plot.png")) %>% 
  magick::image_crop() %>% 
  magick::image_write(here("2020", "week22", "tw22_plot.png"))

                                   