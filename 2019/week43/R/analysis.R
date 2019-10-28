library(tidyverse)
library(lubridate)
library(here)
library(ggraph)
library(tidygraph)
library(glue)
library(jkmisc)
library(colorspace)
library(ggforce)
library(ggtext)

horror_movies <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-22/horror_movies.csv")

movie_cast <- distinct(horror_movies, title, release_date, review_rating, .keep_all = TRUE) %>% 
  mutate(year = str_extract(title, "\\((\\d{4})\\)"),
         year = parse_number(year),
         title = str_remove(title, "(\\s\\(\\d{4}\\))"),
         date = dmy(release_date)) %>% 
  arrange(title) %>% 
  separate_rows(cast, sep = "\\|") %>% 
  mutate(cast = trimws(cast)) %>% 
  select(title, year, review_rating, cast)

cast_df <- left_join(movie_cast, movie_cast, by = c("title", "year", "review_rating")) %>% 
  rename(from = cast.x,
         to = cast.y) %>% 
  filter(from != to) 

nodes <- cast_df %>% 
  group_by(from) %>% 
  summarize(node_size = n_distinct(title)) %>% 
  distinct(from, .keep_all = TRUE) 

focus <- "Eric Roberts"

edges <- cast_df %>% 
  count(from, to, sort = TRUE, name = "edge_size") %>% 
  distinct(from, to, .keep_all = TRUE) %>% 
  mutate(color = if_else(from == focus | to == focus, "#bb0a1e", "#373e40"),
         alpha = if_else(from == focus | to == focus, 1, 0.2),
         size = if_else(from == focus | to == focus, 1, 0.1))

connected <- filter(edges, from == focus) %>% 
  distinct() %>% 
  pull(to) 

cast_network <- tbl_graph(nodes = nodes, edges = edges, directed = FALSE) %>% 
  activate(nodes) %>% 
  mutate(degree = centrality_eigen(),
         alpha = if_else(from %in% c(focus, connected),  1, 0.2)) %>% 
  top_n(500, degree) %>% 
  mutate(fill = if_else(from %in% c(focus, connected), "#bb0a1e", "#373e40"),
         color = if_else(from %in% c(focus, connected), darken("#bb0a1e"), darken("#373e40")))

plot <- ggraph(cast_network, layout = "graphopt") + 
  geom_edge_link(aes(alpha = stat(index), edge_colour = color, edge_width = size), show.legend = FALSE) + 
  geom_node_point(aes(size = node_size, fill = fill, color = color), shape = 21, show.legend = FALSE) +
  #geom_mark_circle(aes(x, y, filter = from == focus, label = from, description = "Legendary B-Movie Actor"), expand = unit(0, "mm"), label.family = c("Oswald", "Lora")) +
  scale_edge_color_identity() +
  scale_alpha_identity() +
  scale_fill_identity() +
  scale_edge_width_identity() +
  scale_color_identity() +
  labs(x = NULL,
       y = NULL,
       title = "Horror Movie Co-Star Networks of the Top 500 Prolific Performers",
       subtitle = glue("The reach of B-movie legend {highlight_text('Eric Roberts', '#bb0a1e', 'b')} is featured below across his 27 films. Prolific performers determined by the top 500<br>actors by eigenvalue centrality."),
       caption = "Data: **IMDB** | Graphic: **@jakekaupp**") +
  theme_jk(grid = FALSE,
           markdown = TRUE) +
  theme(axis.text = element_blank())

ggsave(here("2019", "week43", "tw43_plot.png"), plot, width = 10, height = 6, device = ragg::agg_png())

  
