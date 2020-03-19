library(tidyverse)
library(schrute)
library(tidygraph)
library(ggraph)
library(jkmisc)
library(glue)
library(here)

nodes <- theoffice %>% 
  distinct(season, episode, character) %>% 
  count(character, sort = TRUE) %>% 
  filter(n > 50)

ep_list <- theoffice %>% 
  distinct(season, episode, character) %>% 
  semi_join(nodes) %>% 
  mutate(temp = character) %>% 
  group_by(season, episode) %>% 
  summarize(to = list(toString(character)))

edges <- theoffice %>% 
  distinct(season, episode, character) %>% 
  semi_join(nodes) %>% 
  inner_join(ep_list) %>% 
  unnest(to) %>% 
  separate_rows(to, sep = ",") %>% 
  mutate(to = trimws(to)) %>% 
  rename(from = character) %>% 
  filter(from != to) %>% 
  count(from, to, name = "size") %>% 
  mutate(temp = ifelse(from > to, paste0(to, from), paste0(from, to))) %>% 
  distinct(temp, .keep_all = TRUE) %>% 
  select(-temp)

graph <- tbl_graph(nodes, edges)

important <- graph %>% 
  activate(nodes) %>% 
  mutate(degree = centrality_degree()) %>% 
  as_tibble() %>% 
  top_n(1, degree) %>% 
  pull(character)

edge_pos <- which(nodes$character == important)

out <- graph %>% 
  activate(nodes) %>% 
  mutate(color = if_else(character == important, "#17459D", "#646881"),
         stroke = if_else(character == important, 2, 0.5),
         alpha = if_else(character == important, 1, 0.5)) %>%
  activate(edges) %>% 
  mutate(edge_color = if_else(from == edge_pos | to == edge_pos, "#17459D", "#646881"),
         edge_alpha = if_else(from == edge_pos | to == edge_pos, 1, 0.7),
         edge_size = if_else(from == edge_pos | to == edge_pos, 2, 0.5)) %>% 
  ggraph(layout = "linear") + 
  geom_edge_arc(aes(edge_alpha = edge_alpha, edge_color = edge_color, edge_width = size), show.legend = FALSE) +
  geom_node_point(aes(fill = "white", color = color, stroke = stroke), shape = 21, size = 23) +
  geom_node_text(aes(label = character), family = "American Typewriter") +
  labs(x = NULL,
       y = NULL,
       title = glue("In *The Office*, {highlight_text('Andy', '#17459D', 'b')} is the Glue Binding The Main Characters Together"),
       subtitle = "Illustrated below is an arc diagram of the core cast of characters (more than 50 appearances) in *The Office*. Andy's importance was determined using degree centrality<br>for the presented characters.  The thicker the edge, the more episodes the connected characters appear in together.",
       caption = "**Data**: schrute package (bradlindblad.github.io/schrute) | **Graphic**: @jakekaupp") +
  scale_edge_width_continuous(range = c(0.5, 6)) +
  scale_edge_color_identity() +
  scale_color_identity() +
  scale_alpha_identity() +
  scale_fill_identity() +
  theme_jk(plot_title_family = "American Typewriter",
           subtitle_family = "American Typewriter",
           grid = FALSE,
           markdown = TRUE) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())

ggsave(here("2020", "week12", "tw12_plot.png"), out, width = 14, height = 10, dev = ragg::agg_png())
