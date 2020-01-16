library(tidyverse)
library(janitor)
library(here)
library(ggraph)
library(tidygraph)
library(paletteer)
library(jkmisc)
library(patchwork)


passwords <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-14/passwords.csv') %>% 
  remove_empty("rows") %>% 
  arrange(category, password)

root <- tibble(to = unique(passwords$category),
               from = "origin")

groups <- distinct(passwords, password, category) %>% 
  set_names(c("to", "from")) 

order <- groups %>% 
  count(from) %>% 
  pull(from) %>% 
  c(., NA)

edges <- bind_rows(root, groups)

nodes <- tibble(name = unique(c(edges$from, edges$to))) %>% 
  mutate(group = edges$from[match(name, edges$to)]) %>% 
  mutate(size = passwords$strength[match(name, passwords$password)],
         size = ifelse(group == "origin", NA, size)) %>% 
  mutate(group = ifelse(group == "origin", NA, group)) 

graph <- tbl_graph(edges = edges, nodes = nodes, directed = TRUE)

stem_labels <- create_layout(graph, layout = 'dendrogram', circular = TRUE) %>% 
  filter(leaf == FALSE) %>% 
  mutate(group = name,
         n = count(groups, from) %>% pull(n) %>% c(NA_real_, .)) %>% 
  slice(-1) %>% 
  mutate(percent = n/sum(n, na.rm = TRUE)) %>% 
  mutate(label = str_remove(name, "simple-")) %>%
  mutate(label = str_to_upper(str_replace_all(label, "(?<=.)(?!$)", " "))) %>% 
  mutate(label = ifelse(name == "simple-alphanumeric", "A L P H A -\nN U M E R I C", label))
  

big_plot <- ggraph(graph, layout = 'dendrogram', circular = TRUE) + 
  geom_edge_diagonal(colour  = "grey", alpha = 0.1) +
  geom_node_text(aes(x = x*1.3, y = y*1.3, filter = leaf, label = name,  colour = group, angle = -((-node_angle(x, y)+90)%%180)+90,  hjust = ifelse(between(node_angle(x,y), 90, 270), 0, 1)), size = 2.2, family = "Lora") +
  geom_node_text(aes(x = x*3, y = y*3, label = label, color = group, hjust = ifelse(between(node_angle(x,y), 90, 270), 1, 0)), size = 5, family = "Oswald", data = stem_labels)  +
  geom_node_text(aes(x = x*2.1, y = y*2.1, label = scales::percent(percent), color = group, hjust = ifelse(between(node_angle(x,y), 90, 270), 1, 0)), size = 5, family = "Oswald", data = stem_labels)  +
  geom_node_point(aes(filter = leaf, colour = group, alpha = 0.2)) +
  annotate("text", x = 0, y = 0, label = "B A D   P A S S W O R D S", family = "Oswald", size = 10, color = "white") +
  scale_colour_paletteer_d("vapoRwave::vapoRwave") +
  labs(x = NULL, 
       y = NULL,
       title = "Classification and Distribution of 500 of the Worst Passwords",
       subtitle = "Illustrated below as a donut-dendrogram hybrid are the 500 passwords, classified.  An unsurpising 37% of bad passwords are proper names, followed by 16% of<br>cool-macho type bad passwords. Time to change your password Maverick.", 
       caption = "**Data**: informationisbeautiful.net | **Graphic**: @jakekaupp") +
  theme_jk(dark = TRUE, 
           grid = FALSE,
           markdown = TRUE) +
  theme(legend.position="none",
        axis.text = element_blank()) +
  expand_limits(x = c(-1.5, 1.5), y = c(-1.5, 1.5)) +
  coord_equal() 


ggsave(here("2020", "week3", "tw3_plot.png"), plot = big_plot, width = 14.5, height = 14.5)


