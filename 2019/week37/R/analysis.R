library(tidyverse)
library(lubridate)
library(here)
library(tidygraph)
library(ggraph)
library(ggforce)
library(janitor)
library(jkmisc)
library(glue)
library(ggtext)
library(colorspace)
library(ragg)

legacy_data <- here("2019", "week37", "data", "Saferparks-dataset-legacy.csv") %>% 
  read_csv() %>% 
  mutate(year = year(mdy(acc_date))) %>% 
  filter(between(year, 1999, 2007))

device_type <- legacy_data %>% 
  filter(industry_sector != "unknown") %>% 
  count(industry_sector, device_category, device_type, name = "size") %>% 
  mutate(color = case_when(industry_sector == "amusement ride" ~ lighten("#251351", .75),
                           industry_sector == "recreation" ~ lighten("#7d2e68", .75),
                           industry_sector == "water park" ~ lighten("#41658a", .75))) %>% 
  select(name = device_type, size, color)

device_category <- legacy_data %>% 
  filter(industry_sector != "unknown") %>% 
  count(industry_sector, device_category, name = "size") %>% 
  mutate(color = case_when(industry_sector == "amusement ride" ~ lighten("#251351", .25),
                           industry_sector == "recreation" ~ lighten("#7d2e68", .25),
                           industry_sector == "water park" ~ lighten("#41658a", .25))) %>% 
  select(name = device_category, size, color) 

sector <-  legacy_data %>% 
  filter(industry_sector != "unknown") %>% 
  count(industry_sector, name = "size") %>% 
  mutate(color = case_when(industry_sector == "amusement ride" ~ "#251351",
                           industry_sector == "recreation" ~ "#7d2e68",
                           industry_sector == "water park" ~"#41658a")) %>% 
  select(name = industry_sector, size, color)

nodes <- bind_rows(sector, device_category, device_type)

edge_one <- legacy_data %>% 
  filter(industry_sector != "unknown") %>%
  select(industry_sector, device_category) %>% 
  mutate_all(~as.numeric(factor(., nodes$name))) %>% 
  set_names(c("from", "to"))

edge_two <- legacy_data %>% 
  filter(industry_sector != "unknown") %>%
  select(device_category, device_type) %>% 
  mutate_all(~as.numeric(factor(., nodes$name))) %>% 
  set_names(c("from", "to"))

edges <- bind_rows(edge_one, edge_two)

graph <- tbl_graph(nodes = nodes, edges = edges) 

labels <- legacy_data %>% 
  filter(industry_sector != "unknown") %>% 
  count(industry_sector, device_category, device_type, name = "size") %>% 
  group_by(industry_sector) %>% 
  top_n(1 , size) %>% 
  filter(size > 1) %>% 
  pull(device_type) 
 
text_bc <- function(text, color) {
  
  glue("<span style = color:{color}>**{text}**</span>")
  
}


plot <- ggraph(graph, 'circlepack', weight = size) + 
  geom_node_circle(aes(fill = color)) + 
  geom_node_text(aes(label = glue("{str_remove(name, ' - undefined')}:\n{size}"), filter = name %in% labels, family = "Oswald")) +
  scale_fill_identity() +
  labs(x = NULL,
       y = NULL,
       title = "Attractions With The Most Reported Injuries from 1999-2007",
       caption = "Data: **SaferParks** | Graphic: **@jakekaupp**",
       subtitle = glue("Shown below is a packed circle representation of reported accidents in the SaferParks database from 1999-2007.<br>Circles are organized by {highlight_text('Amusement rides', '#251351', 'b')}, {highlight_text('Recreation', '#7d2e68', 'b')} and {highlight_text('Water Park', '#41658a', 'b')}. Device category and device type are the<br>middle and lightest hues, respectively.")) +
  theme_jk(grid = FALSE) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.subtitle = element_markdown(),
        plot.caption = element_markdown()) 


ggsave(here("2019", "week37", "tw_37plot.png"), plot, width = 9, height = 10, dev = agg_png())


  