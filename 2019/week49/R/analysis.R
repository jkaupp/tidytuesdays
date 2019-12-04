library(tidyverse)
library(osmdata)
library(here)
library(jkmisc)
library(nord)
library(colorspace)
library(glue)


philly_bb <- getbb("Philadelphia")

roads <- philly_bb %>% 
  opq() %>% 
  add_osm_feature("highway") %>% 
  osmdata_sf()

water <- philly_bb %>% 
  opq() %>% 
  add_osm_feature("water") %>% 
  osmdata_sf()

tickets <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-03/tickets.csv")

tickets_in <- filter(tickets, between(lon, philly_bb[1,1], philly_bb[1,2]), between(lat, philly_bb[2,1], philly_bb[2,2])) %>% 
  mutate_at(c("lat", "lon"), ~round(.x, 3))

map <- ggplot() +
  geom_sf(data = water$osm_multipolygons, fill = "#0077be", color = darken("#0077be")) +
  geom_sf(data = water$osm_polygons, fill = "#0077be", color = darken("#0077be")) +
  geom_sf(data = water$osm_lines, color = darken("#0077be")) +
  geom_bin2d(data = tickets_in, aes(x = lon, y = lat, fill = ..count..), binwidth = 0.001) +
  geom_sf(data = roads$osm_lines, color = nord_palettes$snowstorm[[3]], size = 0.05)  +
  coord_sf(xlim = philly_bb[1,], 
           ylim = philly_bb[2,],
           expand = FALSE) +
  scale_fill_viridis_c(option = "plasma") +
  labs(x = NULL,
       y = NULL,
       title = "P H I L A D E L P H I A,  P A",
       subtitle = glue("Illustrated below is a street level parking ticket heatmap of Philadelphia. Lighter colours indicate<br>{highlight_text('frequently ticketed areas', last(viridis::plasma(5)), 'b')} and darker colours indicate {highlight_text('areas ticketed less frequently', nth(viridis::plasma(5), 1), 'b')}."),
       caption = "**Data:** Open Data Philly | **Graphic:** @jakekaupp") +
  theme_jk(dark = TRUE,
           grid = FALSE,
           markdown = TRUE) +
  theme(axis.text = element_blank(),
        legend.position = "none")



ggsave(here("2019", "week49", "tw49_plot.png"), map, width = 8, height = 10, dev = ragg::agg_png(), dpi = 200)
