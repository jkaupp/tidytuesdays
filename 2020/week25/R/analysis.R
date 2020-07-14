library(tidyverse)
library(ggstream)
library(ggmap)
library(rnaturalearth)
library(sf)
library(here)
library(jkmisc)
library(scales)
library(ggtext)
library(glue)
library(magick)

blackpast <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/blackpast.csv')
slave_routes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/slave_routes.csv')

map <- ne_countries(type = 'countries', scale = 'medium', returnclass = "sf") %>% 
  filter(admin != "Antarctica")

us <- map %>% 
  filter(adm0_a3 == "USA") %>% 
  st_transform(crs = 4326)

canada <- map %>% 
  filter(admin == "Canada") %>% 
  st_transform(crs = 4326)

arcs <- slave_routes %>% 
  filter(across(c(place_of_purchase, port_arrival), ~!is.na(.x))) %>% 
  filter(port_arrival != "???") %>% 
  mutate(across(c(place_of_purchase, port_arrival), ~str_remove_all(.x, ", port unspecified"))) %>% 
  mutate(across(c(place_of_purchase, port_arrival), ~str_remove_all(.x, ", colony unspecified"))) %>% 
  mutate(across(c(place_of_purchase, port_arrival), ~str_remove_all(.x, ", location unspecified"))) %>%
  mutate(across(c(place_of_purchase, port_arrival), ~str_remove_all(.x, ", region unspecified"))) %>%
  mutate(across(c(place_of_purchase, port_arrival), ~str_remove_all(.x, ",\\s*unspecified"))) %>%
  mutate(across(c(place_of_purchase, port_arrival), ~str_remove_all(.x, "(colony unspecified)"))) %>%
  count(place_of_purchase, port_arrival, wt = n_slaves_arrived) %>% 
  filter(n != 0)
    
if (!file.exists(here("2020", "week25", "data", "geocodes.RDS"))) {
  
  geocodes <- tibble(address = c(unique(arcs$place_of_purchase), unique(arcs$port_arrival))) %>% 
    mutate(geo = geocode(address))
  
  geo_out <- tibble(address = c(unique(arcs$place_of_purchase), unique(arcs$port_arrival))) %>% 
    bind_cols(geocodes$geo)
  
  saveRDS(geo_out, here("2020", "week25", "data", "geocodes.RDS")) 
} else {
  
  geocodes <- readRDS(here("2020", "week25", "data", "geocodes.RDS"))
 
}

total <- comma(sum(slave_routes$n_slaves_arrived, na.rm = TRUE))

geo_trans <- geocodes %>% 
  filter(!is.na(lon) | !is.na(lat)) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  st_join(us, join = st_within) %>% 
  mutate(in_usa = !is.na(admin)) %>% 
  select(address, geometry, in_usa)
  
cdns <- geo_trans %>% 
  st_join(canada, st_within) %>% 
  filter(!is.na(admin)) 

points <- geo_trans %>% 
  st_coordinates() %>% 
  as_tibble() %>% 
  bind_cols(geo_trans, .) %>% 
  filter(!address %in% cdns$address)

arcs_final <- left_join(arcs, select(points, address, X, Y), by = c("place_of_purchase" = "address")) %>% 
  left_join(select(points, address, X, Y, in_usa), by = c("port_arrival" = "address")) %>% 
  select(-contains("geometry")) %>% 
  filter(X.x != X.y) %>% 
  distinct() %>% 
  filter(!port_arrival %in% cdns$address)


box <- make_bbox(points$X, points$Y, f = 0.1)

plot <- ggplot(map) +
  geom_sf(fill = "#eceff4", color = "grey15", size = 0.1) +
  geom_point(data = points, aes(x = X, y = Y), size = 0.1, color = "#A80303") +
  geom_curve(data = arcs_final, aes(x = X.x, y = Y.x, xend = X.y, yend = Y.y), alpha = 0.2, size = 0.2, color = "#A80303") +
  annotate(geom = 'text', x = -150, y = 8, label = "An Estimated", color = "#eceff4", family = "Oswald", size = 6, hjust = 0) +
  annotate(geom = 'text', x = -150, y = 0, label = glue("12,521,337"), color = "#A80303", family = "Anton", size = 10, hjust = 0) +
  annotate(geom = 'text', x = -150, y = -12, label = "Africans Enslaved\nFrom 1514 to 1875", color = "#eceff4", family = "Oswald", size = 6, hjust = 0) +
  annotate(geom = 'text', x = -50, y = -39, label = "Slave Routes: Tides of Inhumanity and Oppression", color = "#A80303", family = "Anton", size = 10, hjust = 0) +
  annotate(geom = 'text', x = -50, y = -47, label = "#J U N E T E E N T H #B L A C K L I V E S M A T T E R", color = "#eceff4", family = "Lato", size = 6, hjust = 0) +
  labs(x = NULL,
       y = NULL,
       caption = "**Data**: slavevoyages.org | **Graphic**: @jakekaupp") +
  coord_sf(crs = 4326, 
           xlim = c(box[1], box[3]),
           ylim = c(box[2], box[4])) +
  theme_jk(grid = FALSE,
           markdown = TRUE,
           dark = TRUE) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.background = element_rect(fill = "grey15"))

ggsave(here("2020", "week25", "tw25_plot.png"), plot, width = 16, height = 10)
