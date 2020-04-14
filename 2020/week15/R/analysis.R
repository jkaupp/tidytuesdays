library(tidyverse)
library(rvest)
library(glue)
library(janitor)
library(here)
library(sf)
library(jkmisc)
library(colorspace)
library(ggmap)
library(ggtext)
library(magick)


scrape_stage <- function(url) {
 
   read_html(url) %>% 
    html_table(fill = TRUE) %>% 
    keep(~first(names(.x)) == "Stage") %>% 
    map(clean_names) %>% 
    first() %>% 
    select(one_of("stage", "date", "course", "distance", "winner")) %>% 
    filter(!is.na(stage))
}


if (!file.exists(here('2020', "week15", 'data', "tdf_stages.RDS"))) {
  
  get_stage <- possibly(slowly(scrape_stage, rate_delay(0.5)), NA)
  
  
  out <- tibble(url = glue("https://en.wikipedia.org/wiki/{1903:2019}_Tour_de_France"),
                year = 1903:2019) %>% 
    rowwise() %>% 
    mutate(table = list(get_stage(url))) %>% 
    ungroup() 
} else {
  
  out <- readRDS(here('2020', "week15", 'data', "tdf_stages.RDS"))  
  
}

plot_data <- filter(out, !is.na(table)) %>% 
  select(year, table) %>% 
  mutate(table = map(table, ~mutate(.x, stage = as.numeric(stage)))) %>% 
  unnest(table) %>% 
  filter(winner != "Rest day", !is.na(stage))

stages <- plot_data %>% 
  separate(course, c("start", "finish"), " to ") %>% 
  pivot_longer(start:finish, values_to = "place") %>% 
  distinct(year, place) 


if (!file.exists(here('2020', "week15", 'data', "geo_stages.RDS"))) {
  
  poss_geocode <- possibly(geocode, NA)
  
  geo_stages <- distinct(stages, place) %>% 
    rowwise() %>% 
    mutate(coords = poss_geocode(glue("{place}(Europe)"), output = "more"))
  
  saveRDS(geo_stages, here('2020', "week15", 'data', "geo_stages.RDS")) 
} else {
  
  geo_stages <- readRDS(here('2020', "week15", 'data', "geo_stages.RDS"))
  
}

geo_points <- tibble(place = pull(geo_stages, place),
       lon = geo_stages$coords$lon,
       lat = geo_stages$coords$lat)  

eu <- here("2020", "week15", "data", "eu", "european-union-countries.shp") %>% 
  read_sf() %>% 
  st_union()

sz <- here("2020", "week15", "data", "switzerland", "ch_100km.shp") %>% 
  read_sf() %>% 
  st_union()

france <- here("2020", "week15", "data", "france", "REGION_CARTO.shp") %>% 
  read_sf() %>% 
  st_simplify() %>% 
  st_union() 

map <- ggplot(eu) + 
  geom_sf(fill = "#565b5f", color = "#15171e", size = 0) +
  geom_sf(data = sz, fill = "#565b5f", color = "#15171e", size = 0) +
  geom_sf(data = france, fill = darken("#565b5f"), color = "#FFFFFF", size = 0.2) +
  geom_point(data = geo_points, aes(x = lon, y = lat), shape = 21, fill = "#FEC801", color = darken("#FEC801", 0.2), alpha = 1) +
  annotate(GeomRichText, x = -7.5, y = 47.2, label = glue("{highlight_text('M A P P I N G', '#FFFFFF', size = 50)}"), label.color = NA, fill = NA, family = "Roboto Condensed Light") +
  annotate(GeomRichText, x = -7.5, y = 46.3, label = glue("{highlight_text('L E', '#FEC801', 'b', size = 30)}"), label.color = NA, fill = NA, family = "Montserrat") +
  annotate(GeomRichText, x = -7.5, y = 45.2, label = glue("{highlight_text('T O U R', '#FEC801', 'b', size = 60)}"), label.color = NA, fill = NA, family = "Montserrat") +
  annotate(GeomRichText, x = 11, y = 40, label = "**Data**: Wikipedia | **Graphic**: @jakekaupp", label.color = NA, fill = NA, color = "white", family = "Montserrat") +
  annotate(GeomRichText, x = -7.4, y = 44.2, label = glue("Illustrated in this map are all {highlight_text('749<br>stage locations', '#FEC801', 'b', size = 14)} in the Tour de France"), label.color = NA, fill = NA, color = "white", family = "Montserrat") +
  theme_jk(grid = FALSE,
           markdown = TRUE) +
  coord_sf(clip = "on") +
  labs(x = NULL,
       y = NULL,
       title = NULL,
       subtitle = NULL,
       caption = NULL) +
  scale_x_continuous(limits = c(-10, 15)) +
  scale_y_continuous(limits = c(40, 55)) +
  theme(plot.background = element_rect(fill = "#15171e", color = "#15171e"), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank())

ggsave(here("2020", "week15", "tw15_plot.png"), map, width = 12, height = 10, device = ragg::agg_png())

image_read(here("2020", "week15", "tw15_plot.png"), "png") %>% 
  image_trim() %>% 
  image_write(path = here("2020", "week15", "tw15_plot.png"), format = "png")



