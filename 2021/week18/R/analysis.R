library(tidyverse)
library(rnaturalearth)
library(osmdata)
library(sf)

water <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-04/water.csv')

africa <- ne_countries(continent = "Africa", returnclass = "sf")

available_features()


waterways <- getbb("Uganda") %>% 
  opq() %>% 
  add_osm_feature("waterway") %>% 
  osmdata_sf()

ggplot() +
  geom_sf(data = filter(africa, soverint == "Uganda"),  fill = NA) +
  geom_sf(data = waterways$osm_lines, color = "red")


