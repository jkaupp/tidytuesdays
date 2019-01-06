library(tidyverse)
library(here)
library(janitor)
library(albersusa)
library(sf)
library(sp)
library(rgeos)
library(maptools)
library(ggthemes)
library(viridis)
library(scales)
library(glue)
library(jkmisc)


census_data <- dir(here("week5", "data"), full.names = TRUE) %>% 
  read_csv() %>% 
  clean_names()

# Lets look at commuting!
commuting_data <- census_data %>% 
  select(census_id, state, county, total_pop, drive:mean_commute)

# Despacito is 3:47 in length
despacito_length <- 3 + 47/60

# Mapping things
us <- counties_composite()
us_map <- fortify(us, region="fips") 

# Calculate the how many despacitos the average commute has
despacito_commute <- commuting_data %>% 
  mutate(despacitos = mean_commute/despacito_length,
         id = ifelse(str_length(as.character(census_id)) < 5, glue("0{census_id}"), as.character(census_id))) %>% 
  right_join(us_map)


# Make the map!
map <- ggplot() +
 geom_map(data = us_map, map = us_map,
                    aes(x = long, y = lat, map_id = id),
                    color ="#2b2b2b", size = 0.05, fill = NA) +
  geom_map(data = despacito_commute, map = us_map,
           aes(x = long, y = lat, map_id = id, fill = despacitos),
           color ="#2b2b2b", size = 0.05) + 
  scale_fill_viridis(name="How many despactios?", na.value = viridis(5, option = "cividis")[3], option = 'cividis', breaks = seq(1,12,2)) +
  labs(title = "Just how much do you like your commute?",
       subtitle = str_wrap("What if your commute was defined by hearing a song on repeat?  
                           What if that song was the most streamed song on the planet, Despacito? 
                           Illustrated below is the average number of times you'd hear it on your way home across the US.", 80),
       caption = "Data: census.gov | Graphic: @jakekaupp") +
  coord_map() +
  theme_map(base_family="Scope One", 
            base_size = 16) +
  theme(legend.title = element_text(size = 10),
        plot.title = element_text(family = "Oswald"),
        legend.background = element_rect(fill = NA),
        plot.caption = element_text(size = 10),
        legend.position = c(0.9,0.1))

ggsave(here("week5", "tw5_choropleth map.png"), width = 10, height = 6)


