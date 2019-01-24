library(tidyverse)
library(readxl)
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



incar_data <- here("2019","week4","data") %>% 
  dir(full.names = TRUE, pattern = "incarceration") %>% 
  read_excel()

fix_null <- function(x) if_else(is.nan(x), NA_real_, x)

# Mapping things
us <- counties_composite()
us_map <- fortify(us, region="fips") %>% 
  mutate_at("id", as.numeric)

ratio_data <- incar_data %>% 
  group_by(year, fips, state, county_name) %>% 
  transmute(black_pop_ratio = black_pop_15to64/total_pop_15to64,
         black_prison_ratio = black_prison_pop/total_prison_pop,
         asian_pop_ratio = asian_pop_15to64/total_pop_15to64,
         asian_prison_ratio = asian_prison_pop/total_prison_pop,
         latino_pop_ratio = latino_pop_15to64/total_pop_15to64,
         latino_prison_ratio = latino_prison_pop/total_prison_pop,
         native_pop_ratio = native_pop_15to64/total_pop_15to64,
         native_prison_ratio = native_prison_pop/total_prison_pop,
         white_pop_ratio = white_pop_15to64/total_pop_15to64,
         white_prison_ratio = white_prison_pop/total_prison_pop) %>% 
  group_by(fips, state, county_name) %>% 
  summarize_at(vars(contains("ratio")), mean, na.rm = TRUE) %>% 
  mutate_at(vars(contains("ratio")), fix_null)


map_data <- left_join(us_map, ratio_data, by = c("id" = "fips"))  %>% 
  ungroup() %>% 
  gather("variable","percentage", contains("ratio")) %>% 
  separate(variable, c("ethnicity", "category"), sep = "_")

plot <- ggplot() +
  geom_map(data = us_map, map = us_map,
           aes(x = long, y = lat, map_id = id),
           color ="#2b2b2b", size = 0.05, fill = NA) +
  geom_map(data = map_data, map = us_map,
           aes(x = long, y = lat, map_id = id, fill = percentage),
           color ="#2b2b2b", size = 0.05) + 
  scale_fill_viridis_c("", na.value = "white", option = 'cividis', labels = scales::percent) +
  coord_map() +
  labs(title = "Differences between the General and Prison Population by County and Ethnic Group from 1970 to 2016",
       subtitle = str_wrap("Non-white and non-Asian ethnic groups in the South-Eastern United States have a higher average representation in prison than in the overall population.  Missing data indicated by no fill color.",  90),
       caption = "Data: Vera Institute of Justice | Graphic: @jakekaupp") +
  facet_grid(category ~ ethnicity , labeller = labeller(category = c("pop" = "Total\nPopulation", "prison" = "Prison\nPopulation"),
                                                       ethnicity = str_to_title)) +
  theme_map(base_family = "Scope One", 
            base_size = 16) +
  theme(plot.caption = element_text(size = 10),
        plot.title = element_text(family = "Oswald"),
        legend.background = element_rect(fill = NA),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.position = "bottom",
        legend.justification = "center",
        legend.direction = "horizontal",
        legend.key.height = unit(0.2, "cm"),
        legend.key.width = unit(1, "cm"),
        strip.background = element_blank(),
        strip.text.y = element_text(angle = 0))

ggsave(here("2019","week4","tw4_choro.png"), width = 11, height = 5)

