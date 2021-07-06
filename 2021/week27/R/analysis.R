library(tidyverse)
library(jkmisc)
library(here)
library(sf)
library(magic)

animal_rescues <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-29/animal_rescues.csv')

wards <- st_read("https://opendata.arcgis.com/datasets/60ea78fd4f9d47099adfa63c2ccbc8bf_0.geojson")
regions <- st_read("https://opendata.arcgis.com/datasets/cfa25518ddd7408a8da5c27eb42dd428_0.geojson")


london <- filter(regions, RGN20NM == "London")

animal_by_wards <- animal_rescues %>% 
  mutate(animal_group_parent = str_to_title(animal_group_parent)) %>% 
  count(ward_code, animal_group_parent, sort = TRUE) %>% 
  filter(animal_group_parent %in% c("Cat", "Bird", "Dog")) %>% 
  ungroup() 

london_animals <- wards %>%
  left_join(animal_by_wards, by = c("WD20CD" = "ward_code")) %>% 
  filter(!is.na(animal_group_parent))

center <- st_coordinates(st_centroid(london$geometry))

st_buffer()

r7_cols <- list(
  orange = "#E85E26",
  blue = "#31739C",
  slate = "#3B454A",
  green = "#1CBB98",
  yellow = "#FFD349",
  aqua = "#21B3CD",
  gray = "#DEE5E8"
)

r7_ramp <- grDevices::colorRampPalette(c("#3B454A", "#31739C", "#21B3CD", "#FFD349", "#E85E26"))

scale_fill_r7c <- function(..., ramp_size = 10, na.value = "grey50",
                           guide = "colourbar", aesthetics = "fill") {
  continuous_scale(
    aesthetics, "r7c",
    scales::gradient_n_pal(r7_ramp(ramp_size), NULL, "Lab"),
    na.value = na.value, guide = guide, ...
  )
  
}

box <- st_bbox(london) %>% 
  as.matrix() 

labels <- tibble(x = rep(box[1,1], 3),
                 y = rep(box[2,1], 3),
                 animal_group_parent = c("Bird", "Cat", "Dog"))




plot <- ggplot(london_animals) +
  geom_point(aes(x = center[1], y = center[2]), size = 150, shape = 21, fill = "#007EA7", stroke = 2) +
  geom_sf(data = london$geometry, fill = "grey50", color = "white", size = 0.5) +
  geom_sf(aes(fill = n), size = 0.1, color = "grey10") +
  geom_text(data = labels, aes(x = x, y = y, label = toupper(paste0(animal_group_parent,"s"))), family = "Oswald", size = 14, hjust = 0, vjust = 1) +
  labs(x = NULL, 
       y = NULL,
       title = "Frequency of Rescues of Birds, Cats and Dogs in London from 2009-2021",
       subtitle = "Illustrated below in three choropleth maps are rescues of birds, cats and dogs in London wards.  Darker colors indicate lower rescue numbers while brighter colors indicate a greater<br>number of rescues in that ward.",
       caption = "**Data**: London.gov | **Graphic**: @jakekaupp") +
  scale_fill_r7c("Number of Rescues", ramp_size = 5) +
  facet_wrap(~animal_group_parent) +
  theme_jk(grid = FALSE,
           markdown = TRUE,
           plot_title_family = "Oswald",
           base_family = "Assistant",
           subtitle_family = "Assistant",
           caption_family = "Assistant",
           subtitle_size = 18,
           plot_title_size = 30) +
  theme(axis.text.x = element_blank(),
        legend.title = element_text(face = "bold"),
        axis.text.y = element_blank(),
        strip.text = element_blank()) +
  coord_sf(clip = "off")

ggsave(here("2021", "week27", "tw27_plot.png"), plot, width = 19, height = 8)


