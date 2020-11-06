library(tidyverse)
library(osmdata)
library(sf)
library(rnaturalearth)
library(jkmisc)
library(here)
library(colorspace)
library(ggtext)
library(glue)
library(magick)

wind_turbine <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-27/wind-turbine.csv')


maritimes_turbines <- filter(wind_turbine, province_territory %in% c("Nova Scotia", "New Brunswick", "Prince Edward Island")) %>% 
  mutate(height_bin = cut(hub_height_m, c(0, 50, 75, 100, Inf), labels = c("0-50m", "51-75m", "76-100m", "100+m"), include.lowest = FALSE))


power <- maritimes_turbines %>% 
  group_by(project_name) %>% 
  summarize(total_project_capacity_mw = unique(total_project_capacity_mw),
            latitude = mean(latitude),
            longitude = mean(longitude))

maritimes <- ne_states("Canada", returnclass = "sf") %>% 
  filter(name %in% c("Nova Scotia", "New Brunswick", "Prince Edward Island"))



mt_box <- st_bbox(maritimes) 

maritimes_bbox <- matrix(c(mt_box[1], mt_box[3], mt_box[2], mt_box[4]), nrow = 2, ncol = 2)

mt_hwy <- maritimes_bbox %>% 
  opq(timeout = 6000) %>% 
  add_osm_feature(key = "highway", 
                  value = c("primary", "secondary", "tertiary", "secondary_link", "tertiary_link")) %>% 
  osmdata_sf()

mt_hwy_clipped <- st_intersection(mt_hwy$osm_lines, maritimes)


plot <- ggplot() +
  geom_sf(data = maritimes, inherit.aes = FALSE, color = darken("#2F394D"), fill = "#F2EFEA") +
  geom_sf(data = mt_hwy_clipped, inherit.aes = FALSE, color = "#403d58", size = 0.1) +
  geom_point(data = maritimes_turbines, aes(x = longitude, y = latitude), fill = "#F35B04", color = "#F18701", shape = 21, alpha = 0.4, size = 3) +
  annotate("text", label = "Wind Power Generation in the Maritimes", x = -64.3, y = 48, family = "Francois One", color = "#F2EFEA", size = 9, hjust = 0) +
  annotate("richtext", label = glue("Shown below are the {highlight_text('wind turbines', '#F18701', 'b', size = 15)} built from 2001-2018 as part of larger wind farm prpjects in<br>Nova Scotia, New Brunswick and Prince Edward Island, also called the Maritimes or the<br>Maritime Provinces of Canada."), x = -64.3, y = 47.65, family = "Francois One", color = "#F2EFEA", size = 4, fill = NA, label.colour = NA, hjust = 0) +
  annotate("text", label = "N O V A  S C O T I A", x = -62.3, y = 44.5, family = "Francois One", color = "#F2EFEA") +
  annotate("text", label = "P R I N C E   E D W A R D  I S L A N D", x = -62, y = 47, family = "Francois One", color = "#F2EFEA", hjust = 1) +
  annotate("text", label = "N E W  B R U N S W I C K", x = -67, y = 44.9, family = "Francois One", color = "#F2EFEA") +
  labs(x = NULL,
       y = NULL,
       caption = "**Data**: Government of Canada | **Graphic**: @jakekaupp") +
  scale_size_area(max_size = 6) +
  theme_jk(grid = FALSE,
           markdown = TRUE) +
  theme(plot.background = element_rect(fill = "#2F394D", color = NA),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.caption = element_markdown(color = "#F2EFEA"))

ggsave(here("2020", "week44", "tw44_plot.png"), plot, width = 16, height = 10, dev = ragg::agg_png())

here("2020", "week44", "tw44_plot.png") %>% 
  image_read() %>% 
  image_trim() %>% 
  image_write(here("2020", "week44", "tw44_plot.png"))


