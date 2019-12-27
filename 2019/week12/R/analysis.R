library(tidyverse)
library(albersusa)
library(here)
library(sf)
library(maps)
library(glue)
library(jkmisc)

combined_data <- read_csv("https://raw.githubusercontent.com/5harad/openpolicing/master/results/data_for_figures/combined_data.csv")

data("county.fips")

states <- bind_cols(state = state.abb, name = state.name)

plot_data <- combined_data %>%
  mutate(location = tolower(location),
         location = str_remove(location, "county"),
         location = trimws(location)) %>% 
  left_join(states) %>% 
  mutate(polyname = glue("{tolower(name)},{location}")) %>% 
  left_join(county.fips) %>% 
  mutate(fips = str_pad(fips, 5, "left", "0")) %>% 
  select(driver_race, fips) %>% 
  left_join(counties_sf(), by = "fips") 

plot <- ggplot(plot_data) +
  geom_sf(data = counties_sf(), size = 0.1, fill = "#028090", color = "#e4fde1") +
  geom_sf(aes(geometry = geometry), fill = "white", size = 0.1, color = "#114b5f") +
  labs(x = NULL,
       y = NULL,
       title = "County-Level Availability of in Open Policing Data in the US.",
       subtitle = glue("Presented below is a choropleth map of the {highlight_text('missing county-level open policing data', '#028090', 'b')}.  Greater participation<br>in releasing policing data is a step towards transparency, resolving injustice and promoting accountability in<br>policing."),
       caption = "**Data:** Stanford Open Policing Project (arXiv:1706.05678) | **Graphic:** @jakekaupp") +
  theme_jk(grid = FALSE,
           markdown = TRUE) +
  theme(axis.text = element_blank())

ggsave(here("2019", "week12", "tw12_plot.png"), plot, width = 10, height = 6)
