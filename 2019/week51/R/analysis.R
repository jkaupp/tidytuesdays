library(tidyverse)
library(jkmisc)
library(nominatim)
library(glue)
library(here)
library(ggvoronoi)
library(ggtext)


dog_descriptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_descriptions.csv')
  

slowly_geocode <- slowly(~osm_search(glue("{city = .x}, USA")), rate = rate_delay())

if (file.exists(here("2019","week51", "data", "geolocated.RDS"))) {
  
  geocoded_loc <- readRDS(here("2019","week51", "data", "geolocated.RDS"))
  
} else {
  
  geocoded_loc <- dog_descriptions %>%
    distinct(contact_city) %>%
    mutate(geocoded = map(contact_city, slowly_geocode))
  
  geocoded_loc %>% 
    unnest(cols = "geocoded") %>% 
    write_rds(here("2019","week51", "data", "geolocated.RDS")) 
}


usa <- map_data("usa")

plot_data <- dog_descriptions %>% 
  inner_join(geocoded_loc, by = "contact_city") %>% 
  filter(str_detect(contact_state, "[A-Z]{2}"), contact_state %in% state.abb) %>% 
  mutate(age = factor(age,  c("Baby", "Young", "Adult", "Senior")),
         sex = fct_explicit_na(factor(sex,  c("Male", "Female"))),
         size = factor(size, c("Small", "Medium", "Large", "Extra Large"))) %>% 
  select(contact_state, age, lat, lon) %>% 
  add_count(contact_state, age, name = "age_n") 


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



plot <- plot_data %>%
  distinct(lon, lat, .keep_all = TRUE) %>%
  ggplot(aes(x = lon, y = lat)) +
  geom_voronoi(aes(fill = age_n, group = age), size = 0.25, color = "#3B454A", outline = usa) +
  coord_map(projection = 'albers', lat0 = 29.5, lat1 = 45.5) +
  labs(x = NULL,
       y = NULL,
       title = "In Adoption It Doesn't Pay Well To Be An Old Dog, Even With New Tricks.",
       subtitle = str_wrap("Shown below is a choropleth map with Voronoi tesselation of adoption shelters across the US, broken into small multiple by age category. Colour indicates the number of dogs available for adoption in each area.  We can see that adult dogs are the largest group in dogs waiting to be adopted, and where the majority of them are across the country.", 120),
       caption = "**Data:** Petfinder via The Pudding | **Graphic:** @jakekaupp") +
  facet_wrap(~age, nrow = 1) +
  scale_fill_r7c("Number of Dogs") +
  theme_jk(grid = FALSE) +
  theme(axis.text = element_blank(),
        legend.position = "bottom",
        plot.caption = element_markdown(),
        legend.key.height = unit(2, "mm"))
           
ggsave(here("2019", "week51", "tw51_plot.png"), plot = plot, width = 10, height = 4, dev = ragg::agg_png())
