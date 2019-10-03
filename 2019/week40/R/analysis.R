library(tidyverse)
library(sf)
library(tigris)
library(glue)
library(colorspace)
library(jkmisc)
library(ggforce)
library(ragg)
library(here)

# Get TidyTuesday data
pizza_datafiniti <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-01/pizza_datafiniti.csv") %>% 
  filter(province == "NY") %>% 
  distinct(name, latitude, longitude)

pizza_barstool <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-01/pizza_barstool.csv") %>% 
  distinct(name, latitude, longitude) %>% 
  filter(!is.na(latitude))

# Get all New York County road maps
counties <- c("New York County", "Kings County", "Bronx County", "Richmond County",  "Queens County")

roads_data <- map(counties, ~roads("NY", .x, class = "sf")) %>% 
  do.call(sf:::rbind.sf, .)

# Build plot colors as a named vector and as a tibble
plotcolors <- c('Other' = '#cccccc',
                'Ave' = '#59c8e5',
                'St' = '#fed032',
                'Tunl' = '#fed032',
                'Brg' = '#fed032',
                'N' = '#fed032',
                'S' = '#fed032',
                'E' = '#fed032',
                'W' = '#fed032',
                'Rd' = '#4cb580',
                'Dr' = '#0a7abf', 
                'Hwy' = '#ff9223', 
                'Plz' = '#ff9223',
                'Viaduct' = '#ff9223', 
                'Expy' = '#ff9223', 
                'Pkwy' = '#ff9223',
                'Thruway' = '#ff9223',
                'State Hwy' = '#ff9223',
                'State' = '#ff9223',
                'US Hwy' = '#ff9223',
                'Blvd'= '#2e968c')

pc_tibble <- tibble(street_type = names(plotcolors),
                    color = plotcolors)

# Assign street types to roads
roads <- roads_data %>% 
  filter(!is.na(RTTYP)) %>% 
  mutate(street_type = map_chr(FULLNAME, ~first(names(plotcolors)[str_which(.x, glue("{names(plotcolors)}\\b"))]))) %>% 
  mutate(street_type = if_else(str_detect(FULLNAME, "I-"), 'I-', street_type)) %>% 
  mutate(street_type = case_when(is.na(street_type) & MTFCC == "S1100" ~ 'Expy',
                                 is.na(street_type) & MTFCC == "S1200" ~ 'St',
                                 is.na(street_type) & !MTFCC %in% c("S1100", "S1200") ~ "Other",
                                 TRUE ~ street_type)) %>% 
  left_join(pc_tibble, by = "street_type")

# Get Counties shapefiles to determine which pizza places are in the areas I want
counties_sf <- counties("NY", class = "sf") %>% 
  filter(NAMELSAD %in% counties)

# Use st_intersects and filter to remove out of bounds pizza places
pizza_sf_df <- st_as_sf(pizza_datafiniti, coords = c("longitude", "latitude"), crs = st_crs(roads)) 
pizza_sf_bs <- st_as_sf(pizza_barstool, coords = c("longitude", "latitude"), crs = st_crs(roads)) 
  
  
# Filter to pizza places in the five boroughs
ny_pizza_df <-  filter(pizza_sf_df, map_lgl(st_intersects(pizza_sf_df, counties_sf), ~!is_empty(.x)))

ny_pizza_bs <-  filter(pizza_sf_bs, map_lgl(st_intersects(pizza_sf_bs, counties_sf), ~!is_empty(.x)))

ny_pizza <- sf:::rbind.sf(ny_pizza_bs, ny_pizza_df) %>% 
  distinct(geometry)

# Construct the color legend
legend <- pc_tibble %>% 
  filter(street_type %in% c("Other","Ave","St", "Rd", "Dr", "Hwy", "Blvd")) %>% 
  mutate(street_type = factor(street_type, levels = c("Other", "Ave", "Dr", "Rd", "Blvd", "St", "Hwy"), labels = c("Other", "Avenue", "Drive", "Road", "Boulevard ", "Street", "Highway"))) %>%
  arrange(street_type) %>% 
  mutate(x0 = seq(3, by = 4.5, length.out = 7),
         r = 1.75,
         y0 = 0) %>% 
  ggplot(aes(x0 = x0, y0 = y0, r = r)) +
  geom_circle(aes(fill = color, color = darken(color))) +
  geom_text(aes(label = street_type, x = x0, y = 0), family = "Lora", size = 3) +
  annotate("text", family = "Oswald", x = -2, y = 0, label = "Legend", size = 6) +
  scale_fill_identity() +
  scale_color_identity() +
  expand_limits(y = c(-0.5, 4),
                x = c(-4, 24)) +
  labs(x = NULL,
       y = NULL) +
  coord_equal(clip = "off") +
  theme_jk(grid = FALSE, plot_title_size = 30) +
  theme(panel.grid.major = element_line(colour = "transparent"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank()) 

legend_grob <- ggplotGrob(legend)

subtitle <- "Shown on this map are the roads and the pizza places of the Five Boroughs of New York City.  
Pizza places are distinct locations almagamated from the DataFiniti and Barstool datasets, and a represented by purple dots.  Manhattan is the most represented borough in the dataset, unsurprising given the relative population, and it being the home of the Teenage Mutant Ninja Turtles.
The map style of plotting the colored roads were inspired by Erin Davis (erdavis1 on github), and her series of circular maps of World Cities."

caption <- "Data: DataFiniti, Barstool, US Census Shapefiles\nGraphic: @jakekaupp"


# Plot the Street maps and Pizza place data
pizza_map <- ggplot() +
  geom_sf(data = filter(roads, street_type != "Other"), aes(color = color), size = 0.25) + 
  geom_sf(data = filter(roads, street_type == "Other"), aes(color = color), size = 0.35) + 
  geom_sf(data = ny_pizza, color = darken("#963484"), fill = "#963484", shape = 21, size = 2, alpha = 0.5) +
  annotate("text", label = "Pizza Places of the Five Boroughs", family = "Oswald", x = -74.3, y = 40.91, size = 6, hjust = 0) +
  annotate("text", family = "Lato", label = str_wrap(subtitle, 60), x = -74.3, y = 40.89, hjust = 0, vjust = 1) +
  annotate("text", family = "Lato", label = caption, x = -74.3, y = 40.78, hjust = 0, vjust = 1) +
  annotation_custom(legend_grob, xmin = -74.2, xmax = Inf, ymin = 40.49, ymax = 40.55) +
  scale_color_identity() +
  scale_size_identity() +
  coord_sf(clip = "off") +
  labs(x = NULL, y = NULL) +
  theme_jk(grid = FALSE) +
  theme(panel.grid.major = element_line(colour = "transparent"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank()) 


ggsave(here('2019', 'week40', 'tw40_plot.png'), width = 12, height = 9, dev = agg_png())