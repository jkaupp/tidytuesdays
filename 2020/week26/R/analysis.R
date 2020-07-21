library(tidyverse)
library(sf)
library(jkmisc)
library(lubridate)
library(here)
library(scales)
library(patchwork)
library(glue)

locations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-23/locations.csv')

empty <- st_as_sfc("POINT(EMPTY)") 

if (!file.exists(here("2020", "week26", "data"))) {
  
  distances <- locations %>% 
    st_as_sf(coords = c( "longitude", "latitude" ), crs = 4326) %>% 
    group_by(animal_id) %>%
    arrange(animal_id, timestamp) %>% 
    mutate(distance = st_distance(geometry, 
                                  lead(geometry),
                                  by_element = TRUE))
  
} else {
  distances <- readRDS(here("2020", "week26", "data", "distances.RDS"))
}


seasons <- distances %>% 
  as_tibble() %>% 
  group_by(year = year(timestamp), season) %>% 
  filter(year > 2000, timestamp == min(timestamp) | timestamp == max(timestamp)) %>% 
  ungroup() %>% 
  distinct(season, timestamp, year) %>% 
  arrange(year, season, timestamp) %>% 
  group_by(year, season) %>% 
  mutate(idx = c('start', "end")) %>% 
  pivot_wider(names_from = idx, values_from = timestamp)
         
total_distances <- distances %>% 
  group_by(animal_id) %>% 
  mutate(cum_dist = as.numeric(cumsum(distance))) %>% 
  ungroup() %>% 
  as_tibble() %>% 
  filter(year(timestamp) > 2000) %>% 
  distinct(animal_id, timestamp, .keep_all = TRUE) 

counts <- total_distances %>% 
  distinct(study_site, animal_id) %>% 
  count(study_site)

total_km <- total_distances %>% 
  group_by(study_site) %>% 
  summarize(total_km = sum(distance, na.rm = TRUE)) %>% 
  left_join(counts) %>% 
  mutate(avg_km = total_km/n) %>% 
  arrange(avg_km) %>% 
  mutate(color = colorRampPalette(c("#FEDAD7","#fb5a4b"))(8))

total_distances <- inner_join(total_distances, total_km)

movement <- locations %>% 
  mutate(
    season = fct_rev(season), # reverse seasons
    longitude = round(longitude, 2), # round long, lat to reduce number of points
    latitude = round(latitude, 2)
  ) %>% 
  distinct(study_site, longitude, latitude) %>% 
  left_join(total_km)

# For step Graph ----

background <- total_distances %>% 
  split(.$study_site) %>% 
  imap_dfr(~bind_rows(filter(total_distances, study_site != .y)) %>% 
             mutate(study_site = .y))
             
labels <- total_distances %>% 
  distinct(study_site, color) %>% 
  mutate(timestamp = as_datetime("2000-01-01 00:00:01 UTC"),
         cum_dist = 3100000) 

lines <- tibble(timestamp = as_datetime("2000-01-01 00:00:01 UTC"),
                cum_dist = seq(0, 3000000, 1000000)) %>% 
  mutate(label = number(cum_dist, suffix = " km", scale = 1/1000)) %>% 
  filter(cum_dist != 0) 

facet_lines <- tibble(study_site = c("Burnt Pine", "Moberly"),
       data = list(lines)) %>% 
  unnest(cols = c(data))
 
# step graph of daily distance travelled ----
step <- ggplot() +
  geom_step(data = background, aes(x = timestamp, y = cum_dist, group = animal_id), color = "#f4f4f5", alpha = 0.1) +
  geom_step(data = total_distances, aes(x = timestamp, y = cum_dist, group = animal_id, color = color)) +
  geom_text(data = labels, aes(x = timestamp, y = cum_dist, label = toupper(study_site), color = color), family = "Anton", size = 8, hjust = 0, vjust = 0) +
  geom_text(data = facet_lines, aes(x = timestamp, y = cum_dist, label = label), family = "Oswald", size = 3, color = "white", hjust = 0.25, vjust = 1.2) +
  scale_y_continuous(labels = number_format(suffix = " km", scale = 1/1000), expand = c(0,0)) +
  scale_x_datetime(date_labels = "%Y", limits = c(as_datetime("2000-01-01 00:00:01 UTC"), as_datetime("2017-01-01 00:00:01 UTC"))) +
  scale_color_identity() +
  labs(x = NULL,
       y = NULL) +
  facet_wrap(~study_site, nrow = 2, labeller = as_labeller(toupper)) +
  theme_jk(grid = "Y", 
           dark = TRUE) +
  theme(strip.text = element_blank(),
        axis.text.y = element_blank(),
        plot.background = element_rect(fill = "#2E3440", color = "#2E3440"))

# Map----

map <- ggplot(movement) +
  geom_point(aes(x = longitude, y = latitude, group = study_site, color = color), size = 0.1) +
  scale_color_identity() + 
  labs(x = NULL,
       y = NULL) +
  theme_jk(grid = FALSE, 
           dark = TRUE) +
  theme(strip.text = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        plot.background = element_rect(fill = "#2E3440", color = "#2E3440"))

# Bar ---

bar <- total_km %>% 
  mutate(idx = row_number()) %>% 
  ggplot(aes(x = idx, y = 1, fill = color)) +
  geom_tile() +
  geom_text(aes(x = 0, y = 1, label = "Lower\nMigration"), family = "Anton", color = "#f4f4f5", size = 3) +
  geom_text(aes(x = 9, y = 1, label = "Higher\nMigration"), family = "Anton", color = "#f4f4f5", size = 3) +
  scale_fill_identity() +
  labs(x = NULL,
       y = NULL) +
  coord_equal() +
  theme_jk(grid = FALSE, 
           dark = TRUE) +
  theme(strip.text = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        plot.background = element_rect(fill = "#2E3440", color = "#2E3440"))
  
stack <- wrap_plots(map, bar, nrow = 2 , heights = c(0.9, 0.1)) 

out <- wrap_plots(stack, step, nrow = 1, widths = c(0.33, 0.67)) +  
  plot_annotation(title = "In British Columbia the Caribou Travel Thousands of Kilometers Migrating Between Winter and Summer Ranges",
                  subtitle = str_break(glue("Caribou heards located farthest away from the central valleys face larger migrations between summer and winter habitats than other herds, as illustrated by the step graph below. The indiviudal step lines represent a single caribou in the herd, {highlight_text('colored lines', '#fb5a4b', 'b')} represent the specific herd and {highlight_text('white lines', '#FFFFFF', 'b')} represent caribou across all herds. Hue on both the map and step graph indicates the average distance travelled by each herd."), 280),
                  caption = "**Data**: movebank.org | **Graphic**: @jakekaupp",
    theme = theme_jk(dark = TRUE,
                     plot_title_family = "Anton",
                     markdown = TRUE)) 

ggsave(here('2020', 'week26', 'tw26_plot.png'), out, width = 20, height = 10)

