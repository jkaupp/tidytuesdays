library(tidyverse)
library(here)
library(ggforce)
library(jkmisc)
library(sf)
library(osmdata)

full_trains <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-26/full_trains.csv")

available_tags("railway")

railways <- st_read(here("2019", "week9", "data", "railways.shp"))

q <- getbb("fr") %>%
  opq(timeout=25*1000)%>%
  add_osm_feature("railway")

stations <- osmdata_sf(q)

ggplot(stations$osm_lines) +
  geom_sf()

railways$geometry[[1]] %>% st_coordinates() %>% as_tibble -> line

ggplot(line, aes(x = X, y = Y)) +
  geom_link2() +
  coord_sf(datum=NA)

nat_trains <- full_trains %>% 
  filter(service == "National") %>% 
  group_by(year, departure_station, arrival_station) %>% 
  summarize_at(vars(journey_time_avg, total_num_trips, avg_delay_late_at_departure, avg_delay_late_on_arrival), mean, na.rm = TRUE)


# Orbit test

centre <- "PARIS LYON"

test_data <- filter(nat_trains, departure_station == centre | arrival_station == centre) %>% 
  arrange(departure_station)



positions <- test_data %>% 
  filter(arrival_station == centre) %>% 
  group_by(arrival_station, departure_station) %>% 
  summarize(dist = mean(journey_time_avg)) 


circles <- test_data %>% 
  group_by(departure_station) %>% 
  summarize(centre_radius = mean(avg_delay_late_at_departure)) %>% 
  left_join(positions)

main <- circles %>% 
  filter(departure_station == centre)

circles <- circles %>% 
  filter(departure_station != centre) %>% 
  mutate(fraction = nrow(.) - (nrow(.) - seq_along(departure_station)),
         delta = 360/nrow(.)*fraction) %>% 
  bind_rows(main) %>% 
  mutate(x0 = if_else(departure_station == centre, 0, dist*cos((delta*pi/180))),
         y0 = if_else(departure_station == centre, 0, dist*sin((delta*pi/180)))) 


link_coords <- function(dept, arr, lnk) {
  
  circles %>% 
    filter(departure_station == dept | departure_station == arr) %>%
    summarise(x = ifelse(lnk == "from", x0[x0 != 0], 0),
           xend = ifelse(lnk == "from", 0, x0[x0 != 0]),
           y = ifelse(lnk == "from", y0[y0 != 0], 0),
           yend = ifelse(lnk == "from", 0, y0[y0 != 0]))
  
  
  
}

links <- test_data %>% 
  group_by(departure_station, arrival_station) %>% 
  mutate(total_delay = ((avg_delay_late_at_departure + avg_delay_late_on_arrival)/journey_time_avg),
         total_trips = sum(total_num_trips)) %>% 
  summarize(size = mean(total_delay),
            alpha = mean(total_num_trips)/max(total_num_trips)) %>% 
  mutate(link = if_else(departure_station == centre, "to", "from")) %>% 
  ungroup() %>% 
  mutate(links = pmap(list(departure_station, arrival_station, link), ~link_coords(..1, ..2, ..3))) %>% 
  unnest() %>% 
  arrange(link, departure_station)




ggplot() +
  geom_curve(data = links, aes(x = x, xend = xend, y = y, yend = yend, size = size, color = link, alpha = alpha), lineend = "round", angle = 270) +
  geom_circle(data = circles, aes(x0 = x0, y0 = y0, group = departure_station, r = 5), fill = "white", color = "#2b41a7") +
  scale_size(range = c(1,6)) +
  scale_color_manual(values = c("#2b41a7", "#c7ad24")) +
  scale_fill_distiller(palette = "Greys")+
  scale_alpha_identity() +
  labs(x = NULL, y = NULL) +
  coord_equal() +
  theme_jk(grid = FALSE) +
  theme(axis.text = element_blank())

