library(nord)
library(tidyverse)
library(ggmap)
library(here)
library(countrycode)
library(jkmisc)
library(patchwork)

source(here("2019", "week24", "R", "packed_bars.R"))

if (!file.exists(here("2019", "week24", "data", "rev_geocoded_meteorites.RDS"))) {
  
  slow_revgeocode <- slowly(~revgeocode(.x, output = "address"), rate = rate_delay(0.03), quiet = TRUE)
  
  reverse_geocoded <- meteorites %>% 
    distinct(long, lat) %>% 
    mutate(location = map2_chr(long, lat, ~slow_revgeocode(c(.x, .y))))
  
  saveRDS(reverse_geocoded, here("2019", "week24", "data", "rev_geocoded_meteorites.RDS"))
  
  
} else {
  
  meteorite_locations <- readRDS(here("2019", "week24", "data", "rev_geocoded_meteorites.RDS"))
  
  
}

meteorites <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-11/meteorites.csv")

meteorites <- left_join(meteorites, meteorite_locations) %>% 
  mutate(country_code = countrycode(location, "country.name", "iso3c")) %>% 
  mutate(country_code = case_when(str_detect(location, "UK") ~ "GBR",
                                  str_detect(location, "USA") ~ "USA",
                                  str_detect(location, "China") ~ "CHN",
                                  str_detect(location, "Philippines") ~ "PHL",
                                  str_detect(location, "Serbia") ~ "RUS",
                                  str_detect(location, "Australia") ~ "AUS",
                                  str_detect(location, "Chile") ~ "CHL",
                                  str_detect(location, "Shopian") ~ "IND",
                                  str_detect(location, "Argentina") ~ "ARG",
                                  str_detect(location, "Bass Strait") ~ "AUS",
                                  TRUE ~ country_code)) %>% 
  mutate(country_code = case_when(str_detect(name, "Indarch") ~ "AZE",
                                  str_detect(name, "Oum Dreyga") ~ "ESH",
                                  str_detect(name, "Zag") ~ "ESH",
                                  str_detect(name, "Al Haggounia") ~ "ESH",
                                  str_detect(name, "Bou Kra") ~ "ESH",
                                  TRUE ~ country_code)) %>% 
  rename(iso3c = country_code) %>% 
  filter(!is.na(iso3c)) 


world_tile_grid <- read_csv("https://gist.githubusercontent.com/maartenzam/787498bbc07ae06b637447dbd430ea0a/raw/9a9dafafb44d8990f85243a9c7ca349acd3a0d07/worldtilegrid.csv")

meteorite_wtg <- meteorites %>% 
  group_by(iso3c) %>% 
  summarize(n = n(),
            mass = sum(mass, na.rm = TRUE)/1000) %>%
  mutate(per_meteorite = mass/n) %>% 
  right_join(world_tile_grid, by = c("iso3c" = "alpha.3")) %>% 
  mutate(text_color = if_else(per_meteorite < 1, "white", "black")) %>% 
  replace_na(list('alpha.2' = "NA",
                  "text_color" = "black")) 


meteorite_map <- ggplot(meteorite_wtg, aes(x, y, fill = odds, group = iso3c)) +
  geom_tile(color = "grey30", size = 0.1) +
  geom_text(aes(label = alpha.2, color = text_color), family = "Oswald") +
  labs(x = NULL,
       y = NULL) +
  scale_y_reverse() +
  scale_fill_viridis_c(name = "Average Metorite Mass (kg, log scale)",option = "cividis", na.value = "white", breaks = c(1, 10, 100, 1000, 10000, 100000), guide = guide_colourbar(title.position = "top", title.hjust = 0)) +
  scale_color_identity() +
  theme_jk(grid = FALSE) +
  theme(axis.text = element_blank(),
        legend.direction = "horizontal",
        legend.key.width = unit(2, "lines"),
        legend.position = c(0.2, 0.05))


plot_data <- meteorite_wtg %>%
  select(alpha.2, n) %>% 
  mutate(n = log10(n)) %>% 
  replace_na(list(n = 0)) %>% 
  pack_bars(10, value_column = n, fill_color = last(nord("lumina", 5)))


packed_bars <- ggplot(plot_data) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill), color = "white", size = 0.1) +
  geom_text(data = filter(plot_data, (xmax - xmin) > 0.1), aes(x = (xmin + xmax)/2, y = (ymin + ymax)/2, label = alpha.2), family = "Oswald", color = "white") +
  scale_fill_identity() +
  scale_color_identity() +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  labs(x = NULL,
       y = NULL) +
  theme_jk(grid = FALSE, ticks = TRUE) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())


out <- packed_bars + meteorite_map  + plot_annotation(title = "You May Need More Than An Umbrella in Russia:  Where the Most, and Heaviest, Meteorites fall",
                                                subtitle = str_wrap("On the left is a packed bar chart showing the top 10 regions struck by the most meteorites, while the tile map on the right shows the average meteorite mass across all regions.  Both measures have been scaled logathrimically to aid in comparability.", 180),
                                                caption = "Data: NASA | Graphic: @jakekaupp",
                                              theme = theme_jk())

ggsave(here("2019", "week24", "tw24_plot.png"), out, width = 14, height = 7)
