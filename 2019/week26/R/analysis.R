library(sf)
library(albersusa)
library(here)
library(jsonlite)
library(RCurl)
library(janitor)
library(jkmisc)
library(cowplot)
library(tidyverse)



# Get ufo  & pop. density data----
ufo_sightings <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-25/ufo_sightings.csv")

pop_density <- read_csv(here("2019", "week26", "data", "pop_density.csv"), skip = 1) %>% 
  clean_names() %>% 
  filter(target_geo_id2 > 1000) %>% 
  mutate(fips = str_pad(target_geo_id2, 5, side = "left", pad = "0")) %>% 
  select(fips, density = contains("density"))

# Get alberusa us county sf object----
us_counties <- counties_sf()

# Conver the us ufo sightings to an sf object and join with the alberusa to the the county fips----
usa_sightings_fips <- ufo_sightings %>% 
  filter(country == 'us') %>% 
  st_as_sf(crs = 4326, coords = c("longitude", "latitude")) 

cont_usa_sightings <- st_join(us_counties, usa_sightings_fips) 



# Some are missing!----
missing <- st_join(usa_sightings_fips, us_counties)  %>% 
  filter(is.na(fips)) %>% 
  semi_join(ufo_sightings,.)

# Make a function to call to the fcc census block API----
geocode_fips <- function(latitude, longitude, index) {
  
  url <- sprintf("https://geo.fcc.gov/api/census/block/find?latitude=%f&longitude=%f&format=json",  latitude, longitude)
  
  response <- getURL(url)
  
  json <- fromJSON(response)
  
  print(index)
  
  as.character(json$County['FIPS'])
}

# Make this work insistently----
insistent_geocode <- insistently(~geocode_fips(..1, ..2, ..3), rate = rate_backoff())

# Make it return NA if it fails ----
poss_insistent_geocode <- possibly(~insistent_geocode(..1, ..2, ..3), otherwise = NA_character_)

# Get the missing fips ----

if(!file.exists(here("2019", "week26", "data", "missing_fips.RDS"))) {
  missing_fips <- missing %>% 
    distinct(latitude, longitude) %>% 
    mutate(index = row_number()) %>% 
    mutate(fips = pmap_chr(list(latitude, longitude, index), poss_insistent_geocode)) } else {
      
      missing_fips <- readRDS(here("2019", "week26", "data", "missing_fips.RDS"))
      
    }

# Join it back to missing to fill in fips ----
missing <- left_join(missing, missing_fips) %>% 
  dplyr::select(-index) 

# Bind rows back to cont_usa_sightings for full_usa data ----
full_usa <- cont_usa_sightings %>% 
  left_join(missing, by = c(names(ufo_sightings)[c(1:2,4:9)], "state.x" = "state")) %>% 
  mutate_at(vars(contains("fips")), as.character) %>% 
  mutate(fips = coalesce(`fips.x`, `fips.y`)) %>% 
  select(-fips.x, -fips.y, -state_fips, -county_fips, -latitude, -longitude)

# Summarize sightings, create a ratio and add in population densities----
plot_data <- full_usa %>% 
  group_by_at(.vars = vars(fips, name, lsad, census_area, state.y, iso_3166_2)) %>% 
  summarize(sightings = n()) %>% 
  ungroup() %>% 
  mutate(sightings_ratio = 100*sightings/sum(sightings)) %>% 
  left_join(pop_density)


# create 3 buckets for variables ---
quantiles_sightings <- plot_data %>%
  pull(sightings_ratio) %>%
  quantile(probs = seq(0, 1, length.out = 4))

quantiles_density <- plot_data %>%
  pull(density) %>%
  quantile(probs = seq(0, 1, length.out = 4))

# create color scale that encodes two variables
# red for sightings and blue for population density
bivariate_color_scale <- tibble(
  "3 - 3" = "#3F2949", # high sightings, high density
  "2 - 3" = "#435786",
  "1 - 3" = "#4885C1", # low sightings, high density
  "3 - 2" = "#77324C",
  "2 - 2" = "#806A8A", # medium sightings, medium density
  "1 - 2" = "#89A1C8",
  "3 - 1" = "#AE3A4E", # high sightings, low density
  "2 - 1" = "#BC7C8F",
  "1 - 1" = "#CABED0" # low sightings, low density
) %>%
  gather("group", "fill")


# Assign each fips area to their correct group and assign the fill from the bivariate scale ----
plot_data <- plot_data %>%
  mutate(sightings_quantiles = cut(sightings_ratio,
                              breaks = quantiles_sightings,
                              include.lowest = TRUE),
    density_quantiles = cut(density,
                            breaks = quantiles_density,
                            include.lowest = TRUE),
    group = paste(as.numeric(sightings_quantiles), "-", as.numeric(density_quantiles))) %>%
  left_join(bivariate_color_scale, by = "group")


# Making ze plot ----
plot <- ggplot(plot_data) +
  geom_sf(aes(fill = fill), size = 0.05, color = "#2b2b2b") +
  scale_fill_identity() +
  labs(title = "If A UFO Flew Over The Desert And No One Was Around To See It, Would Senators Be Briefed?",
       subtitle = str_wrap("Below is a bivariate choropleth map by county illustrating the relationship between the UFO sightings (% of recorded sightings since 1911) and population density (people per sq. mile circa 2010).  Densely populated coastal and lakeside areas along with the sparsely populated southwest have the highest sightings, whereas the less populous midwest and Alaska have lower percentages of sightings.", 110),
       caption = "Data: NUFORC & 2010 US Census | Graphic: @jakekaupp",
       x = NULL,
       y = NULL) +
  theme_jk(grid = FALSE) +
  theme(axis.text = element_blank()) +
  coord_sf(clip = "off")

# Making ze legend ---
bivariate_legend <- bivariate_color_scale %>% 
  separate(group, into = c("sightings", "density"), sep = " - ") %>%
  mutate_at(c("sightings", "density"), as.integer)

legend <- ggplot(bivariate_legend) +
  geom_tile( aes(x = sightings, y = density, fill = fill)) +
  scale_fill_identity() +
  labs(x = expression(paste("More Sightings ", symbol('\256'))),
       y = expression(paste("More People ", symbol('\256')))) +
  theme_jk(grid = FALSE) +
  theme(axis.title = element_text(size = 6),
        axis.text = element_blank()) +
  coord_fixed(clip = "off")

finished_plot <- ggdraw() +
  draw_plot(plot, 0, 0, 1, 1) +
  draw_plot(legend, 0.75, 0.075, 0.2, 0.2)


ggsave(here("2019", "week26", "tw26_plot.png"), plot = finished_plot, width = 10, height = 6)
