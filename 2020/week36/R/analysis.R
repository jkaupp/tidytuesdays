library(tidyverse)
library(janitor)
library(jkmisc)
library(here)
library(colorspace)
library(glue)

tractors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/cereal_yields_vs_tractor_inputs_in_agriculture.csv')

arable_land <- here("2020", "week3", "data", "arable_land.csv") %>% 
  read_csv(na = "") %>% 
  pivot_longer(5:64, names_to = "year", values_to = "arable_land") %>% 
  clean_names() %>% 
  select(year, country_code, country_name, arable_land) %>% 
  filter(country_code != "AND")

world_tractor <- tractors %>% 
  clean_names() %>% 
  rename(tractors_per = tractors_per_100_sq_km_arable_land) %>% 
  select(year, country_code = code, tractors_per, total_population_gapminder) %>% 
  inner_join(arable_land) %>% 
  mutate(tractors = tractors_per/100*(1/100)*(arable_land/total_population_gapminder)) %>%
  mutate(fill = "#367c2b") %>% 
  filter(year == 2000, !is.na(tractors))
  
global_avg <- world_tractor %>% 
  summarize(tractors = mean(tractors),
            year = "2000",
            country_name = "Global Average",
            fill = "#FFDE00")

plot_data <- bind_rows(world_tractor, global_avg) %>% 
  mutate(country_name = if_else(country_name == "Global Average", 
                                highlight_text(country_name, colour = "#FFDE00", style =  "b", size = 10), 
                                highlight_text(country_name, colour = "#FFFFFF", style =  "", size = 10)))
  
plot <- ggplot(plot_data, aes(y = reorder(country_name, tractors), x = tractors)) +
  geom_col(aes(fill = fill, color = darken(fill)), size = 0.1) +
  labs(y = NULL,
       x = NULL,
       title = "You Can Take a Ride in My Big Green Tractor: Estimated Global Tractors Per Capita Circa 2000",
       subtitle = str_break(glue("Illustrated below is a bar chart of the estimated number of {highlight_text('tractors per capita', '#367c2b', 'b')} in 2000 across the globe, as well as the {highlight_text('estimated global average', '#FFDE00', 'b')}.  The tractor estimates were constructed by taking the number of tractors per square kilometer of arable land multiplying by the amount of arable land and dividing by the population."), 125),
       caption = "**Data**: Our World in Data | **Graphic**: @jakekaupp") +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_identity() +
  scale_color_identity() +
  theme_jk(dark = TRUE,
           markdown = TRUE,
           plot_title_family = "Oswald",
           plot_title_size = 20,
           subtitle_family = "Poppins",
           caption_family = "Poppins",
           base_family = "Poppins",
           grid = "X") +
  theme(plot.title.position = "plot")
  
ggsave(here("2020", "week36", "tw36_plot.png"), plot, width = 11, height = 16, dev = ragg::agg_png())
