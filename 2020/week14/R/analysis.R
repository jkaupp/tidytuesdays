library(tidyverse)
library(cartogram)
library(sf)
library(here)
library(janitor)
library(readxl)
library(jkmisc)
library(albersusa)
library(pals)


beer_states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_states.csv')

pop <- here("2020", "week14", "data") %>%
  dir(full.names = TRUE) %>% 
  read_excel(col_names = FALSE) %>% 
  slice(-1:-3) %>% 
  set_names(slice(., 1)) %>% 
  slice(-1) %>% 
  clean_names() %>% 
  rename(state = na) %>% 
  filter(str_detect(state, "\\.")) %>% 
  mutate(state = str_remove(state, "\\.")) %>% 
  select(name = state, x2019)

total_beer_states <- beer_states %>% 
  filter(year == 2019) %>% 
  group_by(state) %>% 
  summarize(total = mean(barrels, na.rm = TRUE))

  
usa <-  usa_sf() %>% 
  st_transform(3857) %>% 
  left_join(tibble(state = state.abb,
                   name = state.name)) %>% 
  left_join(total_beer_states) %>% 
  inner_join(pop) %>% 
  filter(name != "District of Columbia") %>% 
  filter(name != "Alaska") %>% 
  filter(name != "Hawaii") %>% 
  mutate(beer_cap = total/x2019)

us_cartogram <- cartogram_cont(usa, "beer_cap")

labels <- filter(us_cartogram, beer_cap >= 0.25) %>% 
  mutate(name = str_replace_all(toupper(name), "(?<=.)(?!$)", " ")) %>% 
  mutate(name = str_replace(name, "N E W", "N E W\n"))

out <- ggplot(us_cartogram) +
  geom_sf(aes(fill = beer_cap), color = "#f5f5f4", size = 0.1) +
  geom_sf_text(data = labels, aes(label = name), family = "Oswald Light", color = "white") +
  scale_fill_gradient("Barrels per capita", low = "#2d3047", high = "#1a936f",  breaks = c(0.01, 0.25, 0.5, 0.75, 1.1), labels = c("<1/4", "1/4", "1/2", "3/4", "1+"), guide = guide_legend( keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'top', nrow =1 )) +
  labs(x = NULL,
       y = NULL,
       title = "Barrels of Beer Per Capita in the Mainland United States in 2019",
       subtitle = "Shown below is a cartogram, with state size distorted to show the porportion of per capita beer production in 2019.  Colorado is the state with the greatest<br>beer production per capita, with more than a single barrel of beer produced for each person in the state.",
       caption = "**Data**: ttb.gov & census.gov | **Graphic**: @jakekaupp") +
  theme_jk(grid = FALSE,
           markdown = TRUE) +
  theme(legend.position = c(0.2, 0.2),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.background = element_rect(fill = "#f5f5f4", color = NA), 
        panel.background = element_rect(fill = "#f5f5f4", color = NA), 
        legend.background = element_rect(fill = "#f5f5f4", color = NA))

ggsave(here("2020", "week14", "tw14_plot.png"), out, width = 16, height = 8, dev = ragg::agg_png())

