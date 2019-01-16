library(tidyverse)
library(here)
library(nord)
library(jkmisc)
library(ggbeeswarm)
library(ggrepel)

agencies <- read_csv("https://github.com/rfordatascience/tidytuesday/raw/master/data/2019/2019-01-15/agencies.csv")

launches <- read_csv("https://github.com/rfordatascience/tidytuesday/raw/master/data/2019/2019-01-15/launches.csv")


us_launch_data <- launches %>% 
  filter(agency == "US" | state_code == "US") %>% 
  mutate(type = gsub("Zenit-", "Zenit ", type),
         type = gsub("/", " ", type),
         type = gsub("Minotaur-", "Minotaur ", type)) %>% 
  separate(type, "type", sep = " ", extra = "drop") %>% 
  mutate(type = if_else(type == "Space", "Space Shuttle", sprintf("%s Program",type))) %>% 
  mutate(label = if_else(type == "Space Shuttle" & category == "F", "Challenger Disaster", NA_character_)) %>% 
  group_by(type) %>% 
  filter(n() > 10)

plot <- ggplot(us_launch_data, aes(x = launch_year, y = type), size = 4) +
  geom_quasirandom(data = filter(us_launch_data, category == "O"), alpha = 0.2, fill = nord("polarnight", 2)[2], shape = 21, groupOnX = FALSE) +
  geom_quasirandom(data = filter(us_launch_data, category == "F"), fill = nord("victory_bonds", 5)[1], shape = 21, groupOnX = FALSE, color = "grey30", stroke = 0.2) +
  theme_jk(grid = "XY", dark = FALSE) +
  labs(x = NULL,
       y = NULL,
       title = "From the Space Race to Space-X: 1548 Successes and 101 Failures of US Launch Vehicles from 1958-2018.",
       subtitle = str_wrap("A beeswarm plot illustrating the success or failure of a launch vehicle program over time. Red dots indicate failed launches, grey dots indicate success.  Deeper grey colors indicate a higher frequency of success in a given year due to multiple launches. Only includes programs with more than 10 launches", 120),
       caption = "Data: JSR Launch Vehicle Database | Analysis: @jakekaupp")

plot <- plot + annotate("segment", x = 1987, xend = 1986.2, y = 8.7, yend = 8.2, arrow = arrow(length = unit(1, "mm")), color = nord("victory_bonds", 5)[1]) +
  annotate("text", x = 1986, y = 9, label = "Challenger Disaster", family = "Scope One", color = nord("victory_bonds", 5)[1]) +
  annotate("segment", x = 1959, xend = 1958.2, y = 1.7, yend = 1.2, arrow = arrow(length = unit(1, "mm")), color = nord("victory_bonds", 5)[1]) +
  annotate("text", x = 1959, y = 2.5, label = "First Communication\nSatellite Protoype", family = "Scope One", color = nord("victory_bonds", 5)[1], hjust = 0) +
  annotate("segment", x = 2015, xend = 2015, y = 5.5, yend = 3.1, arrow = arrow(length = unit(1, "mm")), color = nord("victory_bonds", 5)[1]) +
  annotate("text", x = 2013, y = 6.5, label = "SpaceX Falcon 9\nStrut Failure", family = "Scope One", color = nord("victory_bonds", 5)[1], hjust = 0)

ggsave(here("2019", "week3", "tt_week3.png"), plot, width = 11, height = 5)

  