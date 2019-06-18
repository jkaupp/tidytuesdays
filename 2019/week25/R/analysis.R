library(tidyverse)
library(waffle)
library(jkmisc)
library(here)

bird_counts <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-18/bird_counts.csv")

top_10 <- bird_counts %>% 
  separate(species_latin, c("genus", "family"), sep = " ") %>% 
  group_by(genus) %>% 
  summarize(total = sum(how_many_counted_by_hour, na.rm = TRUE)) %>% 
  top_n(10, total) %>% 
  pull(genus)

by_genus <- filter(bird_counts, year >= 2000) %>% 
  separate(species_latin, c("genus", "family"), sep = " ") %>% 
  filter(genus %in% top_10) %>% 
  group_by(year, genus) %>% 
  summarize(counts_by_hour = sum(how_many_counted_by_hour, na.rm = TRUE))

colors <- set_names(gray.colors(10), top_10)

colors["Anas"] <- "#ffd45c"

ducks <- ggplot(by_genus, aes(values = counts_by_hour, fill = genus)) +
  geom_waffle(color = "white", size = .25, n_rows = 10, flip = TRUE, show.legend = FALSE) +
  facet_wrap(~year, nrow = 1, strip.position = "bottom") +
  coord_equal() +
  labs(title = str_to_title("The Duck is one of the most noble, agile and intelligent creatures in the animal kingdom."),
       subtitle = str_wrap("Total counts per hour, of the top 10 genera from since 2000.  Duck counts (genus Anas) are highlighted in yellow, because if it looks like a duck, and quacks like a duck, we have at least to consider the possibility that we have a small aquatic bird of the family anatidae on our hands.", 120),
       caption = "Data: www.birdscanada.org/ | Graphic: @jakekaupp") +
  scale_x_discrete() + 
  scale_y_continuous(labels = function(x) x * 10,
                     expand = c(0,0)) +
  scale_fill_manual(values = colors) +
  theme_jk(grid = FALSE, ticks = TRUE) +
  theme(strip.text = element_text(size = rel(0.8)))

cobra_chicken <- bird_counts %>% 
  filter(year >=1950) %>% 
  group_split(species) %>% 
  map_dfr(~mutate(.x, color = if_else(species == "Canada Goose", "#CB181D", sample(gray.colors(255), 1)),
                  alpha = if_else(species == "Canada Goose", 1, 0.25))) %>%   
  ggplot(aes(x = year, y = how_many_counted_by_hour, group = fct_relevel(species, "Canada Goose", after = Inf), fill = color, alpha = alpha), color = "grey30") +
  geom_area() +
  scale_y_continuous(expand = c(0.01, 0.1), breaks = scales::pretty_breaks()) +
  scale_x_continuous(breaks = c(seq(1950, 2010, 10), 2017)) +
  scale_fill_identity() +
  scale_alpha_identity() +
  labs(x = NULL, 
       y = NULL, 
       title = "Rise Of The Cobra Chicken, the Scourge of the Hamilton Waterfront",
       subtitle = str_wrap("The Canada Goose, hilarious and aptly referred to as a 'Cobra Chicken' has been a threat to the delicate ecosystem of Hamilton's Harbor.", 120),
       caption = "Data: www.birdscanada.org | Graphic: @jakekaupp") +
  theme_jk(grid = FALSE, ticks = TRUE)
  

ggsave(here("2019", "week25", "tw25_ducks.png"), ducks, width = 10, height = 4)
ggsave(here("2019", "week25", "tw25_canada_goose.png"), cobra_chicken, width = 10, height = 4)
