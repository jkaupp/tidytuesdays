library(tidyverse)
library(jkmisc)
library(lubridate)
library(here)
library(ggridges)
library(tidytext)
library(countrycode)
library(ggwordcloud)
library(patchwork)

source(here("2019", "week22", "R", "packed_bars.R"))

wine_ratings <- here("2019", "week22", "data", "winemag-data-130k-v2.csv") %>% 
  read_csv()

wine_counts <- wine_ratings %>% 
  count(country) %>% 
  mutate(max_rel_val = n/sum(n)) %>% 
  filter(!is.na(country))
 
summary_ratings <- wine_ratings %>% 
  group_by(country) %>% 
  summarize_at(c("points","price"), mean, na.rm = TRUE) %>% 
  filter(!is.na(country))

summary_data <- left_join(wine_counts, summary_ratings)

plot_data <- pack_bars(summary_data, number_rows = 4, max_rel_val)

packed_bar <- ggplot(plot_data) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill), color = "white") +
  geom_text(data = filter(plot_data, fill == "#4B384C"), aes(x = xmin, y = (ymin + ymax)/2, label = country), family = "Oswald", color = "white", nudge_x = 0.01, hjust = 0) +
  scale_fill_identity() +
  scale_color_identity() +
  scale_x_continuous(labels = scales::percent) +
  labs(x = NULL,
       y = NULL) +
  theme_jk(grid = FALSE, ticks = TRUE) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

word_counts <- wine_ratings %>%
  select(country, description) %>%
  group_by(country) %>% 
  filter(n() > 2) %>% 
  filter(!is.na(country)) %>% 
  unnest_tokens(word, description) %>% 
  anti_join(stop_words) %>% 
  count(country, word) %>% 
  mutate(word = trimws(word)) %>% 
  filter(!str_detect(word, "[0-9]"), !str_detect(word, "aroma|wine|note|nose|notes|aromas|drink|drinks|feel|feels|finish")) %>% 
  group_by(country) %>% 
  top_n(300, n) 

clouds <- word_counts %>% 
  ungroup() %>% 
  mutate(iso2 = tolower(countrycode(country, "country.name", "iso2c")),
         iso2 = if_else(country == "England", "gb", iso2)) %>% 
  filter(country %in%  c("US", "France", "Italy", "Spain")) %>% 
  mutate(country = factor(country, levels = c("US", "France", "Italy", "Spain")),
         iso2 = factor(iso2, levels = c("us","fr", "it", "es"))) %>% 
  group_by(iso2) %>% 
  nest() %>% 
  arrange(iso2) %>% 
  mutate(clouds =  map2(iso2, data, create_wc))

word_clouds <- wrap_plots(clouds$clouds, ncol = 1) 

out <- packed_bar + word_clouds +
  plot_annotation(title = "Wine-ing: The Top 4 Countries and What Reviewers Say About Their Wines",
                  subtitle = str_wrap("On the left, a packed bar chart showing the % of reviewed wines by country.  On the right, wordclouds of the top 300 most frequent terms used in reviews.", 100),
                  caption = "Data: Kaggle via WineEnthusiast | Graphic: @jakekaupp",
                  theme = theme_jk()
                  )

ggsave(here('2019', "week22", "tw22_plot.png"), out, width = 8, height = 12)

ggsave(here('2019', "week22", "packed_bar.png"), packed_bar + labs(title = "Top 4 Countries Reviewed as Packed Bar Chart",
                                                                   subtitle = str_wrap("The visualizion below is a packed bar chart, developed by Xan Gregg.  It combines the ordered nature of a bar chart with the total view and condensed nature of a treemap.  Colour denotes the focus, while the each gray sections represents each other reviwed country. This gives a sense of how many secondary categories there are, their magnitude and distribution. Additionally, since they are on the same scale of the focused bars we can even estimate some of the values from the length they span on the axis.", 100)), width = 8, height = 6)
