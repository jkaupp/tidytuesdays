library(tidyverse)
library(glue)
library(here)
library(waffle)
library(nord)
library(jkmisc)
library(magick)

hike_data_rds <- url('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-24/hike_data.rds') %>% 
  read_rds()


hike_data <- hike_data_rds %>%
  distinct(name, location, .keep_all = TRUE) %>%
  mutate(trail_id = row_number()) %>%
  select(trail_id, everything()) %>%
  mutate(across(c(length, gain, highpoint, rating), parse_number)) %>%
  rename(length_mi = length,
         gain_ft = gain,
         highpoint_ft = highpoint) %>%
  mutate(rating = replace(rating, rating == 0, NA))


plot_data <- hike_data %>% 
  select(name, features) %>% 
  unnest(features) %>% 
  count(features, sort = TRUE) %>% 
  mutate(not_n = nrow(hike_data) - n) %>% 
  pivot_longer(n:not_n) %>% 
  mutate(features = str_trim(features)) %>% 
  mutate(features = factor(features, unique(features)))

labels <- plot_data %>% 
  filter(name == "n") %>% 
  distinct(features, value)

plot <- ggplot(plot_data) +
  geom_waffle(aes(fill = name, values = value), n_rows = 20, size = 0.1, colour = "white") +
  geom_text(data = labels, aes(x = nrow(hike_data)/20 + 10, y = 1.2, label = glue("{value} {toupper(features)}")), family = "Staatliches", hjust = 0, vjust = -0.5, color = nord_palettes$mountain_forms[5]) +
  scale_fill_manual(values = nord_palettes$mountain_forms[c(5,3)])+
  scale_x_continuous(labels = function(x) x * 20,
                     expand = c(0,0)) +
  labs(x = NULL,
       y = NULL,
       title = "Frequency of Features in Washington<br>Hiking Trails",
       subtitle = "Illustrated below are waffle bar charts showing<br>the frequencies of hiking trail features against<br>the all 1957 trails listed by the Washington Trails<br>Association.",
       caption = "**Data**: wta.org | **Graphic**: @jakekaupp") +
  coord_equal(clip = "off") +
  facet_wrap(~features, ncol = 1) +
  theme_jk(grid = FALSE,
           ticks = TRUE,
           plot_title_family = "Staatliches",
           markdown = TRUE) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = element_blank(),
        legend.position = "none")

ggsave(here("2020", "week48", "tw48_plot.png"), plot, width = 8, height = 10, dev = ragg::agg_png())

