library(tidyverse)
library(lubridate)
library(here)
library(jkmisc)
library(patchwork)

tidy_anime <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-23/tidy_anime.csv")

plot_data <- tidy_anime %>% 
  mutate(title = coalesce(title_english, name)) %>% 
  mutate(end_date = if_else(is.na(end_date), as.Date("2019-04-01"), end_date)) %>% 
  mutate(interval = interval(start_date, end_date)) %>% 
  filter(type != "Unknown") %>% 
  distinct(animeID, .keep_all = TRUE) 


scaffold <- tibble(year = rep(1917:2019, each = 6),
       type = rep(c("Movie", "Music", "ONA", "OVA", "Special", "TV"), length(1917:2019))) 

timeline <- scaffold %>% 
  mutate(count = map2_dbl(year, type, ~nrow(filter(plot_data, ymd(sprintf("%s/01/01", .x)) %within% interval , type == .y))))

order <- timeline %>% 
  filter(year == last(year)) %>% 
  arrange(desc(count)) %>% 
  pull(type)

# Area ----
area <- timeline %>% 
  mutate(type = factor(type, order, order)) %>% 
  ggplot(aes(x = year, y = count)) +
  geom_area(aes(fill = type)) +
  scale_fill_manual("Anime Type", values = tol6qualitative) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  labs(x = NULL, 
       y = NULL,
       title = "Fourty Years of Growth: The Rapid Rise of Anime",
       subtitle = str_wrap("The area chart below presents the number of anime titles released from 1919 to the present by release type.  Anime releases have increased over 400% since the 1980s, to meet the increasing demand driven by the invention of the VCR, the internet and the rise of streaming media services.", 95),
       caption = "Data: MyAnimeList | Graphic: @jakekaupp") +
  theme_jk(grid = "XY") +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))

# Popularity vs Score Hexbin ----
hex <- ggplot(plot_data, aes(x = popularity, y = score)) +
  geom_hex() +
  facet_wrap(~type, nrow = 1) +
  scale_fill_viridis_c(option = "plasma") +
  scale_x_continuous(trans = "reverse", breaks = scales::pretty_breaks(), labels = c("","","Higher\nPopularity", "", "Lower\nPopularity", "")) +
  scale_y_continuous(breaks = scales::pretty_breaks(), limits = c(0,10)) +
  guides(fill = guide_colorbar(title = "Titles per Hex"), alpha ="none") +
  labs(x = NULL, 
       y = NULL,
       title = "The Relationship Between Ratings and Popularity on MyAnimeList",
       subtitle = str_wrap("The chart below plots the ratings score (out of 10) against popularity (rank) for all anime titles and anime types.  The data were hexangonally binned to illustrate areas of high occurance. It appears that a relationship may exist between ratings and popularity, warranting further analysis.", 100),
       caption = "Data: MyAnimeList | Graphic: @jakekaupp") +
  theme_jk(grid = "XY") +
  theme(legend.position = "bottom") 

ggsave(here("2019","week17","tw17_hex.png"), hex, width = 8, height = 6)

ggsave(here("2019","week17","tw17_area.png"), area, width = 8, height = 6)

                         