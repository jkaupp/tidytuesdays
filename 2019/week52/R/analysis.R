library(tidyverse)
library(glue)
library(lubridate)
library(here)
library(jkmisc)
library(ggchicklet)

christmas_songs <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-24/christmas_songs.csv")

order <- christmas_songs %>% 
  distinct(songid, year, .keep_all = TRUE) %>%
  count(performer, sort = TRUE) %>% 
  filter(n>1) %>% 
  pull(performer)


plot_data <- christmas_songs %>% 
  distinct(songid, year, .keep_all = TRUE) %>%
  count(performer, song, sort = TRUE) %>% 
  ungroup() %>% 
  filter(performer %in% order)





plot <- ggplot(plot_data, aes(x = factor(performer, levels = rev(order)), y = n)) +
  geom_chicklet(fill = "#bd4651", color = "white", size = 1) +
  geom_text(aes(label = performer, y = 0.05), color = "white", family = "Oswald", hjust = 0) +
  coord_flip() +
  labs(x = NULL,
       y = NULL,
       title = "Holiday Artists: Prolfic or Memorable?",
       subtitle = str_wrap("Illustrated below is a chicklet plot (segemented bars) showing the number of chart appearances for each artist.  Each chicklet represents a individual song for that artist.", 80),
       caption = "**Data:** Kaggle | **Graphic:** @jakekaupp") +
  scale_y_continuous(expand = c(0,0.1), breaks = 1:8) +
  theme_jk(grid = "X",
           markdown = TRUE) +
  theme(axis.text.y = element_blank())


ggsave(here("2019", "week52", "tw52_plot.png"), plot, width = 15, height = 15)
