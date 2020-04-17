library(tidyverse)
library(here)
library(jkmisc)
library(colorspace)
library(ggforce)
library(glue)


polls <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/polls.csv')
rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/rankings.csv')

# By Year ----

songs_year <- count(polls, year, name = "songs")

points_year <- count(rankings, year, wt = points, name = "points")

labels <- rankings %>% 
  distinct(year, title, artist, .keep_all = TRUE) %>% 
  arrange(year) %>% 
  group_by(year) %>% 
  top_n(2, points) %>% 
  slice(1:2) %>% 
  group_by(year) %>% 
  summarize(description = paste0(glue("{artist}: {title}"), collapse = "\n"))

y_axis_labels <- tibble(x = rep(2021, 5),
                      y = c(-50, 0, 100, 200, 300),
                      label = glue("{c(50, 0, 100, 200, 300)} {c('songs', rep('points', 4))}"))

x_axis_labels <- tibble(x = c(1979, 2019),
                        y = rep(-10, 2))

plot_data <- reduce(list(songs_year, points_year, labels), left_join) 

plot <- ggplot(plot_data, aes(x = year)) +
  geom_hline(yintercept = c(-50, 0, 100, 200, 300), size = 0.1, color = "grey60") +
  geom_text(data = y_axis_labels, aes(x = x, y = y, label = label), family = "Oswald", size = 4, color = "grey60", vjust = 1.2, hjust = 0) +
  geom_text(data = x_axis_labels, aes(x = x, y = y, label = x), family = "Oswald", size = 4, color = "grey60") +
  geom_area(aes(y = -songs), fill = "#3f3047", color = lighten("#3f3047", amount = 0.1), size = 0.5) +
  geom_area(aes(y = points), fill = "#26408b", color = darken("#26408b", amount = 0.1), size = 0.5) +
  geom_mark_circle(aes(y = points, label = year, description = description, filter = year %in% c(1982, 1994, 1998, 2000, 2010)), label.minwidth = unit(200, "mm"), label.family = c("Oswald", "Montserrat"),  label.buffer = unit(5, "mm")) +
  scale_x_continuous(limits = c(1979, 2021)) +
  scale_y_continuous(limits = c(-100, 350)) +
  labs(x = NULL, 
       y = NULL,
       title = "The Rise, The Golden Age and The Future: The Greatest Hip-Hop Songs of All Time",
       subtitle = glue("Shown below is a mirrored area chart illustrating {highlight_text('ratings point totals', '#26408b', 'b', 16)} and {highlight_text('number of rated songs', '#3f3047', 'b', 16)} aggregated from the BBC Music poll of 108 critics.  The greatest song, Juicy by The Notorious B.I.G hails from the Golden Age of Hip-Hop<br>which produced many of the songs in contention for the title."),
       caption = "**Data**: @sjockers | **Graphic**: @jakekaupp") +
  theme_jk(grid = FALSE,
           markdown = TRUE,
           subtitle_family = "Montserrat") +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank())

ggsave(here('2020', 'week16', 'tw16_plot.png'), plot, width = 20, height = 12, device = ragg::agg_png())
 