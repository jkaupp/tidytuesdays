library(tidyverse)
library(jkmisc)
library(here)

board_games <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-12/board_games.csv")

prolific <- board_games %>% 
  separate_rows(designer, sep = ",") %>% 
  filter(!str_detect(designer, "Uncredited"), !str_detect(designer, "Jr|III"), year_published >= 1990) %>% 
  group_by(designer) %>% 
  filter(n() >= 10) %>% 
  group_by(designer, year_published) %>% 
  summarize(year_avg_rating = mean(average_rating, na.rm = TRUE),
            games_published = n()) 

top_rated <- prolific %>% 
  group_by(designer) %>% 
  summarize(total_games = sum(games_published),
            overall_rating = mean(year_avg_rating, na.rm = TRUE)) %>% 
  top_n(1, overall_rating) %>% 
  pull(designer)

top_publishing <- prolific %>% 
  group_by(designer) %>% 
  summarize(total_games = sum(games_published),
            overall_rating = mean(year_avg_rating, na.rm = TRUE)) %>% 
  top_n(1, total_games) %>% 
  pull(designer)

overall_avg <- prolific %>% 
  group_by(year_published) %>% 
  summarize(year_avg_rating = mean(year_avg_rating, na.rm = TRUE),
            designer = "Overall")


plot_data <- prolific %>%
  bind_rows(overall_avg) %>% 
  mutate(color = case_when(designer == top_rated ~ "#eebd31" ,
                           designer == "Overall" ~ "firebrick",
                           designer == top_publishing ~ "dodgerblue",
                           TRUE ~ "black"),
         alpha = case_when(designer == top_rated ~ 1,
                           designer == "Overall" ~ 1,
                           designer == top_publishing ~ 1,
                           TRUE ~ 0.05),
         size = case_when(designer == top_rated ~ 0.5,
                           designer == "Overall" ~ 0.5,
                           designer == top_publishing ~ 0.5,
                           TRUE ~ 0.3),
         point_size = case_when(designer == top_rated ~ 2,
                          designer == "Overall" ~ 2,
                          designer == top_publishing ~ 2,
                          TRUE ~ 1),
         line = case_when(designer == top_rated ~ "solid",
                           designer == "Overall" ~ "dashed",
                           designer == top_publishing ~ "solid",
                           TRUE ~ "solid"))

plot <- ggplot(plot_data, aes(x = year_published, y = year_avg_rating, group = designer)) +
  geom_path(aes(color = color, alpha = alpha, linetype = line, size = size)) +
  geom_point(aes(fill = color, alpha = alpha, size = point_size), color = "white", shape = 21) +
  annotate("label", x = 1989.8, y = 2, label = "Most Prolific: Reiner Knizia with 229 published games.", family = "Oswald", label.size = 0, fill = "white", color = "dodgerblue", hjust = 0) +
  annotate("segment", x = 1990, xend = 1990, y = 2.3, yend = 5.5, arrow = arrow(type = "closed", length = unit(1, "mm")), color = "dodgerblue") +
  annotate("label", x = 2002, y = 9, label = "Highest Average Rating: Mark H. Walker with a 7.70 rating.", family = "Oswald", label.size = 0, fill = "white", color = "#eebd31") +
  annotate("segment", x = 2002, xend = 2002.8, y = 8.8, yend = 7.7, arrow = arrow(type = "closed", length = unit(1, "mm")), color = "#eebd31") +
  scale_y_continuous(limits = c(0, 10), breaks = scales::pretty_breaks()) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  scale_color_identity() +
  scale_linetype_identity() +
  scale_size_identity() +
  scale_fill_identity() +
  scale_alpha_identity() +
  labs(x = NULL, 
       y = NULL,
       title = "It's Not A Habit, It's Cool, I'm a Prolific Game Designer",
       subtitle = str_wrap("A comparison from 1990 to 2016 of the the top rated and top published designer amongst those with 10 or more published games. Red dashed line represents the overall average designer rating.", 150),
       caption = "Data: Board Game Geek | Graphic: @jakekaupp") +
  theme_jk(grid = "XY")

ggsave(here("2019","week11", "tw11_plot.png"), width = 12, height = 6)
