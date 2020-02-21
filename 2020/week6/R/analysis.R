library(tidyverse)
library(ggplot2)
library(ggtext)
library(here)
library(ggforce)
library(jkmisc)
library(patchwork)
library(viridisLite)
library(colorspace)

games <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/games.csv')

more_games <- here('2020', 'week6', 'data', 'spreadspoke_scores.csv') %>% 
  read_csv() 

hist_games <- more_games %>% 
  rename(year = schedule_season) %>% 
  filter(year < 2000) %>% 
  select(year, home_team = team_home, away_team = team_away, score_home, score_away) %>% 
  mutate(total = score_home + score_away,
         pts_win = map2_dbl(score_home, score_away, ~max(.x, .y)),
         pts_loss = map2_dbl(score_home, score_away, ~min(.x, .y))) %>%
  select(year, home_team, away_team, total, pts_win, pts_loss)
  
full_games <- games %>% 
  select(year, home_team, away_team, pts_win, pts_loss) %>% 
  mutate(total = pts_win + pts_loss) %>% 
  select(year, home_team, away_team, total, pts_win, pts_loss) %>% 
  bind_rows(hist_games) %>% 
  arrange(year) 

areas_data <- full_games %>% 
  mutate(total_bin = cut(total, c(0, 20, 40, 60, 80, Inf), labels = c("0 - 2 0 p t s", "2 1 - 4 0 p t s", "4 1 - 6 0 p t s", "6 1 - 8 0 p t s", "8 1 - 1 0 0 + p t s"))) %>% 
  count(year, total_bin)  
  
areas <- ggplot(areas_data, aes(x = year, y = n, group = total_bin, fill = total_bin)) +
  geom_area(show.legend = FALSE) +
  geom_point(data = filter(areas_data, year %in% c(1972, 1974, 1978, 1994)), aes(fill = total_bin), color = "#ECEFF4", show.legend = FALSE, shape = 21, stroke = 0.5) +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100, 125, 150)) +
  scale_x_continuous(breaks = c(1970, 1980, 1990, 2000, 2010, 2020)) +
  labs(x = NULL, y = "Combined Score") +
  facet_wrap(~total_bin, nrow = 1) +
  scale_fill_viridis_d(option = "plasma") +
  scale_color_viridis_d(option = "plasma", ) +
  theme_jk(dark = TRUE,
           grid = "XY") +
  theme(plot.background = element_rect(colour = NA, fill = "#2E3440", size = 0))


annotations <- full_games %>% 
  group_by(year) %>% 
  summarize(total = mean(total)) %>% 
  mutate(total = ifelse(year %in% c(1972, 1978), total - 2, total + 2)) %>% 
  filter(year %in% c(1972, 1974, 1978, 1994)) %>% 
  mutate(year2 = if_else(year == 1978, 1984, year),
         label_y = c(-2, 135, -2, 135),
         arrow_y = c(-10 ,110, -10, 110),
         label = c("**1 9 7 2**<br>First major rule change to boost scoring and excitement: Hashmarks set at goal-post width to widen the short-side of the field.",
                   "**1 9 7 4**<br>Second major rule change: Goalposts moved back and offensive penalties reduced to 10 from 15 yards.",
                   "**1 9 7 8**<br>Third major rule change:  Introduced illegal contact rules and provided more freedom to pass-blocking linemen.",
                   "**1 9 9 4**<br>Fourth major rule change:  Introduced two-point conversions, longer kickoffs and field goal changes."))

means <- full_games %>% 
  group_by(year) %>% 
  summarize(total = mean(total)) %>% 
  filter(year %in% c(1972, 1974, 1978, 1994))

dots <- full_games %>% 
  ggplot(aes(x = year, y = total, color = total)) +
  geom_sina(aes(group = year)) + 
  scale_x_continuous(breaks = c(1966, 1970, 1980, 1990, 2000, 2010, 2020)) +
  expand_limits(y = c(-40, 150)) +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100, 125)) +
  stat_summary(fun.y = "mean", geom = "line", color = "#ECEFF4") +
  stat_summary(fun.y = "mean", geom = "point", color = "#ECEFF4") +
  geom_point(data = means, fill = "#CC4678FF", color = "#ECEFF4", show.legend = FALSE, shape = 21, stroke = 1) +
  geom_curve(data = filter(annotations, !year %in%  c(1972, 1978)), aes(x = year, xend = year, y = arrow_y, yend = total), color = "#ECEFF4", arrow = arrow(type = "closed", length = unit(3, "mm")), curvature = 0.2) +
  geom_curve(data = filter(annotations, year %in%  c(1972, 1978)), aes(x = year2, xend = year, y = arrow_y, yend = total), color = "#ECEFF4", arrow = arrow(type = "closed", length = unit(3, "mm")), curvature = -0.2) +
  geom_textbox(data = annotations, aes(x = year2, y = label_y, label = label), family = "Oswald", color = "#ECEFF4", fill = "#2E3440") +
  scale_color_viridis_c(option = "plasma") +
  labs(x = NULL, y = "Combined Score") +
  theme_jk(grid = "XY",
           dark = TRUE) +
  theme(legend.position = "none") +
  theme(plot.background = element_rect(colour = NA, fill = "#2E3440", size = 0))


plot <- wrap_plots(dots, areas, ncol = 1, heights = c(0.75, 0.25)) + plot_annotation(title = "Did NFL Rule Changes to Boost Scoring and Make the Game More Exciting Actually Work?",
                                                                           subtitle = "Illustrated below is a sina plot (combined strip and violin plots) showing the combined score of each NFL game from 1966 to 2019 seasons.  The white line indicates the average combined score for each<br>season.  The area charts below show binned combined scores, circles indicating seasons the rules were changed. From both charts we can see that average scores have been rising, specifically in the<br>41-60 and 61-80 categories rising since the rule changes.",
                                                                           caption = "**Data**: Pro Football Reference | **Graphic**: @jakekaupp",
                                                                           theme = theme_jk(dark = TRUE, markdown = TRUE))

ggsave(here("2020", "week6", "tw6_plot.png"), plot, width = 16, height = 10)

