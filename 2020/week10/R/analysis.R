library(tidyverse)
library(jkmisc)
library(rvest)
library(patchwork)
library(ggforce)
library(glue)
library(here)

season_goals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/season_goals.csv')

url <- "https://www.hockey-reference.com/leagues/stats.html"

total_scoring <- read_html(url) %>% 
  html_node("#stats") %>% 
  html_table() %>% 
  filter(!str_detect(Rk, "[A-Z]")) %>% 
  mutate_at(vars(GP:GAA), as.numeric) %>% 
  mutate(year = as.numeric(str_sub(Season, 1, 4))) 

ppm_data <- season_goals %>% 
  group_by(player, season) %>% 
  summarize(goals = sum(goals, na.rm = TRUE),
            assists = sum(assists, na.rm = TRUE)) %>% 
  group_by(season) %>% 
  mutate(min_goals = min(goals, na.rm = TRUE),
         max_goals = max(goals, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(year = as.numeric(str_sub(season, 1, 4))) %>% 
  arrange(year) %>% 
  filter(between(year, 1943, 2019))

top_3 <- ppm_data %>% 
  filter(goals == max_goals) %>% 
  count(player) %>% 
  ungroup() %>% 
  top_n(3, n) %>% 
  arrange(desc(n)) %>% 
  mutate(color = case_when(str_detect(player, "Ovechkin") ~ "#041E42",
                           str_detect(player, "Bobby Hull") ~ "#FFB81C",
                           str_detect(player, "Maurice") ~ "#AF1E2D",
         TRUE ~ NA_character_),
         alpha = 1,
         size = 1)

bkg_rect <- ppm_data %>% 
  distinct(year, season, min_goals, max_goals) %>% 
  left_join(total_scoring)

plot_data <- ppm_data %>% 
  left_join(top_3) %>% 
  replace_na(list(color = "#363636",
                  alpha = 0.2,
                  size = 0.5))

annotations <- ppm_data %>% 
  filter(goals == max_goals) %>% 
  semi_join(top_3) %>% 
  group_by(player) %>% 
  filter(goals == max(max_goals)) %>% 
  ungroup() %>% 
  mutate(xend = c(1952, 1970, 2010),
         yend = c(70, 90, 70)) %>% 
  mutate(color = case_when(str_detect(player, "Ovechkin") ~ "#041E42",
                           str_detect(player, "Bobby Hull") ~ "#FFB81C",
                           str_detect(player, "Maurice") ~ "#AF1E2D",
                           TRUE ~ NA_character_))

avg_scoring <- ggplot(bkg_rect, aes(x = year)) +
  geom_tile(aes(y = 0, fill = G, height = 1, width = 0.8),  color = "white") +
  geom_tile(aes(x = 2004, y = 0, height = 1, width = 0.8), fill = "#FFFFFF", color = "#363636", size = 0.5) +
  labs(x = NULL,
       y = NULL) +
  coord_equal() +
  scale_x_continuous(limits = c(1943, 2019), expand = c(0,0), breaks = seq(1940, 2020, 10), position = "top") +
  scale_fill_gradient("Average Goals per game, for comparative puporses and\nto account for the pace of the game in different eras.",  low = "#e5e5e5", high = "#DC143C") +
  theme_jk(grid = FALSE) +
  theme(axis.text.y = element_blank(),
        plot.margin = margin(0, 0, 0, 0),
        plot.background = element_rect(fill = "#fbfcfc", colour = NA),
        legend.position = "bottom", #c(0.5, 0),
        legend.direction = "horizontal",
        legend.key.height = unit(2.5, "mm")) 
  

goal_leaders <- ggplot(bkg_rect, aes(x = year)) +
  geom_rect(aes(xmin = year - 0.3, xmax = year + 0.3, ymin = min_goals, ymax = max_goals), fill = "#e5e5e5", color = "#e5e5e5") +
  geom_rect(aes(xmin = 2004 - 0.3, xmax = 2004 + 0.3, ymin = 0, ymax = 60), fill = "#FFFFFF", color = "#363636", size = 0.5) +
  geom_segment(data = plot_data, aes(x = year - 0.3, xend = year + 0.3, y = goals, yend = goals, color = color, alpha = alpha, size = size)) +
  geom_mark_circle(data = annotations, aes(y = goals, group = player, filter = str_detect(player, "Ovechkin"), label = glue("{year} Goal Leader"), description = glue("{player}: {goals} goals")), label.family = c("Oswald"), expand = unit(3, "mm"), label.colour = c("black", "#041E42"), label.fontface = c("plain", "bold"), label.fill = "#fbfcfc", label.buffer = unit(20, "mm")) +
  geom_mark_circle(data = annotations, aes(y = goals, group = player, filter = str_detect(player, "Hull"), label = glue("{year} Goal Leader"), description = glue("{player}: {goals} goals")), label.family = c("Oswald"), expand = unit(3, "mm"), label.colour = c("black", "#FFB81C"), label.fontface = c("plain", "bold"), label.fill = "#fbfcfc", label.buffer = unit(20, "mm")) + 
  geom_mark_circle(data = annotations, aes(y = goals, group = player, filter = str_detect(player, "Maurice"), label = glue("{year} Goal Leader"), description = glue("{player}: {goals} goals")), label.family = c("Oswald"), expand = unit(3, "mm"), label.colour = c("black", "#AF1E2D"), label.fontface = c("plain", "bold"), label.fill = "#fbfcfc") + 
  scale_x_continuous(limits = c(1943, 2019), expand = c(0,0), breaks = seq(1940, 2020, 10)) +
  scale_y_continuous(breaks = seq(0, 120, 20)) +
  labs(x = NULL, 
       y = NULL) +
  scale_color_identity() +
  scale_size_identity() +
  scale_alpha_identity() +
  theme_jk(grid = "Y",
           markdown = TRUE) +
  theme(plot.margin = margin(0, 0, 0, 0),
        axis.text.y = element_blank(),
        plot.background = element_rect(fill = "#fbfcfc", colour = NA))

subtitle <- glue("{paste0(map2_chr(annotations$player, annotations$color, ~highlight_text(.x, .y, 'b')), collapse = ', ') %>% 
  stringi::stri_replace_last_regex(',', ' and')} have recorded 7 seasons as the leading goal scorer in the NHL. Their journey is shown below with each bar representing<br>a season from 1944 to 2019, each line in the bar represents the goals scored by one of the top 250 goal scorers in the NHL. The blank bar and square are indicative of the<br>2004-05 NHL Lockout.")

out <- wrap_plots(avg_scoring, goal_leaders, ncol = 1, heights = c(0.05, 0.95), widths = c(1, 1)) +
  plot_annotation(title = "The Three Way Tie for the Record of Most Seasons as the Leading Goal Scorer in the NHL",
                  subtitle = subtitle,
                  caption = "**Data**: hockey-reference.com | **Graphic**: @jakekaupp",
                  theme = theme_jk(markdown = TRUE) + theme(plot.background = element_rect(fill = "#fbfcfc", colour = NA))) 

ggsave(here("2020", "week10", "tw10_plot.png"), out, width = 14, height = 9, dev = ragg::agg_png())


