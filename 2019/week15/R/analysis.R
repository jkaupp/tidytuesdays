library(tidyverse)
library(lubridate)
library(ggbeeswarm)
library(here)
library(jkmisc)
library(nord)

player_dob <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/player_dob.csv")

grand_slams <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/grand_slams.csv")

grand_slam_timeline <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/grand_slam_timeline.csv")


plot_data <- player_dob %>% 
  select(name, date_of_birth) %>% 
  left_join(grand_slams, by = "name") %>% 
  mutate(age = interval(date_of_birth, tournament_date)/years(1)) %>% 
  group_by(name) %>% 
  filter(n()>1)
  

order <- plot_data %>% 
  group_by(name) %>% 
  filter(rolling_win_count == max(rolling_win_count)) %>% 
  arrange(rolling_win_count) %>% 
  pull(name)
  

plot <- ggplot(plot_data, aes(x = age, y = factor(name, order), size = rolling_win_count, color = gender, alpha = rolling_win_count)) +
  geom_point(aes(group = name)) + 
  facet_wrap(~gender, scales = "free_y") +
  scale_color_manual(values = c("#C01E65","#117AB3")) +
  scale_size_area("Rolling Win Count") +
  guides(size = guide_legend(override.aes = list(shape = 21, color = "black")), color = FALSE, alpha = FALSE) +
  theme_jk(grid = "XY") +
  theme(legend.position = "bottom") +
  labs(x = NULL,
       y = NULL,
       title = "Bright Stardom or Fading Obscurity: Looking at Players Major Wins Across their Careers.",
       subtitle = str_wrap("The chart plots cumulative major wins against player age. Size and transparency of each point are mapped to the cumulative number of majors won.  Looking at the data, we can see the hot streaks in individual players, as well as the dominance of certain champions.", 110),
       caption = "Data: wikipedia | Graphic: @jakekaupp")
  
ggsave(here("2019","week15","tw15_plot.png"), type = "cairo", width = 10, height = 12)
