library(tidyverse)
library(jkmisc)
library(ggrepel)
library(here)

jobs_gender <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-05/jobs_gender.csv")


plot_data <- jobs_gender %>% 
  select(-year) %>% 
  group_by(occupation, major_category, minor_category) %>% 
  summarize_all(mean, na.rm = TRUE) %>% 
  filter(str_detect(occupation, "engineer"), str_detect(major_category, "Engineering")) %>% 
  mutate(diff = if_else((total_earnings_female - total_earnings_male) > 0, "firebrick", "grey80")) %>% 
  mutate(alpha = if_else(diff == "firebrick", 1, 0.2)) %>% 
  gather(variable, value, starts_with("total_earnings_")) %>% 
  mutate(variable = factor(variable, c("total_earnings_male", "total_earnings_female"), c("Men", "Women")))
 
slope_data <- build_slopegraph(plot_data, "variable", "value", "occupation") %>% 
  left_join(distinct(plot_data, occupation, diff, alpha), by = c("group" = "occupation")) %>% 
  mutate(group = case_when(str_detect(group, "Mining") ~ "Mining Engineers",
                                str_detect(group, "Computer") ~ "Computer Engineers",
                                str_detect(group, "Electrical") ~ "Electrical Engineers",
                                str_detect(group, "Marine") ~ "Marine Engineers",
                                str_detect(group, "Industrial") ~ "Industrial Engineers",
                           TRUE ~ str_to_title(group))) %>% 
  mutate(group = str_replace(group, "Engineers", "Engineering"))


labels <- pretty(slope_data$y, 9)
breaks <- pretty(slope_data$ypos, 5)


plot <- ggplot(slope_data, aes(x = x, y = ypos, group = group, color = diff)) +
  geom_point() +
  geom_line() +
  geom_label_repel(data = filter(slope_data, x == "Women"), aes(label = group), direction = "y", hjust = 0, nudge_x = 1, segment.alpha = 0.3, family = "Oswald", label.size = 0, fill = "white") +
  theme_jk(grid = "XY") +
  expand_limits(x = c(0, 5)) +
  scale_color_identity() +
  scale_alpha_identity() +
  scale_y_continuous(labels = scales::dollar(labels), breaks = breaks, limits = range(breaks)) +
  theme(panel.grid.major.x = element_line(linetype = "dashed", color = "black")) +
  labs(x = NULL,
       y = NULL,
       title = "The Unnecessary and Unethical Pay Disparity in Engineering.",
       subtitle = str_wrap("A slopegraph presenting the average total earnings from 2014-2016 for men and women across engineering disciplines.  Mining Engineering is the only discipline with women earning more than men on average.", 70),
       caption = "Data: Census Bureau | Graphic: @jakekaupp")

ggsave(here("2019", "week10", "tw10_plot.png"), plot, width = 6.5, height = 9, type = "cairo")
