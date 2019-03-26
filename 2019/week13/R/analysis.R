library(tidyverse)
library(lubridate)
library(here)
library(fs)
library(janitor)
library(jkmisc)

pet_licenses <- here("2019", "week13", "data") %>% 
  dir_ls(regexp = "Seattle") %>% 
  read_csv() %>% 
  clean_names() %>% 
  mutate_at("license_issue_date", mdy)

burst_name <- function(df) { 
  
  df %>% 
    distinct(license_issue_date) %>% 
    pull(license_issue_date) %>% 
    kleinberg()
    
  }

out <- pet_licenses %>% 
  filter(!is.na(animals_name)) %>% 
  mutate(animals_name = str_to_lower(animals_name)) %>% 
  group_by(animals_name) %>% 
  filter(n() >= 100) %>% 
  nest() %>% 
  mutate(bursts = map(data, burst_name)) %>% 
  unnest(bursts) %>% 
  arrange(desc(animals_name), level) %>% 
  mutate(id = ntile(animals_name, 1)) %>%
  mutate(color = case_when(level == 1 ~ "grey50",
                           level == 2 ~ "#6baed6",
                           level == 3 ~ "#3182bd",
                           level == 4 ~ "#08519c"),
         alpha = case_when(level == 1 ~ 0.5,
                           level == 2 ~ 1,
                           level == 3 ~ 1,
                           level == 4 ~ 1)) %>% 
  ungroup()

facet_labels <- out %>% 
  group_by(id) %>% 
  summarize(label = sprintf("%s to %s", last(str_sub(animals_name, 1, 1)), first(str_sub(animals_name, 1, 1)))) %>% 
  pull(label) %>% 
  set_names(., sort(unique(out$id)))

order <- out %>% 
  filter(level == 1) %>% 
  arrange(desc(start)) %>% 
  pull(animals_name)

plot <- ggplot(out) +
  geom_segment(aes(x = start, xend = end, y = factor(animals_name, order), yend = factor(animals_name, order), color = color, alpha = alpha), size = 4, lineend = "square") +
  scale_color_identity() +
  scale_alpha_identity() +
  scale_x_date(limits = c(ymd("2006/01/01"),ymd("2019/01/01")),  date_breaks = "1 year", date_labels = "%Y", expand = c(0.02, 0)) +
  scale_y_discrete(position = "right") +
  theme_jk(grid = "XY") +
  labs(x = NULL,
       y = NULL,
       title = "What is it, Lassie? 'Bark! Bark-bark-bark! Bark-bark!' What, Timmy's fallen in the well?",
       subtitle = str_wrap("Illustrated below is the recorded use of and bursts in popularity of registered pet names (frequency of use > 100) in Seattle from 2006 to 2019.  The grey bar indicates the duration the name is in use, and the blue segments indicate bursts of increased use of the name.  Darker blue segments represent repeated bursts indicating an increased intensity of use.", 100),
       caption = "Data: seattle.gov | Graphic: @jakekaupp")

ggsave(here("2019","week13","tw13_plot.png"), plot, width = 8, height = 10, type = "cairo-png")
