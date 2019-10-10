library(tidyverse)
library(lubridate)
library(here)
library(janitor)
library(jkmisc)
library(ggforce)
library(ggtext)
library(nord)
library(glue)
library(ragg)

# It's 250mb, can't put it into git, you're going to have to go get it
# from https://openpowerlifting.org/data and stick it in data.
pl_data <- here("2019", "week41", "data") %>% 
  dir(pattern = "openpowerlifting", full.names = TRUE) %>% 
  read_csv() %>% 
  clean_names()

plot_data <- pl_data %>%
  filter_at(vars(starts_with("best")), all_vars(. > 0)) %>% 
  filter(str_detect(place, "1")) %>% 
  mutate(year = year(date)) %>%
  select(-date) %>%
  mutate_at(vars(starts_with("best")), ~./bodyweight_kg) %>% 
  pivot_longer(starts_with("best"), names_to = "lift") %>% 
  group_by(year, sex, lift) %>%
  filter(value == max(value, na.rm = TRUE)) %>% 
  arrange(lift, sex, year)

labels <- tibble(label = c("Bench Press", "Deadlift", "Squat"),
                 lift = c("best3bench_kg", "best3deadlift_kg", "best3squat_kg"),
                 year = 2020,
                 value = 1)

annotations <- plot_data %>% 
  group_by(sex, lift) %>% 
  filter(value == max(value, na.rm = TRUE)) %>% 
  mutate(description = glue("Weight: {bodyweight_kg} kg\nLifted: {bodyweight_kg*value} kg\n{federation}: {meet_name}"),
         name = str_remove(name, "\\#[0-9]"))

plot <- ggplot(plot_data, aes(x = year, y = value)) +
  geom_path(aes(color = sex)) +
  geom_point(aes(fill = sex), shape = 21, color = "#2E3440") +
  geom_text(data = labels, aes(label = label), color = "#E5E9F0", family = "Oswald", fontface = "bold", size = 10, hjust = 1) +
  geom_mark_circle(data = filter(annotations, sex == "M"), aes(color = sex, label = name, description = description), expand = unit(2, "mm"), label.family = c("Oswald", "Lato"), label.fill = "#4C566A", label.colour = "#E5E9F0", con.colour = "#D8DEE9", label.margin = margin(2, 3, 2, 3, "mm")) +
  geom_mark_circle(data = filter(annotations, sex == "F"), aes(color = sex, label = name, description = description), expand = unit(2, "mm"), label.family = c("Oswald", "Lato"), label.fill = "#4C566A", label.colour = "#E5E9F0", con.colour = "#D8DEE9", label.margin = margin(2, 3, 2, 3, "mm")) +
  facet_wrap(~lift) +
  scale_color_manual(values = set_names(c("#314cb6","#DD2A7B"), c("M","F"))) +
  scale_fill_manual(values = set_names(c("#314cb6","#DD2A7B"), c("M","F"))) +
  scale_x_continuous(breaks = seq(1970, 2020, 10)) +
  theme_jk(dark = TRUE, 
           grid = "XY",
           markdown = TRUE) +
  labs(x = NULL,
       y = NULL,
       title = "Evolution of Power: How the Ratio of Bodyweight to Lifted Weight Has Progressed",
       subtitle = glue("Illustrated below is the maximum of the ratio of bodyweight to lifted weight for winning lifts in each year and event for both {highlight_text('Men', '#314cb6', 'b')} and {highlight_text('Women', '#DD2A7B', 'b')} for all meets recorded by Open Powerlifting."),
       caption = "Data: **openpowerlifting.org** | Graphic: **@jakekaupp**") +
  theme(legend.position = "none",
        panel.grid.major = element_line(size = 0.01),
        strip.text = element_blank())

ggsave(here('2019', 'week41', 'tw41_plot.png'), plot, width = 15, height = 8, device = agg_png())