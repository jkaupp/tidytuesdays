library(tidyverse)
library(jkmisc)
library(here)
library(janitor)
library(ggtext)
library(glue)

hs_students <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-02/hs_students.csv')

hs_data <- hs_students %>% 
  clean_names() %>% 
  rename(year = total) %>% 
  filter(!is.na(year)) %>%
  mutate(year = if_else(year > 10000, as.numeric(str_sub(year, 1, 4)), year)) %>% 
  select(year, white = white1, black = black1, hispanic, asian_pacific_islander_pacific_islander, american_indian_alaska_native, two_or_more_race) %>% 
  mutate(across(-year, ~parse_number(as.character(.x))/100))


line_data <- hs_data  %>% 
  pivot_longer(-year) %>% 
  filter(name %in% c('white', 'black'))
 
ribbon_data <- hs_data %>% 
  select(year, white, black)

dots <- line_data %>% 
  filter(year >= 1940) %>% 
  filter(year %in% range(year))

labels <- dots %>% 
  group_by(year) %>% 
  summarize(diff = max(value) - min(value),
            value = mean(value))

pal <- c("#e01a4f","#53b3cb")


plot <- ggplot() +
  geom_ribbon(data = ribbon_data, aes(x = year, ymin = black, ymax = white), fill = "grey70", alpha = 0.5) +
  geom_line(data = line_data, aes(x = year, y = value, color = name), size = 2) +
  geom_point(data = dots, aes(x = year, y = value, color = name)) +
  geom_text(data = labels, aes(x = year, y = value, label = scales::percent(diff)), family = "Goldman Sans Condensed Regular", nudge_x = c(-2, 2), size = 6) +
  annotate(GeomRichtext, x = 1935, y = 0.95, label = glue("**Close, But Not Enough**: High School Attainment in<br>{highlight_text('Black', '#e01a4f', 'b', size = 50)} and {highlight_text('White', '#53b3cb', 'b', size = 50)} Americans Aged 25 and Older"), hjust = 0, vjust = 1, size = 12, family = "Goldman Sans Condensed", label.color = NA) +
  annotate("text", x = 1975, y = 0.3, label = str_wrap('"The U.S. educational system is one of the most unequal in the industrialized world, and students routinely receive dramatically different learning opportunities based on their social status. Despite stark differences in funding, teacher quality, curriculum, and class sizes, the prevailing view is that if students do not achieve, it is their own fault. If we are ever to get beyond the problem of the color line, we must confront and address these inequalities."', 85), hjust = 0, vjust = 1,  family = "Goldman Sans Condensed", size = 6) +
  annotate(GeomRichtext, x = 1974.8, y = 0.055, label = "**Linda Darling-Hammond** - _Unequal Opportunity: Race and Education_", hjust = 0, vjust = 1,  family = "Goldman Sans Condensed", size = 4, label.color = NA) +
  labs(x = NULL,
       y = NULL,
       caption = "**Data**: NCES | **Graphic**: @jakekaupp") +
  scale_x_continuous(limits = c(1935, 2020), breaks = seq(1940, 2010, 10)) +
  scale_y_continuous(labels = scale_percent_labels, limits = c(0, 1)) +
  scale_color_manual(values = pal) +
  theme_jk(base_family = "Goldman Sans Condensed Regular",
           base_size = 14,
           markdown = TRUE,
           grid = "XY") +
  theme(legend.position = "none")

ggsave(here("2021", "week5", "tw5_plot.png"), plot, width = 16, height = 10)

