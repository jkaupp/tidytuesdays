library(tidyverse)
library(here)
library(jkmisc)
library(colorspace)
library(ggtext)
library(scales)
library(glue)

source(here("2020", "week17", "R", "packed_bars.R"))

gdpr_violations <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_violations.tsv')

#Stacked bar -----

plot_data <- gdpr_violations %>% 
  group_by(controller) %>% 
  mutate(controller = if_else(str_detect(controller, "Google"), "Google", controller)) %>% 
  summarize(price = sum(price),
            number = n()) %>% 
  pack_bars(6, price, fill_color = "#2f9c95")


stacked <- ggplot(plot_data) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill, color = darken(fill, 0.2)), size = 0.2) +
  geom_text(data = filter(plot_data, fill == "#2f9c95"), aes(x = xmin, y = (ymin + ymax)/2, label = controller, color = darken(fill, 0.6)), family = "Oswald",  nudge_x = 100000, hjust = 0, size = 5) +
  scale_fill_identity() +
  scale_color_identity() +
  scale_x_continuous(labels = scales::dollar, limits = c(0, 60000000), breaks = seq(0, 60000000, 10000000)) +
  labs(x = NULL,
       y = NULL,
       title = "Six Companies Have the Dubious Honour of GDPR Violations High Scores",
       subtitle = glue("Illustrated below is a packed bar chart for total GDPR violations by company from 2017-2019. The {highlight_text('Top 6 Companies', '#2f9c95', 'b', 16)} are<br>being levied multi-million dollar fines with the {highlight_text('remaining 179 violations', 'grey60', 'b', 16)} being fined much less."),
       caption = "**Data**: Privacy Affairs | **Graphic**: @jakekaupp") +
  theme_jk(grid = "X", 
           ticks = TRUE,
           markdown = TRUE,
           dark = TRUE) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_markdown(size = 12),
        axis.ticks = element_line(color = "#E5E9F0"))

#HS Plot ----

high_score <- gdpr_violations %>% 
  group_by(controller) %>% 
  mutate(controller = if_else(str_detect(controller, "Google"), "Google", controller)) %>% 
  summarize(price = sum(price)) %>% 
  arrange(desc(price)) %>% 
  slice(1:10) %>% 
  mutate(idx = rev(row_number()),
         place = as.character(row_number()),
         price = dollar(price)) %>% 
  pivot_longer(-idx) %>% 
  mutate(hjust = case_when(name == "price" ~ 1,
                           name == "controller" ~ 0,
                           TRUE ~ 1)) %>% 
  mutate(x = case_when(name == "place" ~ 0.9,
                       name == "price" ~ 1.7,
                       name == "controller" ~ 1.9)) %>% 
  mutate(xend = case_when(idx == 1 ~ 5,
                          TRUE ~ 3))

prices <- plot_data %>% 
  slice(1:10) %>% 
  pull(price)

segments <- tibble(price = prices,
                   x = 1.9,
                   xend = 1.9 + 4*price/max(price),
                   idx = rev(1:10))


hs_plot <- ggplot(high_score, aes(x = x, y = idx)) +
  geom_segment(data = segments, aes(x = x, xend = xend, y = idx, yend = idx), color = "#2f9c95", size = 9, alpha = 0.5) +
  geom_text(aes(label = value, hjust = hjust), family = "Hyperspace Bold", color = "white", size = 6) +
  annotate("text", x = 2, y = 12, label = "GDPR VIOLATIONS HIGH SCORES", family = "Hyperspace Bold", color = "white", size = 8) +
  annotate("text", x = 2, y = -2, label = "1   COIN  1  PLAY", family = "Hyperspace Bold", color = "white", size = 8) +
  annotate("text", x = 2, y = -4, label = "2020 Graphic: @jakekaupp | Data: Privacy Affairs", family = "Hyperspace Bold", color = "white", size = 4) +
  scale_x_discrete(limits = c("place", "price", "controller")) +
  labs(x = NULL,
       y = NULL) +
  theme(plot.background = element_rect(fill = "#111111", color = NA),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "#111111", color = NA),
        axis.ticks = element_blank(),
        axis.text = element_blank())

ggsave(here('2020', 'week17', 'tw17_stacked_plot.png'), stacked, width = 10, height = 6, device = ragg::agg_png())

ggsave(here('2020', 'week17', 'tw17_hs_plot.png'), hs_plot, width = 10, height = 6, device = ragg::agg_png())

