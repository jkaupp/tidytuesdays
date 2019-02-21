library(tidyverse)
library(here)
library(readxl)
library(janitor)
library(jkmisc)
library(nord)

oecd_data <- here("2019", "week7", "data", "OECD--1.xlsx") %>% 
  read_excel(skip = 1, na = c("na")) %>% 
  clean_names() %>% 
  filter(!is.na(x1995)) %>% 
  rename(country = x1) %>% 
  gather(year, intensity, -country) %>% 
  arrange(country, year) %>% 
  fill(intensity, .direction =  "down") %>% 
  mutate(year = parse_number(year)) %>% 
  group_by(year) %>% 
  arrange(year, intensity) %>% 
  mutate(rank = row_number(-intensity)) %>% 
  ungroup() %>% 
  mutate(color = if_else(country == "Canada", nord("victory_bonds")[2], nord("snowstorm", 1)),
         text_color = if_else(country == "Canada", nord("snowstorm", 1), "black"))




plot <- ggplot(oecd_data, aes(x = year, y = -rank, group = country)) +
  geom_line(aes(color = color)) +
  geom_point(aes(color = color)) +
  geom_text(data = filter(oecd_data, year == min(year)), aes(label = rank, color = color), x = 1994, hjust = 0, family = "Oswald") +
  geom_text(data = filter(oecd_data, year == max(year)), aes(label = country, color = color), x = 2016.5, hjust = 0, family = "Oswald") +
  expand_limits(x = c(1994, 2019)) +
  scale_x_continuous(breaks = 1995:2016) +
  scale_color_identity() +
  theme_jk(dark = TRUE, grid = FALSE) +
  theme(axis.text.y = element_blank()) +
  labs(x = NULL, 
       y = NULL,
       title = "Canada is Losing a Step In the Global Research Race.",
       subtitle = str_wrap("Shown below is the ranking of research intensity (% of Gross Domestic Product devoted to Research) from 1995-2016. Canada has been on a decline since hitting a peak in 2001.  Most notably is 2009-2016, which coincides with the systematic defunding of Canadian research scientists by the Conservative Harper Government.", 120),
       caption = "Data: OECD | Graphic: @jakekaupp")

ggsave(here("2019", "week7", "tw7_plot.png"), plot, width = 9, height = 4.5)
