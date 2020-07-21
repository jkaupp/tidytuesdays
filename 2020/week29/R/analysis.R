library(tidyverse)
library(jkmisc)
library(glue)
library(lubridate)
library(here)
library(rvest)
library(janitor)
library(ggbeeswarm)
library(ggforce)
library(nord)
library(colorspace)
library(scales)
library(ggtext)

astronauts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-14/astronauts.csv')

spacefacts <- "http://spacefacts.de/english/e_missions.htm"

sf_html <- read_html(spacefacts)

sf_table <- sf_html %>%
  html_table(header = TRUE) %>%
  first() %>%
  as_tibble() %>%
  fill(Nation, .direction = "down") %>%
  clean_names() %>%
  filter(no != nation) %>%
  rename(year = nation) %>%
  mutate(across(c(time, main_tasks), ~str_remove_all(.x, "[\r\n\t]"))) %>%
  mutate(mission = case_when(mission == "Vostok" ~ "Vostok 1",
                             TRUE ~ mission))


missions <- astronauts %>% 
  mutate(mission_title = case_when(str_detect(mission_title, "^\\d+$") ~ ascend_shuttle,
                                   TRUE ~ mission_title)) %>% 
  mutate(mission_title = str_remove(mission_title, "/\\d+"),
         mission_title = str_replace(mission_title, "(\\d+)([- ])(\\w)", "\\1\\3"),
         mission_title = str_replace(mission_title, "sts", "STS"),
         mission_title = str_replace(mission_title, "Mercury-Atlas", "Mercury")) %>% 
  mutate(year_of_mission = case_when(mission_title == "STS-51F" ~ 1985,
                                     mission_title == "STS-77" ~ 1996,
                                     mission_title == "Soyuz 7" ~ 1969,
                                     mission_title == "STS-30" ~ 1989,
                                     mission_title == "STS-61" ~ 1993,
                                     mission_title == "Soyuz TMA-11" ~ 2007,
                                     TRUE ~ year_of_mission)) %>% 
  mutate(mission_title = case_when(mission_title == "apollo 9" ~ "Apollo 9",
                                   mission_title == "Voskhod 1" ~ "Voskhod",
                                   mission_title == "gemini 6A" ~ "Gemini 6A",
                                   mission_title == "Skylab-2" ~ "Skylab 2",
                                   mission_title == "Saluyt 4" ~ "Salyut 4",
                                   mission_title == "Soyuz 19" ~ "Soyuz 19 EPAS",
                                   mission_title == "MA-6" ~ "Mercury 6",
                                   mission_title == 'Soyuz TMA-12 / Soyuz TMA-11' ~ "Soyuz TMA-12",
                                   mission_title == "Soyuz TMA­1/TM­34" ~ "Soyuz TMA-1",
                                   TRUE ~ mission_title)) %>% 
  mutate(mission_title = str_replace(mission_title, "(Soyuz T)(\\d)", "\\1-\\2"),
         mission_title = str_replace(mission_title, "(Soyuz TM)­(\\d)", "\\1-\\2"),
         mission_title = str_replace(mission_title, "(Soyuz T)­(\\d)", "\\1-\\2"),
         mission_title = str_replace(mission_title, "STS 51", "STS-51")) %>% 
  filter(!(mission_title == "Skylab 4" & year_of_mission == 1974),
         mission_title != "STS-151C",
         mission_title != "STS-51L",
         mission_title != "Soyuz TM13") %>% 
  group_by(year_of_mission, mission_title) %>%  
  summarize(hours_mission = max(hours_mission),
            eva_hrs_mission = max(eva_hrs_mission))

full <- left_join(missions, sf_table, by = c("mission_title" = "mission")) %>% 
  filter(!is.na(time)) %>% 
  separate(time, into = c("start", "end"), sep = "-") %>% 
  mutate(end = if_else(is.na(end), start, end)) %>% 
  mutate(across(c(start, end),.fns =  dmy)) %>% 
  mutate(year = year(start)) %>% 
  ungroup() %>% 
  arrange(start) %>% 
  mutate(no = row_number()) %>% 
  mutate(alpha = case_when(mission_title %in% c("Voskhod 2", "Apollo 11", "Apollo 17", "STS-41B", "STS-102") ~ 1,
                           TRUE ~ 0.3),
         color = if_else(mission_title %in% c("Voskhod 2", "Apollo 11", "Apollo 17", "STS-41B", "STS-102"), nord_palettes$lumina[2], desaturate(nord_palettes$lumina[1], 1))) %>% 
  mutate(main_tasks = if_else(mission_title == "STS-102", "Longest Individual Spacewalk of 8:56", main_tasks))

labels <- full %>% 
  filter(eva_hrs_mission > 0) %>% 
  filter(mission_title %in% c("Voskhod 2",  "Apollo 11", "Apollo 17", "STS-41B", "STS-102"))

years <- tibble(start = ymd(glue("{seq(1960, 2020, 10)}-01-01")),
                label = seq(1960, 2020, 10),
                eva_hrs_mission = 90)

stations <- tibble(start = ymd("1971-04-19", "1973-05-14", "1986-02-20", "1998-11-20"),
                   end = ymd("1986-07-16", "1979-07-11", "1996-05-23", "2020-01-01"),
                   name = c("Salyuz 1-7", "Skylab", "Mir", "ISS"),
                   eva_hrs_mission = c(-14, -16, -14, -14),
                   color = grey.colors(4))

flights <- tibble(start = ymd("2022-01-01"),
                eva_hrs_mission = seq(0, 80, 20),
                label = c("No EVA", "20", "40", "60", "80 Hrs"))

plot <- ggplot(full, aes(x = start, y = eva_hrs_mission)) +
  geom_text(data = years, aes(label = label), hjust = -0.1, family = "Oswald Bold", color = "white", size = 8, alpha = 0.1) + 
  geom_vline(data = years, aes(xintercept = start), size = 0.1, color = "white", alpha = 0.1) +
  geom_hline(data = flights, aes(yintercept = eva_hrs_mission), size = 0.1, color = "white", alpha = 0.1) +
  geom_segment(data = stations, aes(x = start, xend = end, y = eva_hrs_mission, yend = eva_hrs_mission, color = color), size = 4) +
  geom_text(data = stations, aes(label = name, y = c(-10, -20, -10, -10)), hjust = -0.1, family = "Oswald Bold", color = "white", size = 8, alpha = 0.1) + 
  geom_text(data = flights, aes(label = label), hjust = 0.5, vjust = -0.5, family = "Oswald Bold", color = "white", size = 4, alpha = 0.1) + 
  geom_beeswarm(aes(alpha = alpha, color = color), groupOnX = FALSE,  size = 3) +
  geom_mark_circle(data = labels, aes(label = mission_title, description = main_tasks, group = start), position = position_beeswarm(groupOnX = FALSE), expand = unit(2, "mm"), color = nord_palettes$lumina[1], con.colour = nord_palettes$lumina[1], label.fill = "transparent", label.colour = c(nord_palettes$lumina[2], nord_palettes$lumina[1]), label.family = c("Oswald", "Lato"), label.fontsize = c("20", "12"), label.minwidth = 80, con.border = "none", label.buffer = unit(50, "mm") ) +
  scale_alpha_identity() +
  scale_color_identity() +
  scale_x_date(date_breaks = "10 years") +
  expand_limits(y = c(-20, 90)) +
  labs(x = NULL,
       y = NULL,
       title = "Since We Started Making Space Stations, Everybody Just Wanted To Go Outside",
       subtitle = glue("Illustrated below are the total EVA hours per American and Russian Space Missions since 1960. Overlaid at the bottom are the lifespans of the 10 space stations constructed by Russia or America. The {highlight_text('annotations', nord_palettes$lumina[2], 'b'} highlight notable missions of interest."),
       caption = "**Data**: Astronaut Database & Spacefacts | **Graphic**: @jakekaupp | 'I'm sorry, Dave. I'm afraid I can't do that'") +
  theme_jk(dark = TRUE,
           grid = FALSE,
           markdown = TRUE) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())

ggsave(here("2020", "week29", "tw29_plot.png"), plot, device = ragg::agg_png(width = 1200, height = 600, units = "px"))
  

