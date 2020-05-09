library(tidyverse)
library(here)
library(jkmisc)
library(janitor)
library(rvest)
library(ggtext)
library(glue)
library(fs)
library(colorspace)

items <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/items.csv')

url <- "https://nookipedia.com/wiki/Fish/New_Horizons"

nh_fish <- read_html(url) %>% 
  html_node(".sortable") %>% 
  html_table() %>% 
  clean_names()

images <- read_html(url) %>% 
  html_nodes("img") %>% 
  html_attr("src") %>% 
  .[1:80]

month_pre <- read_html(url) %>% 
  html_nodes("td") %>% 
  html_nodes("span") %>% 
  as.character() %>% 
  enframe(value = "nodes")

out <- month_pre %>% 
  filter(str_detect(nodes, '^<span style=\"color: #DFDFDF\">')) %>% 
  pull(name) %>% 
  diff() 
  
month_pre <- imap(out, ~rep(.y, each = .x)) %>% 
  flatten_dbl() %>% 
  c(., c(160, 160)) %>% 
  bind_cols(month_pre)

months <- month_pre %>% 
  filter(str_detect(nodes, '^<span style=\"color: #DFDFDF\">')) %>%
  mutate(name = rep(1:80, each = 2)) %>%
  left_join(month_pre, .) %>%
  fill(name, .direction = "down") %>% 
  filter(str_detect(nodes, '^<span style=\"color: #DFDFDF\">', negate = TRUE)) %>%
  mutate(months = str_extract_all(nodes, '(?<=<span style=\"font-weight: 600; color: #50b3d4\">)([A-Z ]+)(?=</span>)')) %>% 
  group_by(...1) %>% 
  summarize(months = paste0(months, collapse = "")) %>% 
  ungroup() %>% 
  mutate(region = rep_along(months, 1:2),
         region = factor(region, 1:2, c("North", "South"))) %>% 
  mutate(number = rep(1:80, each = 2)) %>% 
  select(number, region, months)


string <- "J F M A M J J A S O N D"

replace <- str_split("1,2,3,4,5,6,7,8,9,10,11,12", ",") %>% 
  flatten_chr()

string <- str_replace_all(string, " ", ",") %>% 
  str_split(",") %>% 
  flatten_chr()

new_fish <- nh_fish %>% 
  select(-months) %>% 
  left_join(months, by = "number") %>% 
  mutate(months_num = str_replace_all(months, set_names(replace, string))) %>% 
  mutate(months_num = str_replace_all(months_num, "3 4 3 1 1(?!\\d)", "3 4 5 6 7 8")) %>% 
  mutate(months_num = str_replace_all(months_num, "3 1 1 4", "5 6 7 8")) %>% 
  mutate(months_num = str_replace_all(months_num, "3 4 3 1(?!\\d)", "3 4 5 6")) %>% 
  mutate(months_num = str_replace_all(months_num, "3 4 3", "3 4 5")) %>% 
  mutate(months_num = str_replace_all(months_num, "1 1 4", "6 7 8")) %>% 
  mutate(months_num = str_replace_all(months_num, "1 4 9", "7 8 9")) %>% 
  mutate(months_num = str_replace_all(months_num, "4 9", "8 9")) %>% 
  mutate(months_num = str_replace_all(months_num, "1 1(?!\\d)", "8 9")) %>% 
  mutate(months_num = str_replace_all(months_num, "3 4 5 6 7 8 8 9", "3 4 5 6 7 8 9")) %>% 
  mutate(months_num = trimws(months_num)) %>% 
  mutate(time = case_when(time == "All day" ~ list(0:24),
                          time == "All Day" ~ list(0:24),
                          time == "9AM - 4PM" ~ list(9:16),
                          time == "4PM - 9AM" ~ list(c(16:24, 0:9)),
                          time == "-" ~ list(0:24),
                          time == "9PM - 4AM" ~ list(c(21:24, 0:4)),
                          time == "4PM to 9AM" ~ list(c(16:24, 0:9)),
                          time == "4AM - 9PM" ~ list(4:21),
                          time == "4AM- 9PM" ~ list(4:21),
                          time == "One Day: 9AM - 4PM  Next Day: 9PM - 4AM" ~ list(4:16)
  )) %>% 
  separate_rows(months_num, sep = " ") %>% 
  unnest(time)

sunrise <- colorRampPalette(c("#062B79", "#16498A", "#5995B7", "#FAFBBD", "#FDE050", "#F1B351"))(12)

sunset <- c("#F2671F", "#C91B26", "#9C0F5F", "#60047A")
  
midnight <- colorRampPalette(c("#855988", "#6B4984", "#483475", "#2B2F77", "#141852", "#070B34"))(10)

colors <- set_names(c(sunrise, sunset, midnight, "grey80"), c(0:24, 99))

icons <- here("2020", "week19", "img") %>% 
  dir_ls() %>% 
  glue_data("<img src='{.}' width = '14'/><br>{c('24:00','00:00','12:00', '6:00', '18:00')}") %>% 
  set_names(c("high_moon", "moon", "sun", "sunrise", "sunset"))

labels <- nh_fish %>% 
  select(name, price, location) %>% 
  mutate(label = glue("<img src='{images}' width = '24' height = '24'/>{name} : {price} <img src='https://dodo.ac/np/images/5/52/100_Bells_NH_Icon.png' width = '24' height = '24'/>"))
  
plot_data <- new_fish %>% 
  left_join(labels) %>% 
  mutate(fill = "#66B888") %>% 
  mutate(months_num = as.numeric(months_num)) %>% 
  complete(nesting(name, number, region, label, location), months_num = 1:12, time = 0:24, fill = list(fill = darken("#EBEDDF")))

sea_plot <- plot_data %>% 
  filter(str_detect(location, "Sea")) %>%
  ggplot(aes(x = time, y = months_num)) +
  geom_tile(aes(fill = fill), show.legend = FALSE, color = "#39383D", size = 0.2) +
  facet_wrap(~label, ncol = 5) +
  scale_fill_identity() +
  scale_y_continuous(expand = c(0.1, 0), breaks = 1:12, labels = month.abb, trans = "reverse") +
  scale_x_continuous(breaks = c(0, 6, 12, 18, 24), labels = icons[c("moon", "sunrise", "sun", "sunset", "high_moon")]) +
  #coord_equal() +
  labs(x = NULL, 
       y = NULL,
       title = "The Fisherman's Almanac: Deep Sea Fishing in Animal Crossing New Horizons",
       subtitle = "A handy depiction of where and when to fish in the seas of **northern regions** whether for sport, collections or shells.",
       caption = "**Data**: nookipedia.com | **Graphic**: @jakekaupp") +
  theme_jk(grid = FALSE,
           ticks = TRUE,
           markdown = TRUE,
          ) +
  theme(plot.background = element_rect(fill = "#EBEDDF", color = NA),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        strip.text.x = element_markdown(family = "FinkHeavy"),
        axis.text = element_markdown(family = "FinkHeavy"))

mc_plot_data <- new_fish %>% 
  left_join(labels) %>% 
  mutate(fill = time) %>% 
  mutate(months_num = as.numeric(months_num)) %>% 
  complete(nesting(name, number, region, label, location), months_num = 1:12, time = 0:24, fill = list(fill = 99))


multi_sea_plot <- mc_plot_data %>% 
  filter(str_detect(location, "Sea")) %>%
  ggplot(aes(x = time, y = months_num)) +
  geom_tile(aes(fill = factor(fill)), show.legend = FALSE, color = "#39383D", size = 0.2) +
  facet_wrap(~label, ncol = 5) +
  scale_fill_manual(values = colors) +
  scale_y_continuous(expand = c(0.1, 0), breaks = 1:12, labels = month.abb, trans = "reverse") +
  scale_x_continuous(breaks = c(0, 6, 12, 18, 24), labels = icons[c("moon", "sunrise", "sun", "sunset", "high_moon")]) +
  #coord_equal() +
  labs(x = NULL, 
       y = NULL,
       title = "The Fisherman's Almanac: Deep Sea Fishing in Animal Crossing New Horizons",
       subtitle = "A handy depiction of where and when to fish in the seas of **northern regions** whether for sport, collections or shells.",
       caption = "**Data**: nookipedia.com | **Graphic**: @jakekaupp") +
  theme_jk(grid = FALSE,
           ticks = TRUE,
           markdown = TRUE,
  ) +
  theme(plot.background = element_rect(fill = "#EBEDDF", color = NA),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        strip.text.x = element_markdown(family = "FinkHeavy"),
        axis.text = element_markdown(family = "FinkHeavy"))


ggsave(here("2020", "week19", "tw19_sea_plot.png"), sea_plot, width = 18, height = 14)

ggsave(here("2020", "week19", "tw19_mc_sea_plot.png"), multi_sea_plot, width = 18, height = 14)
