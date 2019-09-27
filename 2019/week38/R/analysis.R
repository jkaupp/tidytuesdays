library(tidyverse)
library(rvest)
library(janitor)
library(here)
library(fuzzyjoin)
library(jkmisc)
library(ragg)

# Get park fees
fees_page <- "https://www.nps.gov/aboutus/entrance-fee-prices.htm"

parks <- read_html(fees_page) %>% 
  html_nodes("h3") %>% 
  html_text() %>% 
  .[-1:-2]

park_fees <- read_html(fees_page) %>% 
  html_nodes(".table-wrapper > table") %>% 
  html_table() %>% 
  map(~set_names(.x, c("date", "park_specific_annual_pass", "per_vehicle", "per_person", 
                       "per_motorcycle"))) %>% 
  map2(parks, ~mutate(.x, park = .y)) %>% 
  bind_rows() %>% 
  filter(date == "Current") %>% 
  rename(park_name = park) %>% 
  mutate(park_name = stringi::stri_trans_general(park_name, id = "Latin-ASCII"),
         park_name = str_replace(park_name, "Hawai'i", "Hawaii"))



#udpated data
summary_report <- here("2019", "week38", "data", "annual_summary_report.csv") %>% 
  read_csv() %>% 
  clean_names()

plot_data <- summary_report %>% 
  filter(year == 2018) %>% 
  mutate(visitors = recreation_visitors + non_recreation_visitors) %>% 
  select(year, park_name, visitors) %>% 
  mutate(park_name = str_remove(park_name, "[A-Z]{2,}"),
         park_name = str_remove(park_name, "& PRES"),
         park_name = trimws(park_name)) %>% 
  regex_left_join(park_fees, ., ignore_case = TRUE) %>% 
  distinct(year, park_name.x, .keep_all = TRUE) %>% 
  filter(str_detect(park_name.x, "Park"), !str_detect(park_name.x, "Great Falls")) %>% 
  mutate(revenue = visitors * parse_number(per_person)) %>% 
  rename(park_name = park_name.x) %>% 
  select(-park_name.y)
  

plot <- ggplot(plot_data, aes(x = fct_reorder(park_name, revenue), y = revenue)) +
  geom_col(fill = "#5e81ac", size = 0.1) +
  coord_flip() +
  scale_y_continuous(labels = scales::dollar, expand = c(0.01,0)) +
  labs(title = "Estimated National Park Revenue from Fees for 2018",
       subtitle = str_wrap("Illustrated below is a bar chart of fee revenue from US National Parks in 2018.  Estimated Revenue calculated using per person admittance rates and total park visitors.", 95),
       caption = "Data: www.nps.gov | Graphic: @jakekaupp",
       x = NULL,
       y = NULL) +
  theme_jk(grid = "X") +
  theme(plot.background = element_rect(fill = "#2e3440"),
        text = element_text(color = "#eceff4"),
        panel.grid = element_line(color = "#e5e9f0"),
        axis.text.x = element_text(color = "#eceff4"),
        axis.text.y = element_text(color = "#eceff4"))

ggsave(here("2019", "week38", "tw_38plot.png"), plot, width = 10, height = 8, device = agg_png())

