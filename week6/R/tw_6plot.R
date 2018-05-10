library(tidyverse)
library(readxl)
library(here)
library(janitor)
library(glue)
library(fuzzyjoin)
library(stringi)
library(ggalt)
library(jkmisc)
library(nord)



provinces <- set_names(c("Alberta", "British Columbia", "Manitoba", "New Brunswick", "Newfoundland and Labrador",
                         "Nova Scotia", "Northwest Territories", "Nunavut", "Ontario", "Prince Edward Island", "Quebec",
                         "Saskatchewan", "Yukon"),
                       c("AB", "BC", "MB", "NB", "NL", "NS", "NT", "NU", "ON", "PE", "QC", "SK", "YT"))

# Just get the Tims data just for Canada
tim_hortons <- dir(here("week6", "data"), full.names = TRUE, pattern = "xlsx") %>% 
  read_excel(sheet = "timhorton") %>% 
  filter(country == "ca") %>% 
  rename(province = state) 

# Counts at the City/Province level
tims_city_prov <- tim_hortons %>% 
  count(city, province)

# Counts at the National level
tims_national <- tim_hortons %>% 
  count(province) %>% 
  mutate(color = ifelse(province == "ON", nord("victory_bonds", 1), "grey50"))

national <- ggplot(tims_national, aes(x = reorder(province,n), y = n)) +
  geom_lollipop(aes(color = color)) +
  scale_color_identity() +
  scale_y_continuous(expand = c(0.01,0.05),  breaks = scales::pretty_breaks()) +
  scale_x_discrete(labels = function(x) provinces[x]) +
  coord_flip() +
  labs(x = NULL, y = NULL, title = "Ontario, we have a problem....", subtitle = "The highest number of Tim Hortons per province goes to Ontario, a land where you can't even get an oat cake.",
       caption = "\nData: timhortons.com | Graphic: @jakekaupp") +
  theme_jk(grid = "Xx")

ggsave(plot = national, here("week6", "National Tims.png"), width = 10, height = 6)

census_2011 <- dir(here("week6", "data"), full.names = TRUE, pattern = "2011 census") %>% 
  read_csv() %>% 
  clean_names() %>% 
  remove_empty("rows") %>% 
  select(city = geographic_name, population = population_2011) %>% 
  mutate(province = stri_extract_last_regex(city, "\\(([A-Za-z\\.]+?)\\)"),
         city = stri_replace_all_regex(city, "\\((.*?)\\)", ""),
         province = gsub("[[:punct:]]", "", province)) %>% 
  mutate(province = case_when(province == "Que" ~ "QC",
                              province == "Ont" ~ "ON",
                              province == "Man" ~ "MB",
                              province == "Sask" ~ "SK",
                              province == "Alta" ~ "AB",
                              province == "NWT" ~ "NT",
                              province == "Nvt" ~ "NU",
                              province == "PEI" ~ "PE",
                              TRUE ~ province)) %>% 
  mutate_if(is.character, trimws)



tims_density <- regex_right_join(census_2011, tims_city_prov,  by = c("city", "province"), ignore_case = TRUE) 


plot_data <- tims_density %>% 
  select(population, city = city.y, province = province.x, n) %>% 
  group_by(city, province) %>% 
  summarize_at(c("n", "population"), sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  filter(population != 0, n > 1, population > 10000) %>% 
  mutate(density = (n/(population/1000))) %>% 
  top_n(25, density) %>% 
  mutate(color = ifelse(density == max(density), nord("victory_bonds", 1), "grey50"))


most_tims <- ggplot(plot_data, aes(x = reorder(city, density), y = density)) +
  geom_lollipop(aes(color = color)) +
  scale_color_identity() +
  scale_y_continuous(expand = c(0,0.01),  breaks = scales::pretty_breaks(), limits = c(0,1.2)) +
  coord_flip() +
  labs(y = "Number of Tim Hortons stores per 1,000 people", x = NULL, title = "However, the title of most Tim Hortons per capita belongs to Cold Lake, Alberta", 
       subtitle = "When looking at towns/cities with population > 10,000 and with more than two Tim Hortons. \nMy hometown of Truro, Nova Scotia comes in a puzzling fourth.",
       caption = "\nData: timhortons.com | Graphic: @jakekaupp") +
  theme_jk(grid = "Xx")

ggsave(plot = most_tims, here("week6", "Most Tims.png"), width = 10, height = 6)

