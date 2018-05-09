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


#
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
  count(province) 

ggplot(tims_national, aes(x = reorder(province,n), y = n)) +
  geom_lollipop(color = nord("halifax_harbor")[5]) +
  scale_y_continuous(expand = c(0.01,0.05),  breaks = scales::pretty_breaks()) +
  scale_x_discrete(labels = function(x) provinces[x]) +
  coord_flip() +
  labs(x = NULL, y = NULL, title = "Ontario, we have a problem....", subtitle = "Number of Tim Horton's stores in each province.") +
  theme_jk(grid = "Xx")

ggsave(here("week6", "National Tims.png"), width = 8, height = 4)

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


tims_density %>% 
  filter(!is.na(population)) %>% 
  select(population, city = city.y, province = province.x, n) %>% 
  group_by(city, province) %>% 
  summarize_at(c("n", "population"), sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  filter(population != 0, n > 1) %>% 
  mutate(density = (population/n)/1000) %>% 
  top_n(-50, density) %>% View()
