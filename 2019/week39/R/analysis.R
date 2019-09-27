library(tidyverse)
library(janitor)
library(tidycensus)
library(glue)
library(here)
library(sf)
library(tigris)
library(jkmisc)
library(ggtext)

school_diversity <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-24/school_diversity.csv")

## Getting the ACS Survey data ----
acs_var <- load_variables(2017, "acs1", cache = TRUE)

race_vars <- filter(acs_var, concept == "RACE") %>% 
  select(name, label) %>% 
  separate(label, c("estimate", "total", "type"), sep = "!!") %>% 
  mutate(type = coalesce(type, total)) %>% 
  select(name, label = type)

if (!file.exists(here("2019", "week39", "data", "acs_race.RDS"))) {
  
  acs_race <- map_df(state.abb, ~get_acs(geography = "school district (unified)", 
                                         variables = race_vars$name,
                                         state = .x))
  
  saveRDS(acs_race, here("2019", "week39", "data", "acs_race.RDS"))
  
} else {
  
  acs_race <- readRDS(here("2019", "week39", "data", "acs_race.RDS"))
}





# Recoding ACS and aggregating data, sadly not easy to determine Hispanic origin ----
# Following methodology from WaPo repo, recoding Native Hawaiian and Pacifici Islander into Asian.
diversity_data <- acs_race %>% 
  left_join(race_vars, by = c("variable" = "name")) %>% 
  mutate(label = case_when(label == "White alone" ~ "White",
                           label == "Black or African American alone" ~ "Black",
                           label == "American Indian and Alaska Native alone" ~ "AIAN",
                           label == "Native Hawaiian and Other Pacific Islander alone" ~ "Asian",
                           label == "Asian alone" ~ "Asian",
                           label == "Two or more races" ~ "Multi",
                           label == "Some other race alone" ~ "Other",
                           TRUE ~ label)) %>% 
  group_by(GEOID, NAME, label) %>% 
  summarize_at(vars(estimate), sum) 


# Using Simpson's Diversity Index instead of max race metrics for diversity----
totals <- diversity_data %>% 
  summarize(total = sum(estimate)*(sum(estimate)-1))

dvs_score <- diversity_data %>% 
  filter(label != "Total") %>% 
  mutate(es_minus = estimate-1) %>% 
  summarize(numerator = sum(estimate*es_minus)) %>% 
  left_join(totals) %>% 
  mutate(diversity = 1 - numerator/total) %>% 
  select(GEOID, NAME, diversity) 
  
acs_diversity <- diversity_data %>% 
  spread(label, estimate) %>% 
  select(-Total) %>% 
  left_join(dvs_score) %>% 
  rename(acs_diversity = diversity) %>% 
  ungroup() %>% 
  mutate(NAME = tolower(NAME),
         NAME = str_remove(NAME, "\\(.+\\)"),
         NAME = str_replace_all(NAME, ";", ",")) %>% 
  separate(NAME, c("NAME", "state"), sep = ",") %>% 
  mutate(NAME = str_remove_all(NAME, "school district*+")) %>% 
  select(GEOID, acs_diversity)

# Use WaPo data and calculate Simpson's Diversity Index
upd_school <- school_diversity %>% 
  filter(SCHOOL_YEAR == "2016-2017") %>% 
  select(LEAID, LEA_NAME, ST, SCHOOL_YEAR, AIAN:Total) %>% 
  pivot_longer(AIAN:Multi, "race", "value") %>% 
  mutate(n = floor(Total * value))

school_totals <- upd_school %>% 
  group_by(LEAID, LEA_NAME, ST, SCHOOL_YEAR) %>% 
  summarize(total = sum(n)*(sum(n)-1))
  
upd_school_dvs <- upd_school %>% 
  group_by(LEAID, LEA_NAME, ST, SCHOOL_YEAR) %>% 
  mutate(n_minus = n-1) %>% 
  summarize(numerator = sum(n*n_minus)) %>% 
  left_join(school_totals) %>% 
  mutate(diversity = 1 - numerator/total) %>% 
  select(GEOID = LEAID, NAME = LEA_NAME, ST, school_diversity = diversity) 

# Environment Cleanup---
remove(list = ls()[str_which(ls(), "upd_school_dvs|acs_diversity", negate = TRUE)])

# Comparing the two diversity measures and creating the ratio ----
overall_diversity <- upd_school_dvs %>% 
  left_join(acs_diversity, by = "GEOID") %>% 
  mutate(ratio = school_diversity/acs_diversity) %>% 
  filter(!is.na(acs_diversity))

# Get School District maps ----

if (!file.exists(here("2019", "week39", "data", "district_maps.RDS"))) {
  
  district_maps <- map(state.abb, ~school_districts(.x, class = "sf"))
  
  saveRDS(district_maps, here("2019", "week39", "data", "district_maps.RDS"))
  
} else {
  
  district_maps <- readRDS(here("2019", "week39", "data", "district_maps.RDS"))
}



# One do.call to keep them all, and in the shallows bind them----
maps <- do.call(sf:::rbind.sf, district_maps)

plot <- overall_diversity %>%
  right_join(maps, ., by = "GEOID") %>%
  filter(!ST %in% c("HI", "AK")) %>%
  ggplot() +
  geom_sf(aes(fill = ratio), color = 'white', size = 0.01) +
  scale_fill_viridis_c("Alignment Ratio", option = "cividis", limits = c(0, 1), labels = scales::percent, na.value = "white") +
  coord_sf(crs = 26915) +
  labs(title = "Is Diversity In School Districts Reflected In The Diversity Of The General Population?",
       subtitle = glue("Shown below is a choropleth map illustrating the ratio between the Diversity Index of a School Population and the Diversity Index of the General Population in that School District in 2017.<br>
       The more {highlight_text('yellow', '#FFEA46', 'b')} an area, the greater alignment between diversity indices.  The more {highlight_text('blue', '#00204D', 'b')} an area, the greater the difference between the diversity of the school and the general populace.<br> 
       This analysis focused on unified school districts and available data on race from the ACS Survey.  Diversity was calculated using Simpson's Diversity Index."),
       caption = "Data: **Washington Post via @dataKateR & American Community Survey** | Graphic: **@jakekaupp**") +
  theme_jk(grid = FALSE,
          markdown = TRUE) +
  theme(axis.text = element_blank())

ggsave(here("2019", "week39", "tw39_plot.png"), width = 16, height = 10, device = ragg::agg_png())
