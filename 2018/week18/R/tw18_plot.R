library(here)
library(tidyverse)
library(treemap)
library(sysfonts)
library(showtext)
library(grid)
library(nord)
library(readxl)
library(ggalt)
library(jkmisc)


raw_data <- read_xlsx(here("week18", "data", "week18_dallas_animals.xlsx"), sheet = 1)

data <- raw_data %>% 
  filter(animal_type %in% c("CAT","DOG"), mo_year == "2017") %>% 
  count(animal_type, month, mo_year, outcome_type) %>% 
  group_by(animal_type, month, mo_year) %>% 
  mutate(percent = n/sum(n)) %>% 
  ungroup() %>% 
  select(-n) %>% 
  filter(outcome_type %in% c("EUTHANIZED","ADOPTION")) %>% 
  mutate_if(is.character, tolower) %>%
  spread(outcome_type, percent) %>% 
  mutate_if(is.character, tools::toTitleCase) %>% 
  mutate(month = ifelse(month == "may", "May", month)) %>% 
  arrange(month) %>% 
  complete(month = month.abb, mo_year, animal_type, fill = list(adoption = NA, euthanized = NA)) %>% 
  mutate(ratio = adoption/euthanized) %>% 
  mutate(month = factor(month, month.abb))



ggplot(data, aes(x = month, y = ratio, group = animal_type, color = animal_type)) +
  geom_hline(yintercept = 1.0, size = 0.1, color = "firebrick", linetype = "dashed") +
  geom_line(size = 0.5) +
  geom_text(data = filter(data, month == "Sep"), aes(label = animal_type), nudge_x = 0.3, family = "Oswald") +
  theme_jk(grid = "XY") +
  scale_color_nord("victory_bonds") +
  labs(x = NULL, y = "Ratio of Adopted/Euthanized", title = "2017 was a bad time to be a cat in a shelter", subtitle = "Cats in the shelters were euthanized more than adopted compared to dogs.") +
  theme(legend.position = "none")
  



