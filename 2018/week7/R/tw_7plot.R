library(tidyverse)
library(here)
library(janitor)
library(likert)
library(jkmisc)
library(nord)

star_wars <- dir(here("week7", "data"), pattern = "StarWars", full.names = TRUE) %>% 
  read_csv() 

clean_names <- stringi::stri_trans_general(names(star_wars), "latin-ascii") %>% 
  gsub("[^\\x{00}-\\x{7f}]", "", ., perl = TRUE) %>% 
  clean_names()

star_wars <- set_names(star_wars, clean_names) 

headers <- slice(star_wars, 1) %>% 
  flatten_chr()

clean_names <- gsub("X\\d+", NA_character_, clean_names) %>% 
  enframe() %>% 
  fill(value) %>% 
  pull(value)


shiny_clean_names <- paste(clean_names, headers, sep = "|")

long_star_wars <- set_names(star_wars, c("RespondentID", shiny_clean_names[-1])) %>% 
  slice(-1) %>% 
  gather(item, value, -1) %>% 
  separate(item, c("question", "category"), sep = "\\|") %>% 
  mutate(category = if_else(category == "Response", NA_character_, category)) %>% 
  mutate(index = group_indices(., question))


plot_data <- long_star_wars %>% 
  filter(index == 12) %>% 
  replace_na(list(value = "Unfamiliar (N/A)")) %>% 
  filter(value != "Unfamiliar (N/A)") %>% 
  spread(category, value) %>% 
  mutate_at(vars(-RespondentID, -question, -index), function(x)
    factor(x, 
            levels = c("Very unfavorably", "Somewhat unfavorably","Neither favorably nor unfavorably (neutral)", "Somewhat favorably", "Very favorably"),
            labels = 1:5
    )) 
            
            
likert_data <- plot_data %>% 
  select(-RespondentID, -question, -index) %>%
  as.data.frame() %>% 
  likert()


ggplot2::update_geom_defaults("text", list(family = "Scope One", size = 4))
  
plot <- likert.bar.plot(likert_data) + 
  scale_fill_nord("mountain_forms", labels = c("Very unfavorably", "Somewhat unfavorably","Neither favorably nor unfavorably (neutral)", "Somewhat favorably", "Very favorably"), name = "Response") +
  labs(title = "The Favorability Rankings of Star Wars Characters", subtitle = "People look favourably upon the scruffy nerf herder, and would give a ride to the EVIL RAISIN THAT SHOOTS LIGHTNING FROM HIS HANDS before the goofy gungan.") +
  theme_jk(grid = "XY") +
  theme(plot.title = element_text(family = "Oswald"))

ggsave(here("week7", "tw7_likert.png"), width = 16, height = 10)
  
  
 
