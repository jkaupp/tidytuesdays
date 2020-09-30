library(tidyverse)
library(here)
library(jkmisc)
library(glue)
library(ggbump)
library(ggtext)

chopped <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-25/chopped.tsv')

ingredient_list <- chopped %>% 
  pivot_longer(cols = appetizer:dessert, names_to = "type", values_to = "ingredient") %>% 
  distinct(season, season_episode, series_episode, type, ingredient) %>% 
  separate_rows(ingredient, sep = ",") %>% 
  mutate(across(ingredient, trimws)) %>% 
  filter(!is.na(ingredient)) 

multiples <- count(ingredient_list, ingredient, type) %>% 
  count(ingredient) %>% 
  filter(n > 2)

counts <- ingredient_list %>% 
  count(type, ingredient) %>% 
  mutate(type = factor(type, c("appetizer", "entree", "dessert"))) %>% 
  arrange(type, n) %>% 
  semi_join(multiples, by = c("ingredient")) %>% 
  group_by(type) %>% 
  mutate(y_pos = seq_along(ingredient)*2) %>% 
  mutate(hjust = case_when(type == "appetizer" ~ 1,
                           type == "dessert" ~ 0,
                           TRUE ~ 0.5)) %>% 
  mutate(color = case_when(ingredient == "tomatillos" ~ "#f8333c",
                           ingredient == "rainbow carrots" ~ "#f8333c",
                           ingredient == "figs" ~ "#f8333c",
                           ingredient == "almonds" ~ "#00B295",
                           ingredient == "animal crackers" ~ "#00B295",
                           TRUE ~ "black")) %>% 
  mutate(ingredient = case_when(ingredient == "tomatillos" ~ glue("**{ingredient}**"),
                           ingredient == "rainbow carrots" ~ glue("**{ingredient}**"),
                           ingredient == "figs" ~ glue("**{ingredient}**"),
                           ingredient == "almonds" ~ glue("**{ingredient}**"),
                           ingredient == "animal crackers" ~ glue("**{ingredient}**"),
                           TRUE ~ ingredient)) 


edge_lists <- split(counts, counts$type)

edge_set_one <- left_join(edge_lists[[1]], edge_lists[[2]], by = "ingredient") %>% 
  select(ingredient, x = type.x, xend = type.y, y = y_pos.x, yend = y_pos.y)

edge_set_two <- left_join(edge_lists[[2]], edge_lists[[3]], by = "ingredient") %>% 
  select(ingredient, x = type.x, xend = type.y, y = y_pos.x, yend = y_pos.y)

edge_sets <- bind_rows(edge_set_one, edge_set_two) %>% 
  mutate(color = case_when(ingredient == "**tomatillos**" ~ "#f8333c",
                           ingredient == "**rainbow carrots**" ~ "#f8333c",
                           ingredient == "**figs**" ~ "#f8333c",
                           ingredient == "**almonds**" ~ "#00B295",
                           ingredient == "**animal crackers**" ~ "#00B295",
                           TRUE ~ "black")) %>% 
  mutate(size = case_when(str_detect(ingredient, "\\*\\*") ~ 1,
                          TRUE ~ 0.1))

plot <- ggplot() +
  geom_label(data = counts, aes(x = type, y = y_pos, label = ingredient, hjust = hjust), family = "Rubik", alpha = 0, color = NA, label.size = 0) +
  geom_sigmoid(data = edge_sets, aes(x = x, xend = xend, y = y, yend = yend, group = ingredient, color = color, size = size)) +
  geom_richtext(data = counts, aes(x = type, y = y_pos, label = ingredient, hjust = hjust, color = color), family = "Poppins", fill = "#fbf7f4", label.color = NA) +
  geom_richtext(data = distinct(counts, type, hjust), aes(x = type, y = 215, label = str_to_title(type), hjust = hjust), family = "Bebas Neue", size = 10, fill = "#fbf7f4", label.color = NA) +
  scale_color_identity() +
  scale_size_identity() +
  theme_jk(grid = FALSE,
           markdown = TRUE,
           plot_title_family = "Bebas Neue",
           plot_title_size = 30,
           subtitle_size = 15,
           subtitle_family = "Poppins",
           caption_family = "Poppins") +
  labs(x = NULL,
       y = NULL,
       title = "Ingredient Frequency in Chopped Across All Seasons",
       subtitle = str_break(glue("Illustrated below is a visualization of ingredient frequency/use in appetizers, entrees and desserts across all seasons of Chopped.  Only ingredients that appear in each menu item are shown. Ingredients higher on the list are used with greater frequency than those lower on the list.  Relative position in each list is indicated by curved lines, and the {highlight_text('top','#f8333c', 'b', 20)} and {highlight_text('bottom','#00B295', 'b', 20)} items in each category are highlighted."), 100),
       caption = "**Data**: Kaggle | **Graphic**: @jakekaupp") +
  theme(plot.background = element_rect(fill = "#fbf7f4"),
        plot.title = element_markdown(hjust = 0.5),
        plot.subtitle = element_markdown(hjust = 0.5),
        axis.text.y = element_blank(),
        axis.text.x = element_blank())

ggsave(here("2020", "week35", "tw_35plot.png"), plot, width = 12, height = 24)
  
