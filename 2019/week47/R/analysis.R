library(tidyverse)
library(lubridate)
library(here)
library(jkmisc)
library(glue)
library(ggforce)
library(ggtext)
  

nz_bird <- here('2019', 'week47', 'data', 'BOTY-votes-2019.csv') %>% 
  read_csv()

bird_long <- nz_bird %>% 
  select(-country) %>% 
  mutate(idx = row_number()) %>% 
  pivot_longer(cols = vote_1:vote_5, names_to = "vote_rank", values_to = "bird_breed") %>% 
  filter(!is.na(vote_rank), !is.na(bird_breed))  
  count(vote_rank, bird)

gunmetal_pal <- c("#AAA9AD", "#848689", "#5B676D", "#2A3439", "#1F262A")
  
  
bird_sets <- nz_bird %>% 
  select(vote_1:vote_5) %>% 
  filter(complete.cases(.)) %>% 
  filter_at(.vars = vars(vote_1:vote_5), .vars_predicate = any_vars(str_detect(., "Yellow-eyed penguin"))) %>% 
  count(vote_1, vote_2, vote_3, vote_4, vote_5, sort = TRUE) %>% 
  filter(vote_1 == "Yellow-eyed penguin") %>% 
  gather_set_data(1:5) %>% 
  group_by(id) %>% 
  mutate(fill = case_when(vote_1 == "Yellow-eyed penguin" ~ "#FFDA29",
                          TRUE ~ gunmetal_pal[5])) %>% 
  mutate(alpha = case_when(vote_1 == "Yellow-eyed penguin" ~ 1,
                          TRUE ~ 0.5))

labels_right <- bird_sets %>% 
  filter(x == "vote_5") 

labels_left <- bird_sets %>% 
  filter(x == "vote_1") 
            
plot <- ggplot(bird_sets, aes(x = x, id = id, split = y, value = n)) +
  geom_parallel_sets(aes(fill = fill), axis.width = 0.05, sep = 0.2) +
  geom_parallel_sets_axes(axis.width = 0.05, color = "#FFDA29", size = 0.1, sep = 0.2) +
  geom_text(data = labels_left, aes(x = x, id = id, split = y, label = y), stat = "parallel_sets_axes", sep = 0.2, size = 3, family = "Oswald", hjust = 1, nudge_x = -0.1, color = "#E5E9F0") +
  geom_text(data = labels_right, aes(x = x, id = id, split = y, label = y), stat = "parallel_sets_axes", sep = 0.2, size = 3, family = "Oswald", hjust = 0, nudge_x = 0.1, color = "#E5E9F0") +
  scale_fill_identity() +
  labs(x = NULL,
       y = NULL,
       title = "Voting Patterns Of Those Who Cast Their First Vote For The Yellow-eyed Penguin",
       subtitle = str_wrap("Illustrated below is a parallel set of the five votes any one voter could cast, which illustrates perference patterns in the vote. 
                           The data is limited to those that cast all five votes and to those that selected the Yellow-eyed Penguin as their first choice.
                           The heavier the line, the more votes cast with that particular combination.", 200),
       caption = "Data: **NZ Forest & Bird** | Graphic: **@jakekaupp**") +
  scale_x_discrete(labels = c("First Vote", "Second Vote", "Third Vote", "Fourth Vote", "Fifth Vote")) +
  theme_jk(grid = FALSE, dark = TRUE) +
  theme(axis.text.y = element_blank(),
        plot.caption = element_markdown())

ggsave(here("2019", "week47", "tw47_plot.png"), plot, width = 16, height = 12, dev = ragg::agg_png())
