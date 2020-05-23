library(tidyverse)
library(ggbeeswarm)
library(jkmisc)
library(glue)
library(here)

vb_matches <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-19/vb_matches.csv', guess_max = 76000)

player_summary <- vb_matches %>% 
  select(year, contains("player"), contains("tot"), contains("age"), gender) %>% 
  pivot_longer(contains("player"), names_to = "type", values_to = "name") %>% 
  pivot_longer(contains("tot"), names_to = "metric", values_to = "total") %>% 
  pivot_longer(contains("age"), names_to = "type2", values_to = "age") %>% 
  mutate(metric = str_remove(metric, "[wl]_p[1-2]_tot_")) %>% 
  mutate(age = floor(age)) %>% 
  group_by(name, gender, age, metric) %>% 
  summarize(total = mean(total, na.rm = TRUE)) %>% 
  filter(metric == "hitpct", between(total, 0, 0.99)) 

age_summary <- player_summary %>% 
  group_by(gender, age, metric) %>% 
  summarize(total = mean(total, na.rm = TRUE)) %>%  
  filter(metric == "hitpct", between(total, 0, 0.99)) 



plot <- ggplot(player_summary, aes(x = age, y = total, fill = gender, color = gender)) +
  geom_quasirandom(groupOnX = TRUE, dodge.width = 0.8, alpha = 0.1, method = "tukey", shape = 21, color = "grey30", show.legend = FALSE) +
  geom_line(data = age_summary, aes(x = age, y = total, color = gender), position = position_quasirandom(dodge.width = 0.8), size = 1, show.legend = FALSE) +
  geom_point(data = age_summary, aes(x = age, y = total, color = gender), position = position_quasirandom(dodge.width = 0.8), size = 2, show.legend = FALSE) +
  labs(x = NULL,
       y = NULL,
       title = "Beach Volleyball Hit/Attack Percentage Remains Consistent with Age",
       subtitle = glue("Shown below is the average hit percentage per player by age for {highlight_text('Men',  tricolour_pal[2], 'b')} and {highlight_text('Women', tricolour_pal[3], 'b')}. Hit percentage is treated similar to baseballs batting average, with 0.5 or 50% reading as 500.<br>With beach volleyball being a faster and more offensive game, hit percentages are higher (400 average) and remain consistent with respect to player age."),
       caption = "**Data**: Adam Vagnar | **Graphic**: @jakekaupp") +
  scale_y_continuous(limits = c(0, 1), labels = function(x) x * 1000, breaks = seq(0, 1, 0.2)) +
  scale_fill_grey() +
  scale_color_manual(values = tricolour_pal[-1]) +
  theme_jk(markdown = TRUE) +
  theme(plot.caption.position = "plot",
        plot.title.position = "plot",
        plot.background = element_rect(fill = "#F0F6F6"))

ggsave()

