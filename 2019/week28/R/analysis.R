library(tidyverse)
library(here)
library(jkmisc)
library(ggforce)
library(glue)


squads <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/squads.csv")


data <- squads %>% 
  mutate(idx = goals/caps) %>% 
  filter(pos != "GK", caps > 0, goals > 0) %>% 
  mutate(pos = case_when(pos == "DF" ~ "Defense",
                         pos == "FW" ~ "Forward",
                         pos == "MF" ~ "Mid-Field")) %>% 
  mutate(desc = glue("{club}\n{caps} Matches, {goals} Goals"))

means <- data %>% 
  group_by(pos) %>% 
  summarize(idx = mean(idx))

plot <- ggplot(data, aes(x = age, y = idx)) +
  geom_point(color = "grey20") +
  geom_mark_circle(aes(label = player, description = desc, filter = player == "Khadija Shaw")) +
  geom_mark_circle(aes(label = player, description = desc, filter = player  == "Lea Schüller")) +
  geom_mark_circle(aes(label = player, description = desc, filter = player  == "Ainon Phancha")) +
  geom_hline(data = means, aes(yintercept = idx), color = "firebrick") +
  theme_jk() +
  facet_wrap(~pos, nrow = 1) +
  labs(y = NULL,
       x = "Age",
       title = "Efficient Scorers Competing in the Womens World Cup by Position and Age",
       subtitle = str_wrap("Goals per games played in international play by player age.  The highly efficient players at each position are a mix of newcomers and seasoned veterans, illustrating consistency in some players through their career.", 150),
       caption = "Data: data.world | Graphic : @jakekaupp")

ggsave("")

