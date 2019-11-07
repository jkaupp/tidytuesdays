library(tidyverse)
library(jkmisc)
library(glue)

commute_mode <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-05/commute.csv")

total_avg <- commute_mode %>% 
  group_by(city, mode) %>% 
  filter(n() > 1) %>% 
  group_by(mode) %>% 
  summarize(avg = mean(percent/100)) %>% 
  mutate(state = "US",
         state_abb = "US")

slope_data <- commute_mode %>% 
  group_by(city, mode) %>% 
  filter(n() > 1) %>% 
  group_by(state, state_abb, mode) %>% 
  summarize(avg = mean(percent/100)) %>% 
  ungroup() %>% 
  mutate(state_abb = ifelse(is.na(state_abb), "DC", state_abb))

direct_labels <- slope_data %>% 
  filter(mode == "Walk") %>% 
  top_n(8, avg) %>% 
  arrange(-avg) %>% 
  slice(-3:-6)

direct_labels_bike <- slope_data %>% 
  filter(mode == "Bike") %>% 
  top_n(5, avg)

mid_labels <- slope_data %>% 
  filter(mode == "Walk") %>% 
  top_n(8, avg) %>% 
  arrange(-avg) %>% 
  slice(3:6) %>% 
  group_by(mode) %>% 
  summarize(state_abb = toString(state_abb),
    avg = mean(.$avg),
    y = min(.$avg),
    yend = max(.$avg))
  
lower_labels <- slope_data %>% 
  filter(mode == "Walk") %>% 
  arrange(-avg) %>% 
  slice(9:nrow(.)) %>% 
  group_by(mode) %>% 
  summarize(state_abb = toString(state_abb),
            avg = mean(avg),
            y = min(.$avg),
            yend = max(.$avg))

lower_bike_labels <- slope_data %>% 
  filter(mode == "Bike") %>% 
  arrange(-avg) %>% 
  slice(6:nrow(.)) %>% 
  group_by(mode) %>% 
  summarize(state_abb = toString(state_abb),
            avg = mean(avg),
            y = min(.$avg),
            yend = max(.$avg))


ggplot(slope_data, aes(x = mode, y = avg)) +
  geom_line(aes(group = state), size = 0.2) +
  geom_line(data = total_avg, aes(group = state), color = "#DD2A7B", size = 1) +
  geom_point(shape = 21, color = "white", stroke = 0.2, fill = "black", size = 2) +
  geom_text(data = direct_labels, aes(label = state_abb),  nudge_x = 0.05, family = "Lora", size = 3, hjust = 0, color = "grey50") +
  geom_text(data = direct_labels_bike, aes(label = state_abb),  nudge_x = -0.05, family = "Lora", size = 3, hjust = 0, color = "grey50") +
  geom_text(data = mid_labels, aes(label = state_abb),  nudge_x = 0.1, family = "Lora", size = 3, hjust = 0, color = "grey50") +
  geom_text(data = lower_labels, aes(label = str_wrap(state_abb, 30)), nudge_x = 0.1, family = "Lora", size = 3, hjust = 0, color = "grey50") +
  geom_text(data = lower_bike_labels, aes(label = str_wrap(state_abb, 30)), nudge_x = -0.5, family = "Lora", size = 3, hjust = 0, color = "grey50") +
  geom_segment(data = lower_labels, aes(x = 2.06, xend = 2.06, y = y, yend = yend), size = 0.2, color = "grey50") +
  geom_segment(data = lower_labels, aes(x = 2.01, xend = 2.06, y = y, yend = y), size = 0.2, color = "grey50") +
  geom_segment(data = lower_labels, aes(x = 2.01, xend = 2.06, y = yend, yend = yend), size = 0.2, color = "grey50") +
  geom_segment(data = lower_labels, aes(x = 2.06, xend = 2.09, y = avg, yend = avg), size = 0.2, color = "grey50") +
  geom_segment(data = mid_labels, aes(x = 2.06, xend = 2.06, y = y, yend = yend), size = 0.2, color = "grey50") +
  geom_segment(data = mid_labels, aes(x = 2.01, xend = 2.06, y = y, yend = y), size = 0.2, color = "grey50") +
  geom_segment(data = mid_labels, aes(x = 2.01, xend = 2.06, y = yend, yend = yend), size = 0.2, color = "grey50") +
  geom_segment(data = mid_labels, aes(x = 2.06, xend = 2.09, y = avg, yend = avg), size = 0.2, color = "grey50") +
  geom_segment(data = lower_bike_labels, aes(x = 0.95, xend = 0.95, y = y, yend = yend), size = 0.2, color = "grey50") +
  geom_segment(data = lower_bike_labels, aes(x = 0.98, xend = 0.95, y = y, yend = y), size = 0.2, color = "grey50") +
  geom_segment(data = lower_bike_labels, aes(x = 0.98, xend = 0.95, y = yend, yend = yend), size = 0.2, color = "grey50") +
  geom_segment(data = lower_bike_labels, aes(x = 0.95, xend = 0.91, y = avg, yend = avg), size = 0.2, color = "grey50") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL,
       y = NULL,
       title = "Bicycling and Walking to Work in the United States: 2008-2012",
       subtitle = glue("Illustrated below is a slopegraph contrasting the percentage of population that bikes to work and the percentage<br>that bikes to work as well as {highlight_text('the US average', '#DD2A7B', 'b')}")) +
  scale_x_discrete(labels = c("Bike to Work", "Walk to Work")) +
  theme_jk(grid = "XY",
           markdown = TRUE) +
  theme(panel.grid.major = element_line(linetype = "dashed"))
  
