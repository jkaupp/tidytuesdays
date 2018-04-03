library(here)
library(readxl)
library(tidyverse)
library(glue)
library(ggrepel)

tidy_data <- dir(here("data"), full.names = TRUE) %>%
  read_excel() %>%
  gather(year, avg_tuition, -State) %>%
  rename(state = State)


nat_avg <- tidy_data %>%
  filter(year %in% c("2005-06", "2015-16")) %>%
  group_by(year) %>%
  summarize(avg_tuition = mean(avg_tuition)) %>%
  mutate(state = "National Average")


plot_data <- tidy_data %>%
  filter(year %in% c("2005-06", "2015-16")) %>%
  left_join(select(nat_avg, year, nat_avg = avg_tuition), by = "year") %>%
  bind_rows(nat_avg)

labels <- plot_data %>%
  group_by(state) %>%
  filter(all(avg_tuition > nat_avg)) %>%
  pull(state) %>%
  unique()


plot <- plot_data %>%
  ggplot(., aes(x = year, y = avg_tuition, group = state)) +
  geom_path(color = "grey50", size = 0.5, alpha = 0.5) +
  geom_point(color = "grey50") +
  geom_text_repel(data = filter(plot_data, state %in% labels, year == "2015-16"), aes(label = state), direction = "y", nudge_x = 0.1, segment.size = 0.1, hjust = 0) +
  geom_path(data = nat_avg, color = "red", size = 1) +
  geom_point(data = nat_avg, color = "red") +
  geom_text_repel(data = filter(plot_data, state == "National Average", year == "2015-16"), aes(label = state), direction = "y", nudge_x = 0.1, segment.size = 0.1, hjust = 0, color = "red", segment.colour = "black", fontface = "bold") +
  scale_y_continuous(labels = scales::dollar) +
  labs(x = NULL, y = NULL, title = "Comparison of the average US tuition growth between 2005 and 2015", subtitle = "Eastern and Northeastern students consistently face tutition above the national average.", caption = "\nData: http://trends.collegeboard.org/ | Graphic: @jakekaupp") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

ggsave(plot, filename = glue('tidyweek-{Sys.Date()}.png'), height = 4, width = 4, dpi = 300)

