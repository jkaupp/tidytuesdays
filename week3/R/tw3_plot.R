library(tidyverse)
library(here)
library(readxl)
library(jkmisc)

# Read in the data
mortality_data <- dir(here("week3","data"), pattern = "global", full.names = TRUE) %>% 
  read_excel()


# Tidy up the data
tidy_mort <- mortality_data %>% 
  gather(cause_of_death, percentage, -country:-year) %>% 
  mutate(cause_of_death = trimws(str_remove(cause_of_death, "\\(\\%\\)"))) %>% 
  mutate(percentage = ifelse(is.na(percentage), NA, percentage/100))

# Get just the data pertaining to suicides
suicide_data <- tidy_mort %>% 
  filter(cause_of_death == "Suicide", !is.na(country_code))

# Get the World percentage
global_rate <- suicide_data %>% 
  filter(country == "World") %>% 
  select(year, percentage)

# Get the top 40 problem countries, those with the suicide rate constantly over the world average (note the all statement in the filter)
problem_countries <- suicide_data %>% 
  filter(country != "World") %>% 
  left_join(global_rate, by = "year") %>% 
  group_by(country) %>% 
  filter(all(percentage.x > percentage.y)) %>% 
  summarize(percentage = mean(percentage.x, na.rm = TRUE)) %>% 
  top_n(40, percentage) %>% 
  arrange(desc(percentage)) %>% 
  pull(country)

# Create the data to make the plot, and arrange descending by the overall avg rate of suicide
plot_data <- suicide_data %>% 
  filter(country %in% problem_countries) %>% 
  mutate(country = factor(country, problem_countries))

# Create the sad plot
sad_plot <- ggplot(plot_data, aes(x = year, y = percentage)) +
  geom_segment(aes(x = min(year), xend = max(year), y = 0, yend = 0)) +
  geom_area(fill = "steelblue4") +
  geom_path(color = "grey30", size = 0.2) +
  geom_area(data = global_rate, fill = "steelblue3") +
  geom_path(data = global_rate, color = "grey30", size = 0.2) +
  facet_wrap(~country, nrow = 5) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Countries Coping With the Tradgedy and Pain of Suicide",
       subtitle = "Dark blue indicates suicide rate by year, Light blue fill indicates the global average suicide rate by year.",
       x = NULL,
       y = NULL,
       caption = "Data: ourworldindata.org | Graphic: @jakekaupp") +
  theme_jk(grid = "Y")

# Save the sad plot
ggsave(here("week3","tw3_sad_plot.png"), sad_plot, width = 16, height = 10)
