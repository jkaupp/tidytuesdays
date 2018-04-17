library(tidyverse)
library(here)
library(readxl)
library(ggridges)
library(jkmisc)
library(glue)


# Formatter for 538 year labels 
labels_538 <- function(labels) {
  # labels_out <- sprintf("20%s", str_sub(labels, 3, 4))
  labels_out <- c(labels[1], glue("'{str_sub(labels[-1], 3, 4)}"))
  return(labels_out)
}

mortality_data <- dir(here("week3","data"), pattern = "global", full.names = TRUE) %>% 
  read_excel()

tidy_mort <- mortality_data %>% 
  gather(cause_of_death, percentage, -country:-year) %>% 
  mutate(cause_of_death = trimws(gsub("\\(\\%\\)", "", cause_of_death))) %>% 
  mutate(percentage = ifelse(is.na(percentage), NA, percentage/100))

suicide_data <- tidy_mort %>% 
  filter(cause_of_death == "Suicide")

global_rate <- suicide_data %>% 
  group_by(year) %>% 
  summarize(percentage = mean(percentage, na.rm = TRUE))

problem_countries <- suicide_data %>% 
  group_by(country) %>% 
  summarize(percentage = mean(percentage, na.rm = TRUE)) %>% 
  top_n(10, percentage) %>% 
  arrange(desc(percentage)) %>% 
  pull(country)

plot_data <- suicide_data %>% 
  filter(country %in% problem_countries) %>% 
  mutate(country = factor(country, problem_countries))

ggplot(plot_data, aes(x = year, y = percentage)) +
  geom_area(fill = "steelblue4") +
  geom_path(color = "white", size = 0.2) +
  geom_area(data = global_rate, fill = "steelblue3") +
  geom_path(data = global_rate, color = "white", size = 0.2) +
  facet_wrap(~country, nrow = 2) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Countries Coping With the Tradgedy and Pain of Suicide",
       subtitle = "Dark blue indicates suicide rate by year, Light blue fill indicates the global average suicide rate by year.",
       x = NULL,
       y = NULL,
       caption = "Data: ourworldindata.org | Graphic: @jakekaupp") +
  theme_jk(grid = "XY")
