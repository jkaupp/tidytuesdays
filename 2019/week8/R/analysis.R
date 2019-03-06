library(tidyverse)
library(here)
library(fs)
library(readxl)
library(janitor)
library(jkmisc)
library(scales)
library(egg)

parse_table31 <- function(file) {
  
  df <- file %>% 
    read_excel(na = "na") %>% 
    clean_names()
  
  start <- min(str_which(df$x3,"\\d{4}"))
  
  end <- pull(df, 1) %>% 
    str_which("Since") %>% 
    max()
  
  df <- slice(df, start:end)
  
  field_idx <- select(df, -1) %>% 
    map_df(is.na) %>% 
    pmap_lgl(all)
  
  labels <- select(df, 1) %>%
    filter(field_idx) %>% 
    na.omit() %>% 
    pull() %>% 
    str_remove("[abcd]$")
  
  years <- slice(df, 1) %>% 
    select(-1) %>% 
    flatten_chr() %>% 
    str_remove("\\.0+$")
  
 rep <- filter(df, !field_idx) %>% 
    slice(-1) %>% 
    select(1) %>% 
    n_distinct()
  
  filter(df, !field_idx) %>% 
    slice(-1) %>% 
    mutate(discipline = rep(labels, each = rep)) %>% 
    set_names(c("category", years, "discipline")) %>% 
    gather(year, value, matches("[0-9]{4}")) %>% 
    mutate_at(vars(-category, -discipline), as.numeric) %>% 
    mutate_at("category", function(x) str_remove(x, "[abcd]$"))
  
  
}


files <- here("2019","week8","data") %>% 
  dir_ls() 

data <- map_df(files, parse_table31) %>% 
  ungroup() %>% 
  distinct()

plot_data <- data %>% 
  filter(discipline != "Other", !str_detect(category, "doctoral")) %>% 
  filter(!str_detect(discipline, "and (?!computer)")) %>% 
  filter(!str_detect(discipline, "Physical")) %>% 
  arrange(year) %>% 
  group_by(category, discipline, year) %>% 
  summarize(value = mean(value, na.rm = TRUE)) %>% 
  ungroup()

plot <- ggplot(plot_data, aes(x = year, y = value, color = discipline)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(breaks = pretty_breaks(), limits = c(0, 25)) +
  scale_color_manual("Discipline", values = tol7qualitative) +
  scale_x_continuous(limits = c(1985, 2017)) +
  expand_limits(x = c(1985, 2025)) +
  facet_wrap(~category, labeller = as_labeller(str_to_title), nrow = 1) +
  labs(x = NULL,
       y = NULL,
       title = "Median Completion Time for Doctoral Degrees Are Getting Shorter",
       subtitle = str_wrap("Median completion time in years from 1985 to 2017 contrasting selected disciplines for both University and Graduate School experience.  Education, Humanities and Social Sciences doctoral candidates have a higher than average time to completion in both categories compared to other disciplines. ", 120),
       caption = "Data: NSF | Analysis: @jakekaupp") +
  theme_jk(grid = "XY") +
  theme(legend.position = "bottom")

ggsave(here('2019','week8',"tw8_plot.png"), plot, width = 10, height = 7)


bonus_plot_data <- plot_data %>% 
  filter(discipline == "All fields") %>% 
  rename(overall = value) %>% 
  select(-discipline) %>% 
  left_join(filter(plot_data, discipline != "All fields")) %>% 
  mutate(diff = abs(value - overall)/2) %>%
  filter(between(year, 1990, 2011))


order <- c("Education", "Mathematics and computer sciences",  "Engineering", "Humanities","Life sciences", 
   "Social sciences")

bonus_plot <- ggplot(bonus_plot_data, aes(ymin = -diff, ymax = diff, x = year, fill = fct_relevel(discipline, order))) +
  geom_ribbon(color = "white", size = 0.2, alpha = 0.8) +
  geom_segment(data = filter(bonus_plot_data, year == 2000, category == "Since bachelor's", discipline == "Education"), aes(y = -diff, yend = diff, x = year, xend = year), color = "grey20", arrow = arrow(length = unit(0.25, "cm"), ends = "both", type = "closed")) +
  facet_wrap(~category, labeller = as_labeller(str_to_title)) +
  scale_fill_viridis_d("Discipline") +
  scale_y_continuous(breaks = c(-5, -2.5, -1, 0, 1, 2.5, 5), labels = c("10 Years", "5 Years", "2 Years", "Group Median", "2 Years", "5 Years", "10 Years")) +
  labs(x = NULL,
       y = NULL,
       title = "Relative Differences in Median Doctoral Completion Time from the Group Median by Discipline and Interval",
       subtitle = str_wrap("The streamgraph below presents the difference between discipline median completion time and the group median completion time (All Fields) as the width of each colored band (discipline) from 1990 to 2011.", 120),
       caption = "Data: NSF | Analysis: @jakekaupp") +
  theme_jk(grid = "XY") +
  theme(legend.position = "bottom")

bonus_plot <- tag_facet(bonus_plot, x = 1996, y = 5.5, open = "", close = "", tag_pool = c("Difference from Group Median = 9.1 years", ""),  fontface = 1, family = "Oswald") 



ggsave(here('2019','week8',"tw8_bonus_plot.png"), bonus_plot, width = 12, height = 6)
