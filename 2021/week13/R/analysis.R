library(tidyverse)
library(here)
library(jkmisc)
library(rtweet)
library(rvest)
library(janitor)
library(parsedate)
library(lubridate)
library(patchwork)
library(glue)
library(ggtext)
library(altText)

unvotes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/unvotes.csv')
roll_calls <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/roll_calls.csv')
issues <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/issues.csv')


full_un_votes <- reduce(list(unvotes, roll_calls, issues), left_join)

nato_table <- read_html("https://en.wikipedia.org/wiki/Member_states_of_NATO") %>% 
  html_node("table.wikitable:nth-child(14)") %>% 
  html_table() %>% 
  clean_names() %>% 
  mutate(year = str_extract(accession_5, "[0-9]{4}"),
         member_state = str_remove(member_state, "\\[c\\]"),
         member_state = str_replace(member_state, "Czech Republic", "Czechia"))

nato_countries <- nato_table %>% 
  arrange(year) %>% 
  pull(member_state) 

nato_votes <- full_un_votes %>% 
  mutate(country = str_replace(country, "Czech Republic", "Czechia")) %>% 
  filter(country %in% nato_countries) %>% 
  arrange(date, rcid) %>% 
  group_by(rcid) %>% 
  add_column(rc_id = group_indices(.)) %>% 
  mutate(country = factor(country, nato_countries),
         vote_num = as.numeric(factor(vote, c("yes", "abstain", "no"))))

breaks <- nato_votes %>% 
  mutate(year = year(date)) %>% 
  group_by(rc_id) %>% 
  slice(1) %>% 
  group_by(rc_id) %>% 
  summarize(year = max(year)) %>%
  distinct(year, .keep_all = TRUE) %>% 
  filter(year %in% seq(1940, 2020, 10))

#Look at historical alignment between Nato countries
stripes <- nato_votes %>% 
  filter(country_code == "US") %>%
  ggplot(aes(x = rc_id, xend = rc_id, y = 0, yend = 1)) +
  geom_segment(aes(color = vote), show.legend = FALSE) +
  geom_text(data = breaks, aes(label = year), family = "Anton", color = "#F7F0F5", size = 15, hjust = 0, vjust = 0) +
  #annotate("text", x = 2, y = 0, label = "U N I T E D  S T A T E S", family = "Anton", color = "#F7F0F5", size = 20, hjust = 0, vjust = 0) +
  theme_jk(grid = FALSE) +
  labs(x = NULL, y = NULL) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_discrete(expand = c(0.001, 0)) +
  scale_color_manual(values = set_names(c("#37323E", "#B42D3F", "#3891A6"), c("abstain", "no", "yes"))) +
  theme(plot.background = element_rect(fill = "#F7F0F5", color = NA),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank())

bar_data <- nato_votes %>% 
  ungroup() %>% 
  filter(country_code == "US") %>% 
  arrange(vote_num) %>% 
  mutate(rc_id = row_number()) 

bar_labels <- bar_data %>% 
  add_count(vote_num) %>% 
  group_by(vote_num) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate(percentage = n/sum(n))

porportion <- ggplot(bar_data, aes(x = rc_id, xend = rc_id, y = 0, yend = 1)) +
  geom_segment(aes(color = vote), show.legend = FALSE) +
  geom_text(data = bar_labels, aes(label = scales::percent(percentage)), family = "Anton", color = "#F7F0F5", size = 15, hjust = 0, vjust = 0) +
  theme_jk(grid = FALSE) +
  labs(x = NULL, y = NULL) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_discrete(expand = c(0.001, 0)) +
  scale_color_manual(values = set_names(c("#37323E", "#B42D3F", "#3891A6"), c("abstain", "no", "yes"))) +
  theme(plot.background = element_rect(fill = "#F7F0F5", color = NA),
        axis.text.y = element_blank(),
        axis.text.x = element_blank())

plot <- stripes/porportion + plot_annotation(title = glue("THE UNITED STATES OF NO"),
                                             subtitle = glue("Illustrated below are two views of the United States voting patterns on UN resolutions from 1946 to 2019. The top chart represents each vote as a vertical stripe for {highlight_text('yes','#3891A6', 'b', 25)}, {highlight_text('no','#B42D3F', 'b', 25)} or {highlight_text('abstentions','#37323E', 'b', 25)}<br>while the bottom chart depicts the percentages between three ballot choices.  Startlingly enough, the United States votes against the resolutions 54.7% of the time."),
                                             caption = "**Data**: Harvard Dataverse | **Graphic**: @jakekaupp",
                                             theme = theme_jk(plot_title_family = "Anton",
                                                              plot_title_size = 50, 
                                                              subtitle_family = "Work Sans",
                                                              subtitle_size = 15,
                                                              markdown = TRUE) + 
                                               theme(plot.background = element_rect(fill = "#F7F0F5", color = NA),
                                                     plot.title = element_markdown(color = "#37323E"),
                                                     plot.subtitle = element_markdown(color = "#37323E")
                                               ))

ggsave(here("2021", "week13", "tw13_plot.png"), plot, width = 20, height = 10, device = ragg::agg_png())

alt_text <- "Show are two charts in a single graphic illustrating United States voting patterns on UN resolutions from 1946 to 2019. The top chart represents each vote as a vertical stripes with red for yes votes, blue for no vote and grey for abstentions. The  the bottom chart depicts the percentages between three ballot choices with the same color categories as the first graphic, showing the United states voting no 54.7% of the time; voting yes 26.2% of the time and abstaining 19.2% of the time."


alt_text(plot)
