library(tidyverse)
library(jkmisc)
library(glue)
library(here)

movies <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/movies.csv')

test_counts <- movies %>% 
  separate_rows(writer, sep = ",") %>% 
  mutate(across(writer, str_trim)) %>% 
  filter(!str_detect(writer, "novel"),
         !str_detect(writer, "characters")) %>% 
  mutate(writer = str_remove_all(writer, "\\s*\\(.*?\\)\\s*")) %>% 
  filter(!is.na(writer), writer != "N/A") %>% 
  count(writer, binary, sort = TRUE)

overall_tests <- test_counts %>% 
  group_by(writer) %>% 
  summarize(total = sum(n))

writer_percentages <- test_counts %>% 
  left_join(overall_tests) %>% 
  mutate(percentage = n/total) %>% 
  filter(total > 4) %>% 
  ungroup() %>% 
  complete(nesting(writer, total), binary, fill = list(n = 0, percentage = 0)) 
  

order <- writer_percentages %>% 
  filter(binary == "PASS") %>% 
  #pivot_wider(names_from = binary, values_from = percentage, values_fill = 0) %>% 
  arrange(percentage) %>% 
  pull(writer) 

plot_data <- writer_percentages %>% 
  mutate(writer = factor(writer, order)) %>% 
  filter(!(binary == "PASS" & n == 0))

labels <- writer_percentages %>% 
  mutate(writer = factor(writer, order)) %>% 
  group_by(writer) %>% 
  slice_max(percentage, with_ties = FALSE) %>% 
  mutate(label_x = if_else(binary == 'FAIL', 0.995, 0.005))

plot <- ggplot(plot_data, aes(x = percentage, y = writer, fill = binary, color = binary)) +
  geom_col(show.legend = FALSE) +
  geom_text(data = labels, aes(x = label_x, y = writer, label = writer), hjust = "inward", color = "white", family = "Atkinson Hyperlegible", size = 3) +
  geom_vline(xintercept = 0.5, linetype = "dashed") +
  scale_fill_manual(values = rev(c("#009C99", "#3A023B"))) +
  scale_color_manual(values = rev(c("#009C99", "#3A023B"))) +
  scale_x_continuous(labels = scales::percent, expand = c(0,0.01)) +
  labs(x = NULL,
       y = NULL,
       title = "Gender Bias in Script and Screenplay Writers",
       subtitle = glue("Illustrated below is a stacked bar chart of the percentage of works that {highlight_text('pass', '#009C99', 'b')} or {highlight_text('fail', '#3A023B', 'b')}<br>the Bechdel Test for writers with five or greater writing credits in evaluated films."),
       caption = "**Data**: FiveThirtyEight | **Graphic**: @jakekaupp") +
  theme_jk(grid = FALSE,
           markdown = TRUE,subtitle_family = "Atkinson Hyperlegible") +
  theme(axis.text.y = element_blank())

ggsave(here("2021", "week11", "tw11_plot.png"), plot, width = 8, height = 16, device = ragg::agg_png())
