library(tidyverse)
library(here)
library(jkmisc)
library(futurevisions)
library(colorspace)
library(glue)

plants <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/plants.csv')

flowering_plants <- filter(plants, group == "Flowering Plant", !is.na(year_last_seen))

# Two ideas, dendrogram by continent, or timeline


order <- count(flowering_plants, continent) %>% 
  arrange(n) %>% 
  mutate(continent = factor(continent, continent)) %>% 
  rowwise() %>% 
  mutate(n = ifelse(continent == "Europe", 12,  n))

by_continent <- flowering_plants %>% 
  select(binomial_name:year_last_seen) %>% 
  mutate(year_last_seen = str_replace(year_last_seen, "Before (\\d+)", "1900-\\1")) %>% 
  separate(year_last_seen, c("start", "end"), sep = "-", convert = TRUE) %>% 
  count(continent, start) %>% 
  complete(continent, start = seq(1900, 2020, 10), fill = list(n = 0)) %>% 
  group_by(continent) %>% 
  mutate(count = cumsum(n))  %>% 
  mutate(continent = factor(continent, levels = pull(order, continent)))



plot <- ggplot(by_continent, aes(x = start, y = count, color = continent, fill = continent)) +
  geom_area(show.legend = FALSE) +
  geom_text(data = order, aes(x = 2025, y = n, label = continent), position = position_stack(), hjust = 0, vjust = 1, show.legend = FALSE, family = "Bebas Neue", size = 5) +
  scale_color_manual(values = darken(rev(futurevisions("cancri")))) +
  scale_fill_manual(values = rev(futurevisions("cancri"))) +
  scale_x_continuous(breaks = seq(1900, 2020, 10), expand = c(0.01,0)) +
  scale_y_continuous(expand = c(0.01,0)) +
  expand_limits(x = c(1900, 2040)) +
  labs(title = "Flowering Plant Species Lost Globally From 1900-2020",
       subtitle = glue("Since 1900, flowering plants have been facing extinctions due to human activities. In total, 500 plant species are considered extinct as of 2020.<br>45% of those were endemic to {highlight_text('Africa', first(futurevisions('cancri')), 'b')} followed by {highlight_text('North', nth(futurevisions('cancri'), 2), 'b')} and {highlight_text('South America', nth(futurevisions('cancri'), 3), 'b')} with 17.8% and 15.4%, respectively."),
       caption = "**Data**: IUCN Red List | **Graphic**: @jakekaupp",
       x = NULL,
       y = NULL) +
  theme_jk(grid = "XY",
           markdown = TRUE,
           base_family = "Poppins",
           plot_title_family = "Bebas Neue",
           subtitle_family = "Poppins",
           caption_family = "Poppins") +
  theme(plot.title.position = "plot",
        plot.caption.position = "panel")

ggsave(here("2020", "week34", "tw34_plot.png"), plot, width = 12, height = 8)
