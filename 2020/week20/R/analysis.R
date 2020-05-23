library(tidyverse)
library(jkmisc)
library(patchwork)
library(colorspace)
library(glue)
library(here)

set.seed(42)

tree_rings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/tree_rings.csv') %>% 
  filter(!is.na(year))

century_rings <- tree_rings %>% 
  rbind(fake_row, .) %>% 
  filter(year != 2000) %>% 
  arrange(year) %>% 
  mutate(century = rep(1:20, each = 100)) %>%
  group_by(century) %>% 
  mutate(idx = row_number()) %>% 
  ungroup() %>% 
  filter(year != 0) 

greys <- grey.colors(20) %>%
  set_names(., 1:20)

plot_data <- map_dfr(1:20, ~mutate(century_rings, fill = if_else(century == .x, "#56B1F7", greys[century]),
                                   facet = .x))

century_labels <- glue("{seq(0, 1900, 100)}-{seq(99, 1999, 100)} CE") %>% 
  as.character() %>% 
  set_names(1:20)

century_labels[1] <- "1-99 CE"


main <- ggplot(filter(plot_data, facet == 1), aes(x = idx, y = n_tree)) +
  geom_area(data = filter(plot_data, fill != "#56B1F7", facet == 1), aes(group = century, fill = fill), position = position_dodge(), alpha = 0.3) +
  geom_area(data = filter(plot_data, fill == "#56B1F7", facet == 1), aes(group = century, fill = map_chr(fill, ~darken(.x, 0.4))), position = position_dodge(), alpha = 1) +
  geom_hline(yintercept = 0, size = 0.5, color = darken("#56B1F7", 0.4)) +
  annotate("text", x = 35, y = 4, label = "Above Average\nTree-Ring Deformity", family = "Oswald",  color = darken("#56B1F7", 0.4)) +
  annotate("segment", x = 35, xend = 35, y = 3, yend = 2.3, arrow = arrow(type = "closed", length = unit(1.5, "mm")),  color = darken("#56B1F7", 0.4)) +
  annotate("text", x = 85, y = 2.5, label = "Below Average\nTree-Ring Deformity", family = "Oswald", color = darken("#56B1F7", 0.4)) +
  annotate("segment", x = 85, xend = 85, y = 1.5, yend = 0.1, arrow = arrow(type = "closed", length = unit(1.5, "mm")),  color = darken("#56B1F7", 0.4)) +
  annotate("text", x = 17, y = 3, label = "Average Tree-Ring Deformity\n(relative to 1000–1099 ce)", family = "Oswald", color = darken("#56B1F7", 0.4)) +
  annotate("segment", x = 15, xend = 15, y = 0.8, yend = 0, color = darken("#56B1F7", 0.4), arrow = arrow(type = "closed", length = unit(1.5, "mm"))) +
  annotate("text", x = 0, y = 4, label = "L E G E N D", family = "Oswald", size = 8, color = darken("#56B1F7", 0.4)) +
  annotate("segment", x = 0.5, xend = 0.5, y = -1, yend = 1, color = darken("#56B1F7", 0.4)) +
  annotate("text", x = 0, y = 1.5, color = darken("#56B1F7", 0.4), label = "Century Start", family = "Oswald", hjust = 0) +
  annotate("segment", x = 1, xend = 5, y = 1, yend = 1, color = darken("#56B1F7", 0.4), arrow = arrow(type = "closed", length = unit(1.5, "mm"))) +
  labs(x = NULL, 
       y = NULL) +
  scale_fill_identity() +
  coord_polar(clip = "off") +
  theme_jk(grid = FALSE) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())
  

facets <- ggplot(plot_data, aes(x = idx, y = n_tree)) +
  geom_area(data = filter(plot_data, fill != "#56B1F7"), aes(group = century, fill = fill), position = position_dodge(), alpha = 0.3) +
  geom_area(data = filter(plot_data, fill == "#56B1F7"), aes(group = century, fill = map_chr(fill, ~darken(.x, 0.4))), position = position_dodge(), alpha = 1) +
  geom_hline(yintercept = 0, size = 0.5, color = darken("#56B1F7", 0.4)) +
  labs(x = NULL, y = NULL) +
  facet_wrap(~facet, nrow = 4, labeller = as_labeller(century_labels), strip.position = "bottom") +
  coord_polar(clip = "off") +
  scale_fill_identity() +
  theme_jk(grid = FALSE) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        strip.text.x = element_text(hjust = 0.5,  color = darken("#56B1F7", 0.4)))

plot <- main + facets + plot_annotation(title = "The Impact of Volcanic Eruptions on Measurements of Tree Ring Deformity by Century",
                                subtitle = glue("Illustrated below is a circular area chart of the z-score of tree ring deformity relative to 1000-1099 CE, broken out into facets by century.  Each individual<br>century forms its own ring and in each facet with the {highlight_text('century of focus', darken('#56B1F7', 0.4), 'b')} is highlighted and plotted against other centuries."),
                                caption = "**Data**: doi.org/10.1038/nature14565 | **Graphic**: @jakekaupp",
                                theme = theme_jk(markdown = TRUE) +
                                  theme(plot.title.position = "plot"))

ggsave(here('2020', "week20", "tw20_plot.png"), plot, width = 12.5, height = 8)
  
