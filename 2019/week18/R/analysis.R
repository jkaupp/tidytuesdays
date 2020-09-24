library(tidyverse)
library(lubridate)
library(here)
library(jkmisc)
library(patchwork)

bird_collisions <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-30/bird_collisions.csv")


plot_data <- bird_collisions %>%
  filter(locality == "CHI") %>% 
  mutate(month = month(date),
         year = year(date)) %>% 
  unite("binomial_name", genus, species, sep = " ") %>% 
  count(year, month, binomial_name) %>% 
  complete(nesting(year, binomial_name), month = 1:12, fill = list(n = 0)) %>% 
  group_by(year, binomial_name) %>% 
  mutate(percent = n/sum(n)) %>% 
  mutate(percent = ifelse(is.nan(percent), 0, percent))


flower <- ggplot(plot_data, aes(x = month, y = percent, fill = binomial_name)) +
  geom_area(size = 0, position = position_dodge(), alpha = 0.2) +
  scale_x_continuous(labels = month.abb, breaks = 1:12) +
  scale_y_continuous(limits = c(0,1), breaks = c(0.5, 0.1)) +
  scale_fill_viridis_d("Year", option = "plasma", direction = 1) +
  scale_color_viridis_d(option = "plasma", direction = 1) +
  guides(fill = guide_colorbar()) +
  coord_polar() +
  labs(x = NULL,
       y = NULL,
       title = "Overall") +
  theme_jk(dark = FALSE, grid = "X", strip_text_size = 10, plot_title_size = 14) +
  theme(axis.text.y = element_blank(),
        legend.position = "none")

petals <- flower +
  aes(group = year) +
  geom_path(aes(color = binomial_name), size = 0.2, show.legend = FALSE) +
  labs(title = "By Species") +
  facet_wrap(~binomial_name, labeller = label_wrap_gen(10), nrow = 7)  +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())

legend <- plot_data %>% 
  filter(binomial_name == "Setophaga fusca") %>% 
  ggplot(aes(x = month, y = percent, fill = binomial_name, group = year)) +
  geom_area(size = 0, position = position_dodge(), alpha = 0.1) +
  geom_path(aes(color = binomial_name), size = 0.2, show.legend = FALSE) +
  annotate("text", x = 11, y = 0.8, label = "One year of\ncollisions in October", family = "Scope One", size = 3, hjust = 0) +
  annotate("segment", x = 10.8, y = 0.8, xend = 10, yend = 0.8, arrow = arrow(length = unit(0.2, "cm"))) +
  annotate("text", x = 3.5, y = 0.8, label = "Multiple years of\ncollisions in May", family = "Scope One", size = 3) +
  annotate("segment", x = 3.8, y = 0.8, xend = 5, yend = 0.8, arrow = arrow(length = unit(0.2, "cm"))) +
  scale_x_continuous(labels = month.abb, breaks = 1:12) +
  scale_y_continuous(limits = c(0,1), breaks = c(0.5, 0.1)) +
  scale_fill_viridis_d("Year", option = "plasma", direction = 1) +
  scale_color_viridis_d(option = "plasma", direction = 1) +
  labs(x = NULL,
       y = NULL,
       title = "How to Interpret This Chart",
       subtitle = str_wrap("A flower represents the recorded total collisions of each bird species with the individual petals representing the normalized events during each year (from 0-1).  The position of the petals indicates the month or months collisions occur, with overlaps indicating repeated year-over-year collisions.", 70)) +
  guides(fill = guide_colorbar()) +
  coord_polar(theta = "x", start = 0) +
  theme_jk(dark = FALSE, grid = "XY", plot_title_size = 14) +
  theme(axis.text.y = element_blank(),
        legend.position = "none")

out <- wrap_plots(flower / legend, plot_spacer(), petals, ncol = 3, widths = c(1, 0.2, 2)) +
  plot_annotation(title = "Seasonality of Bird Collisions in Chicago",
                  subtitle = str_wrap("Presented below is a petal chart of of bird collisions, with instructions on how to interpret this chart in the lower left.  The upper left flower represents collisions recorded across all years and species, with individual species presented as small multiple flowers on the right.", 220),
                  caption = "Data: Winger et al. (2019) Nocturnal flight-calling behaviour predicts vulnerability to artificial light in migratory birds. Proceedings of the Royal Society B 286(1900): 20190364. https://doi.org/10.1098/rspb.2019.0364 | Graphic: @jakekaupp",
      theme = theme_jk() + theme(plot.title.position = "plot"))

ggsave(here("2019","week18", "tw18_plot.png"), out, width = 16, height = 10, dev = ragg::agg_png())



