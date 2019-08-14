library(tidyverse)
library(ggforce)
library(here)
library(jkmisc)
library(patchwork)

bob_ross_paintings <- here("2019", "week32", "data", "tidytuesday_201932_bob_ross_paintings.csv")

data <- read_csv(bob_ross_paintings, col_names = c('episode', 'title', 'color', 'color_name')) %>% 
  mutate(season = parse_number(str_extract(episode, "S\\d+")),
         color = if_else(color == "#FFFFFF", "grey80", color))


plot_data <- data %>% 
  count(season, title, color_name, color) %>% 
  group_by(season, title) %>% 
  mutate(percent = n/sum(n)) %>% 
  ungroup() %>% 
  mutate(color_number = as.numeric(factor(color_name))) %>% 
  mutate(angle = (color_number-1)*(360/15),
         angle = ifelse(angle < 0, 360 + angle, angle),
         radians = angle*pi/180,
         x0 = percent * cos(radians),
         y0 = percent * sin(radians))
  

plot_spiros <- function(data) {
  
season <- sprintf("Season %s",unique(data$season))
  
ggplot(data) +
  geom_spiro(aes(R = ifelse(percent == 1, 0.1, 1 - percent), r = percent, d = radians, color = color, group = color), size = 0.1) +
  scale_color_identity() +
  theme_jk(grid = FALSE, plot_title_size = 8, strip_text_size = 8) +
  facet_wrap(~ title, ncol = 1, labeller = label_wrap_gen(15)) +
  labs(x = NULL, y = NULL, title = season) +
  theme(axis.text = element_blank(),
        legend.position = "none",
        panel.spacing = unit(0.1, "lines")) +
  coord_equal()  }


plots <- plot_data %>% 
  split(.$season) %>% 
  map(plot_spiros)


all_seasons <- wrap_plots(plots, nrow = 1) + plot_annotation(title = "Happy Accidents with 1960s Toys: Sprirographs of Palette Colors of Bob Ross Paintings for 31 Seaons",
                                                             subtitle = "Illustrated below is a spirograph tracing of the 15 distinct un-mixed palette colours used in each of Bob Ross' paintings.  The more colours used in a painting, the larger the spirograph and it appears similar to china pattern while those paintings with a more minimalist palette show up as smaller sparse rings.",
                                                             caption = "Data: c/o @geokaramanis | Graphic: @jakekaupp",
                                                             theme = theme_jk())


ggsave(filename = here("2019", "week32", "tw32_plot.png"), plot = all_seasons, width = 30, height = 15, type = "cairo")

twitter <- map(plots, ~.x + theme(strip.text = element_blank()))


all_seasons_twitter <- wrap_plots(twitter, nrow = 1) + plot_annotation(title = "Happy Accidents with 1960s Toys: Sprirographs of Palette Colors of Bob Ross Paintings for 31 Seaons",
                                                             subtitle = str_wrap("Illustrated below is a spirograph tracing of the 15 distinct un-mixed palette colours used in each of Bob Ross' paintings.  The more colours used in a painting, the larger the spirograph and it appears similar to china pattern while those paintings with a more minimalist palette show up as smaller sparse rings.", 265),
                                                             caption = "Data: c/o @geokaramanis | Graphic: @jakekaupp",
                                                             theme = theme_jk())


ggsave(filename = here("2019", "week32", "tw32_plot_for_twitter.png"), plot = all_seasons_twitter, width = 20, height = 7)


