library(tidyverse)
library(here)
library(ggpattern)
library(ggforce)
library(colorspace)
library(magick)
library(ggtext)
library(here)

tweets <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-15/tweets.csv')

colors <- c("#E1D0BE", "#FBBA00", "#D41735", "#22194E")
text_colors <- c("#D41735", "#22194E", "#FBBA00", "#E1D0BE")

top_4 <- tweets %>% 
  filter(username != "AlDatavizguy",
         str_detect(tolower(content), "tidytuesday")) %>% 
  slice_max(like_count, n = 4) %>% 
  arrange(like_count) %>% 
  mutate(x0 = 0,
         y0 = c(975, 800, 550, 125),
         fill = colors,
         text_color = text_colors) 

plot <- ggplot(top_4, aes(x0 = x0, y0 = y0, r = like_count/2)) +
  geom_circle(aes(fill = fill), size = 0.1) +
  geom_text(aes(x = x0, y = y0, color = text_color, label = like_count), family = "Rajdhani Medium", fontface = "bold", size = 5) +
  geom_text(aes(x = x0, y = y0 - like_count/2, label = paste0("@", username)), family = "Rajdhani",  size = 5, vjust = 1.2) +
  scale_x_continuous(limits = c(-600, 600)) +
  scale_fill_identity() +
  scale_color_identity() +
  coord_equal(clip = "off") +
  labs(title = "TOP FOUR #JUNETEENTH TIDYTUESDAY TWEETS FROM THE #DUBOISCHALLENGE",
       caption = "**Data**: #TidyTuesday Twitter | **Graphic**: @jakekaupp") +
  theme_void() +
  theme(plot.background = element_rect(fill = lighten("#EDE0D0", 0.7), color = NA),
        plot.title = element_text(family = "Rajdhani Medium", size = 20, hjust = 0.5),
        plot.caption = element_markdown(family = "Rajdhani Medium"))

ggsave(here("2021", "week25", "tw25_plot.png"), plot, width = 9, height = 9, device = ragg::agg_png())


