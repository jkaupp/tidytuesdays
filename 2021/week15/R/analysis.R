library(tidyverse)
library(jkmisc)
library(here)
library(scales)
library(packcircles)
library(imager)
library(ggtext)


trees <- tibble(year = 2015:2217) %>% 
  mutate(trees = 3040-15*(year-2015))

im <- load.image(here("2021", "week15", "data", "tree.png")) 

im_df <- im %>%
  as.data.frame(wide = "c") %>% ## so that rgb value is in separate column.
  rename(im_x = x,
         im_y = y) %>%
  mutate(hex = rgb(c.1,c.2,c.3)) 


pack_layout <- circleProgressiveLayout(rbeta(3040,1,2), sizetype = 'area') %>% 
  mutate(im_x = floor(rescale(x, to = range(im_df$im_x))),  
         im_y = floor(rescale(y, to = range(im_df$im_y))),
         id = row_number()) %>% 
  inner_join(select(im_df, im_x,im_y,hex), by = c("im_x","im_y"))

data_gg <- circleLayoutVertices(pack_layout) %>% 
  inner_join(pack_layout %>% select(id,hex), by=c("id")) %>% 
  filter(hex != "#000000")
  
idx <- unique(data_gg$id)
  
sample <- rerun(nrow(trees), sample(idx, 15))

r_sample <- accumulate(sample, ~c(.x, .y)) 

plot_data <- trees %>% 
  mutate(r_s = r_sample,
         tree_plot = list(data_gg)) %>% 
  mutate(alpha_tree = map2(tree_plot, r_sample, ~mutate(.x, hex = if_else(id %in% .y, alpha(hex, 0.1), hex)))) %>% 
  select(year, alpha_tree) %>% 
  unnest(c(alpha_tree))

labels <- tibble(year = seq(2015, 2210, 10))

plots <- filter(plot_data, year %in% seq(2015, 2210, 10)) %>% 
  ggplot(aes(x = x,y = y)) +
  geom_polygon(aes(fill = hex, group = id)) +  
  geom_text(data = labels, aes(x = 11, y = 17, label = year), family = "Anton", size = 8) +
  labs(x = NULL,
       y = NULL,
       title = "Unless someone like you cares a whole awful lot, nothing is going to get better. It's not.",
       subtitle = str_wrap("In a 2015 study, published in Nature, Thomas Crowther and colleagues mapped tree density across the world. They estimated that there were approximately 3.04 trillion trees in the world. The authors also estimated that over 15 billion trees are cut down each year, and the global number of trees has fallen by almost half (46%) since the start of human civilization.  Each dot on the trees below represent one billion trees. Each year 15 dots are faded out to illustrate the progressive loss. At this rate, our forests will evaporate within 200 years.", 200),
       caption = "**Data**: ourworldindata.org/forests | **Article**: doi.org/10.1038/nature14967 | **Graphic**: @jakekaupp") +
  scale_fill_identity() + 
  coord_equal() +
  facet_wrap(~year, nrow = 2) +
  scale_y_reverse() +
  theme_jk(grid = FALSE,
           ticks = FALSE,
           plot_title_family = "Anton",
           plot_title_size = 30,
           subtitle_size = 16,
           caption_size = 12) +
  theme(strip.text = element_blank(),
        plot.caption = element_markdown(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())

ggsave(here("2021", "week15", "tw15_plot.png"), plots, width = 20, height = 8)

altText::alt_text(plots)
