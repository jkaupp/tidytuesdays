library(tidyverse)
library(here)
library(jkmisc)
library(packcircles)
library(colorspace)
library(glue)
library(ggtext)
library(ggforce)
library(scales)
library(magick)

transit_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-05/transit_cost.csv')

nyc_subway_colors <- c("#0039A6", "#FF6319", "#FCCC0A", "#EE352E", "#00933C")

clean_transit <- transit_cost %>% 
  filter(!is.na(cost_km_millions), !is.na(country)) %>% 
  mutate(id = row_number()) 

top_5 <- clean_transit %>% 
  top_n(5, cost_km_millions) %>% 
  pull(line)

clean_transit <- clean_transit %>% 
  mutate(fill = if_else(line %in% top_5, "#A61C3C" , "#808183"),
         alpha = if_else(line %in% top_5, 0.6 , 0.5))


clean_transit[clean_transit$fill == "#A61C3C", 22] <- nyc_subway_colors

packing <- circleProgressiveLayout(clean_transit$cost_km_millions, sizetype = "area")

packing$radius <- 0.95*packing$radius

label_data <- bind_cols(clean_transit, packing)
 
circles <- circleLayoutVertices(packing, npoints = 50) %>% 
  left_join(select(label_data, -x, -y), layout, by = "id")

labels <- filter(label_data, line %in% top_5)

legend_circles <- label_data %>% 
  mutate(groups = cut(radius, 5)) %>% 
  split(.$groups) %>% 
  map_dfr(~top_n(.x, -1, radius)) %>%
  select(radius, cost_km_millions) %>% 
  mutate(x = 0,
         y = max(radius) + radius,
         y_label = max(radius) + 2*radius)

legend <- ggplot(legend_circles, aes(x0 = x, y0 = y, r = radius)) +
  geom_circle(color = "#808183", size = 0.75, fill = "#808183", alpha = 0.5) + 
  geom_text(aes(x = 0, y = y_label, label = dollar(round(cost_km_millions), suffix = " M",accuracy = NULL)), family = "Helvetica Bold", color = lighten("#808183", amount = 0.5), vjust = -0.2) +
  labs(x = NULL,
       y = NULL) +
  coord_equal(clip = "off") +
  theme_jk(dark = TRUE,
           grid = FALSE) +
  theme(legend.position = "none",
        plot.background = element_rect(color = NA, fill = "transparent"),
        axis.text.y = element_blank(),
        axis.text.x = element_blank())

legend_grob <- ggplotGrob(legend)

plot <- ggplot() + 
  geom_polygon(data = circles, aes(x = x, y = y, group = id, fill = fill, color = darken(fill, 0.4), alpha = alpha),  size = 0.2) +
  geom_text(data = labels, aes(x, y,  label = str_wrap(line, 10), size = 6), color = lighten("#808183", amount = 0.5), family = "Helvetica Bold") +
  annotation_custom(legend_grob, xmin = 145, xmax = 225, ymin = 145, ymax = 225) +
  scale_fill_identity() +
  scale_color_identity() +
  scale_alpha_identity() +
  coord_equal(clip = "off") +
  labs(x = NULL,
       y = NULL,
       title = "The 5 Most Expensive Subway Projects In The World Are In NYC",
       subtitle = "The circles below represent 535 subway projects around the world with each area proportional to<br>the cost in millions of dollars per kilometer.",
       caption = "**Data**: TransitCosts.com | **Graphic**: @jakekaupp") +
  theme_jk(base_family = "Helvetica",
           plot_title_family = "Helvetica Bold",
           plot_title_size = 26,
           subtitle_size = 16,
           caption_size = 14,
           subtitle_family = "Helvetica",
           caption_family = "Helvetica",
           markdown = TRUE,
           grid = FALSE,
           dark = TRUE) + 
  theme(legend.position = "none",
        plot.background = element_rect(fill = "black"),
        plot.title = element_markdown(hjust = 0.5),
        plot.subtitle = element_markdown(hjust = 0.5),
        axis.text.y = element_blank(),
        axis.text.x = element_blank()) 
  

ggsave(here("2021", "week2", "tw2_plot.png"), plot, width = 13, height = 13, dev = ragg::agg_png())

image_read(here("2021", "week2", "tw2_plot.png")) %>% 
  image_trim() %>% 
  image_write(here("2021", "week2", "tw2_plot.png"))
