library(tidyverse)
library(waffle)
library(lubridate)
library(jkmisc)
library(scales)
library(colorspace)
library(patchwork)
library(ggtext)
library(glue)
library(here)

#Get the data ----
nyc_squirrels <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-29/nyc_squirrels.csv")

# Process the data ----
activity_data <- nyc_squirrels %>% 
  mutate(date = mdy(date)) %>% 
  pivot_longer(names_to = "activity", running:foraging) %>%
  group_by(date, activity) %>% 
  summarize(value = sum(as.numeric(value))) %>% 
  mutate(activity = factor(activity, labels = unique(activity)),
         activity = fct_reorder(activity, value, .fun = sum)) %>% 
  arrange(date, desc(activity))

# Make my palette

slate_ramp <- colorRampPalette(c("#3B454A", lighten("#3B454A", 0.8)))(5) 

grey_ramp <- grey.colors(5, 0.5, 0.9)

pal <- set_names(slate_ramp, unique(activity_data$activity))

pal["foraging"] <- "#DD2A7B"

partition_waffle <- function(x, start, nrows, flip = FALSE) {
  
   offset <- start - x
  
   offset_rows <- offset %/% nrows
   
   offset_blocks <- offset %% nrows
   
  
   comp_blocks <- nrows - offset_blocks
   
   if (comp_blocks != nrows) {
     
     rows <- (x - comp_blocks) %/% nrows
     
     blocks <- (x - comp_blocks) %% nrows
     
     start_row <- offset_rows + rows + 1
     
     if (blocks == 0) {
       
       end_row <- start_row
       
     } else {
       
       end_row <- start_row + 1 
       
     }
     
   } else {
     
     rows <- x %/% nrows
     
     blocks <- x %% nrows
     
     start_row <- rows + offset_rows
     
     end_row <- rows + offset_rows + 1
     
   }
   
   
   if (flip) {
     
     tibble(y = c(start_row, start_row, end_row),
            yend = c(start_row, end_row, end_row),
            x = c(blocks, blocks, 0),
            xend = c(nrows, blocks, blocks))
     
     
   } else {
     
     tibble(x = c(start_row, start_row, end_row),
            xend = c(start_row, end_row, end_row),
            y = c(blocks, blocks, 0 -1),
            yend = c(nrows + 1, blocks, blocks))
   }
     
     

   
  
} 

# Waffles by activity ----

activity_outlines <- activity_data %>% 
  ungroup() %>% 
  arrange(desc(activity), date) %>% 
  mutate(cum_sum = cumsum(value)) %>% 
  mutate(lines = map2(value, cum_sum, partition_waffle, flip = FALSE, nrows = 20)) %>% 
  unnest(lines) %>% 
  filter(date == last(date))

activity_labels <- activity_data %>% 
  group_by(activity) %>% 
  summarize(total = sum(value)) %>%
  left_join(filter(activity_outlines, yend == 21), by = "activity")

waffle_by_activity <- activity_data %>% 
  arrange(desc(activity), date) %>% 
  ggplot(aes(fill = activity, values = value)) +
  geom_waffle(color = "white", size = 0.25, n_rows = 20, flip = FALSE, show.legend = FALSE) +
  geom_segment(data = activity_outlines, aes(x = x, xend = xend, y = y, yend = yend), color = "black", size = 0.5) +
  geom_richtext(data = activity_labels, aes(label = glue("<b>{str_to_title(activity)}</b>"), x = x, y = yend, color = activity), fill = "white", hjust = 1, vjust = 0, family = "Oswald", show.legend = FALSE, label.color = NA, label.padding = grid::unit(rep(0, 4), "pt")) +
  geom_richtext(data = activity_labels, aes(label = glue("{total}"), x = x + 1, y = -1), fill = "white", color = "black", hjust = 0, vjust = 1, family = "Oswald", show.legend = FALSE, label.color = NA, label.padding = grid::unit(rep(0, 4), "pt")) +
  scale_fill_manual(values = pal) +
  scale_color_manual(values = pal) +
  coord_equal() +
  labs(x = NULL, 
       y = NULL) +
  theme_jk(grid = FALSE) +
  scale_x_continuous(expand = c(0,0)) +
  expand_limits(y = c(-5, 25), x = c(0, 200)) +
  theme(axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        strip.text = element_blank(),
        panel.spacing.y = unit(0.1, "lines"),
        panel.spacing.x = unit(0.1, "lines")) 

# Waffles by day ----

by_day_outlines <- activity_data %>% 
  ungroup() %>% 
  mutate(cum_sum = cumsum(value)) %>% 
  mutate(lines = map2(value, cum_sum, partition_waffle, nrows = 20, flip = FALSE)) %>% 
  unnest(lines) %>% 
  filter(activity == "chasing")

by_day_labels <- activity_data %>% 
  group_by(date) %>% 
  summarize(total = sum(value)) %>% 
  left_join(filter(by_day_outlines, yend == 21))

waffle_by_day <- activity_data %>% 
  ggplot(aes(fill = activity, values = value)) +
  geom_waffle(color = "white", size = 0.25, n_rows = 20, flip = FALSE, show.legend = FALSE) +
  geom_segment(data = by_day_outlines, aes(x = x, xend = xend, y = y, yend = yend), color = "grey20", size = 0.5) +
  geom_richtext(data = by_day_labels, aes(label = glue("{month.abb[month(date)]} {day(date)}"), x = x, y = yend), fill = "white", color = "black", hjust = c(rep(1,10), 0), vjust = 0, family = "Oswald", show.legend = FALSE, label.color = NA, label.padding = grid::unit(rep(0, 4), "pt")) +
  geom_richtext(data = by_day_labels, aes(label = glue("{total}"), x = x, y = -1), fill = "white", color = "black", hjust = c(rep(0,9),1,0), vjust = 1, family = "Oswald", show.legend = FALSE, label.color = NA, label.padding = grid::unit(rep(0, 4), "pt")) +
  scale_fill_manual(values = pal) +
  scale_x_continuous(expand = c(0,0)) +
  coord_equal(clip = "off") +
  labs(x = NULL, 
       y = NULL) +
  expand_limits(y = c(-5, 25), x = c(0, 205)) +
  theme_jk(grid = FALSE) +
  theme(axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        strip.text = element_blank(),
        panel.spacing.y = unit(0.1, "lines"),
        panel.spacing.x = unit(0.1, "lines")) 

# Waffle bars by day ----

waffle_day_outlines  <- activity_data %>% 
  arrange(desc(date)) %>% 
  filter(activity == "foraging") %>% 
  ungroup() %>%
  group_split(date) %>% 
  map(~mutate(.x, cum_sum = cumsum(value))) %>%
  map_dfr(~mutate(.x, lines = map2(value, cum_sum, partition_waffle, nrows = 20, flip = FALSE))) %>%
  unnest(lines) 

waffle_labels <- waffle_day_outlines %>% 
  filter(y == -1)

mday_label <- function(x) {
  
  glue("{month.abb[month(x)]} {day(x)}")
  
}

split_waffle_by_day <- activity_data %>% 
  arrange(desc(date)) %>% 
  ggplot(aes(fill = activity, values = value)) +
  geom_waffle(color = "white", size = 0.25, n_rows = 20, flip = FALSE, show.legend = FALSE) +
  geom_segment(data = waffle_day_outlines, aes(x = x, xend = xend, y = y, yend = yend), color = "grey20", size = 0.5) +
  geom_richtext(data = waffle_labels, aes(label = glue("{value}"), x = x + 1, y = -1), fill = "white", color = "black", hjust = 0, vjust = 1, family = "Oswald", show.legend = FALSE, label.color = NA, label.padding = grid::unit(rep(0, 4), "pt")) +
  scale_fill_manual(values = pal) +
  coord_equal() +
  scale_color_manual(values = pal) +
  labs(x = NULL,
       y = NULL) +
  theme_jk(grid = FALSE) +
  facet_wrap(~date, nrow = 1, as.table = FALSE, strip.position = "top", labeller = labeller(date = mday_label)) +
  expand_limits(y = c(-5, 20)) +
  theme(axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.spacing.y = unit(0.1, "lines"),
        panel.spacing.x = unit(0.1, "lines")) 

out <- wrap_plots(waffle_by_activity, waffle_by_day, split_waffle_by_day, ncol = 1) +
  plot_annotation(title = "Breakdown of Observed Squirrel Activity from the 2018 NYC Squirrel Census",
                  subtitle = glue("Below are  waffle charts of activity totals (I), daily totals (II) and exploded daily activity (III) views of observed squirrel activity.<br>{highlight_text('Foraging', '#DD2A7B', 'b')} is the most frequently observed activity recorded in the census, not surprising for squirrels in the fall."),
                  caption = "Data: **NYC Data Portal** | Graphic: **@jakekaupp**",
                  tag_levels = "I",
                  theme = theme_jk(markdown = TRUE))

ggsave(here("2019", "week44", "tw44_plot.png"), out, width = 11, height = 8, dev = ragg::agg_png(), dpi = "print")

