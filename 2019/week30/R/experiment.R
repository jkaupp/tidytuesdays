library(tidyverse)
library(here)
library(jkmisc)
library(ggforce)
library(geofacet)
library(patchwork)
library(glue)
library(cowplot)

wildlife_impacts <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-23/wildlife_impacts.csv")


# Petal charts ----
plot_data <- wildlife_impacts %>% 
  filter(state %in% state.abb) %>% 
  count(state,  incident_month, incident_year) %>% 
  group_by(incident_year, state) %>% 
  mutate(percent = n/sum(n)) %>% 
  mutate(percent = ifelse(is.nan(percent), 0, percent)) %>% 
  mutate(angle = 90 - (incident_month-1)*30,
         angle = ifelse(angle < 0, 360 + angle, angle),
         radians = angle*pi/180,
         x0 = percent * cos(radians),
         y0 = percent * sin(radians))
         
         
big_flower <- ggplot(plot_data) +
  geom_ellipse(aes(x0 = x0, y0 = y0, a = percent, b = percent/3, angle = radians, fill = incident_year), alpha = 0.2, size = 0, color = "white") +
  scale_fill_viridis_c(option = "plasma", direction = 1) +
  theme_jk(grid = FALSE, plot_title_size = 12) +
  labs(x = NULL, y = NULL, title = "National") +
  theme(axis.text = element_blank(),
        legend.position = "none",
        panel.spacing = unit(0.1, "lines"),
        plot.title = element_text(hjust = 0.5)) +
  coord_equal() 

state_flower <- big_flower +
  facet_geo(~ state, grid = "us_state_grid2")


flower_axes_lines <- tibble(idx = 1:12,
                            angle = 90 - (idx-1)*30,
                            angle2 = ifelse(angle < 0, 360 + angle, angle),
                            radians = angle2*pi/180)

axes_lines <- function(radius) {
  
  tibble(segment = 1:6,
                     x = c(0, radius*cos(pi/3), radius*cos(pi/6), radius, radius*cos(pi/6), radius*cos(pi/3)),
                     xend = c(0, -radius*cos(pi/3), -radius*cos(pi/6), -radius, -radius*cos(pi/6), -radius*cos(pi/3)),
                     y = c(radius, radius*sin(pi/3), radius*sin(pi/6), 0, -radius*sin(pi/6), -radius*sin(pi/3)),
                     yend = c(-radius, -radius*sin(pi/3), -radius*sin(pi/6), 0, radius*sin(pi/6), radius*sin(pi/3))) 
  }

axes_labels <- function(radius) {
  tibble(month = 1:12,
                      label = month.abb[month],
                      x = c(axes_lines(radius)$x, axes_lines(radius)$xend),
                      y = c(axes_lines(radius)$y, axes_lines(radius)$yend))  }


flower_legend <- plot_data %>% 
  filter(state == "ME") %>% 
  ggplot(aes(x = incident_month, y = percent, group = incident_year, color = incident_year, fill = incident_year)) +
  geom_segment(data = axes_lines(2), aes(x = x, xend = xend, y = y , yend = yend), size = 0.1, color = "#cccccc", inherit.aes = FALSE) +
  geom_circle(aes(x0 = 0, y0 = 0, r = 2), inherit.aes = FALSE, size = 0.1, color = "#cccccc") +
  geom_circle(aes(x0 = 0, y0 = 0, r = 1), inherit.aes = FALSE, size = 0.1, color = "#cccccc") +
  geom_ellipse(aes(x0 = x0, y0 = y0, a = percent, b = percent/3, angle = radians, fill = incident_year), alpha = 0.5, size = 0.1, color = "white") +
  geom_mark_circle(aes(x = 2*x0, y = 2*y0, label = glue("{month.name[incident_month]}, {incident_year}"), description = "Single colour long petal represents 100% of collison event during this month and year", filter = incident_year == 1991 & incident_month == 3), expand = unit(1, "mm"), label.family = c("Oswald", "Scope One"), label.fontsize = 10, label.buffer = unit(5, "mm"), inherit.aes = FALSE) +
  geom_mark_circle(aes(x = x0, y = y0, label = glue("{month.name[incident_month]}, Multiple years"), description = "Multiple coloured petals represent repeated annual incidents during this month", filter = incident_year == 1996 & incident_month == 11), expand = unit(1, "mm"), label.family = c("Oswald", "Scope One"), label.fontsize = 10, label.buffer = unit(5, "mm"), inherit.aes = FALSE) +
  geom_text(data = filter(axes_labels(2.15), label != "Feb"), aes(x = x, y = y, label = label), inherit.aes = FALSE, family = "Oswald") +
  scale_color_viridis_c(option = "plasma", direction = 1) +
  scale_fill_viridis_c(option = "plasma", direction = 1) +
  labs(x = NULL, y = NULL) +
  theme_jk(grid = FALSE) +
  coord_fixed(clip = "off") +
  theme(axis.text = element_blank(),
        legend.position = "none")

color_legend <- tibble(year = 1990:2018,
                       y = 1) %>% 
  ggplot() +
  geom_tile(aes(x = year, y = y, fill = year), show.legend = FALSE, color = "white", size = 0.1) +
  scale_fill_viridis_c(option = "plasma", direction = 1) +
  scale_x_continuous(breaks = c(1990, 2000, 2010, 2018)) +
  coord_equal() +
  theme_jk(grid = FALSE) +
  labs(x = NULL, y = NULL) +
  theme(axis.text.y = element_blank())


finished_legend <- ggdraw() +
  draw_plot(flower_legend, 0, 0, 1, 1) +
  draw_plot(color_legend, 0.3, -0.175, 0.4, 0.4)

state_flower_grid <- ggdraw() +
  draw_plot(state_flower, 0, 0, 1, 1) + 
  draw_plot(big_flower, 0.75, 0.15, 0.25, 0.25)

out <- wrap_plots(finished_legend, state_flower_grid, nrow = 1, widths = c(0.85, 1.2)) +
  plot_annotation(title = "Seasonality of Wildlife-Aircraft Collisions by State",
                  subtitle = str_wrap("Presented below is a petal chart of of wildlife collisions with aircraft across the US from 1990-2018. Below this is an inset legend showing assisting interpretation of the plots.  On the right are wildlife-aircraft collisions by state presented as small multiples, geographically arranged, with an inset flower representing the National data. Petal length is the annual proportion of collisions in a given month.  Smaller compact flowers illustrate states with collisions occuring year round, while the bigger flowers tend to see single or concentrated spikes of collision activity.  Flowers with diverse colours indicate repeated annual collisons while the single-hued flowers illustrate more sparse or isolated annual events.", 210),
                  caption = "Data: FAA Wildlife Strike Database | Graphic: @jakekaupp",
                  theme = theme_jk())


ggsave(here("2019","week30", "tw30_plot_remix.png"), out, width = 16, height = 10, type = "cairo")
