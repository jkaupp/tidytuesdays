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
  #mutate(airport_type = if_else(str_detect(airport, "INTL"), "INT", "DOM")) %>% 
  filter(state %in% state.abb) %>% 
  count(state,  incident_month, incident_year) %>% 
  complete(incident_year = 1990:2018, state,  incident_month = 1:12, fill = list(n = 0)) %>% 
  group_by(incident_year, state) %>% 
  mutate(percent = n/sum(n)) %>% 
  mutate(percent = ifelse(is.nan(percent), 0, percent))


state_flower_grid <- plot_data %>% 
  ggplot(aes(x = incident_month, y = percent, group = incident_year, color = incident_year, fill = incident_year)) +
  geom_area(position = position_identity(), alpha = 0.5, size = 0.1) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  scale_y_continuous(limits = c(0,1), breaks = c(0.5, 0.1)) +
  scale_fill_viridis_c("Year", option = "plasma", direction = 1) +
  scale_color_viridis_c(option = "plasma", direction = 1) +
  labs(x = NULL, 
       y = NULL) +
  coord_polar() +
  facet_geo(~ state, grid = "us_state_grid2") +
  theme_jk(grid = FALSE) +
  theme(axis.text = element_blank(),
        legend.position = "none",
        panel.spacing = unit(0.1, "lines"))

flower_legend <- plot_data %>% 
  filter(state == "ME") %>% 
  ggplot(aes(x = incident_month, y = percent, group = incident_year, color = incident_year, fill = incident_year)) +
  geom_area(position = position_identity(), alpha = 0.5, size = 0.1) +
  geom_mark_circle(aes(label = glue("{month.name[incident_month]}"), description = "Single colour petal represents a single collison event during this month", filter = incident_year == 1991 & incident_month == 3), expand = unit(1, "mm"), label.family = c("Oswald", "Scope One"), label.fontsize = 10, label.buffer = unit(5, "mm")) +
  geom_mark_circle(aes(label = glue("{month.name[incident_month]}"), description = "Multiple coloured petals represent repeated annual incidents during this month", filter = incident_year == 1996 & incident_month == 11), expand = unit(1, "mm"), label.family = c("Oswald", "Scope One"), label.fontsize = 10, label.buffer = unit(5, "mm")) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  scale_y_continuous(limits = c(0,1), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  scale_fill_viridis_c("Year", option = "plasma", direction = 1, breaks = c(seq(1990, 2020, by = 5))) +
  scale_color_viridis_c(option = "plasma", direction = 1) +
  guides(fill = guide_colorbar(), color = "none") +
  labs(x = NULL, y = NULL) +
  coord_polar(clip = "off") +
  theme_jk(grid = "XY") +
  theme(axis.text.y = element_blank(),
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
  draw_plot(color_legend, 0.3, -0.17, 0.4, 0.4)

out <- wrap_plots(finished_legend, state_flower_grid,  nrow = 1, widths = c(0.85, 1.2)) +
   plot_annotation(title = "Seasonality of Wildlife-Aircraft Collisions by State",
                   subtitle = str_wrap("Presented below is a petal chart of of wildlife collisions with aircraft, with an inset legend showing assisting interpretation.  Wildlife collisions by state are presented as small multiples, geographically arranged.  Smaller compact flowers illustrate states with collisions occuring year round, while the bigger flowers tend to see single or concentrated spikes of collision activity.  Flowers with diverse colours indicate repeated annual collisons while the single-hued flowers illustrate more sparse or isolated annual events.", 210),
                   caption = "Data: FAA Wildlife Strike Database | Graphic: @jakekaupp",
                   theme = theme_jk())


ggsave(here("2019","week30", "tw30_plot.png"), out, width = 16, height = 10, type = "cairo")


