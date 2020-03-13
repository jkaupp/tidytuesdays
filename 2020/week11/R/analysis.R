library(tidyverse)
library(jkmisc)
library(glue)
library(here)
library(patchwork)
library(ggbeeswarm)
library(ggmap)
library(tidycensus)
library(albersusa)
library(sf)
library(colorspace)
library(ggtext)

diversity_school <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/diversity_school.csv')


if (!file.exists(here("2020", "week11", "data", "school_div.RDS"))) {
  
  school_div <- diversity_school %>% 
    filter(!category %in% c("Total Minority", "Women", "Non-Resident Foreign")) %>% 
    group_by(name, state, total_enrollment) %>% 
    mutate(num = enrollment * (enrollment - 1),
           den = total_enrollment * (total_enrollment - 1)) %>% 
    summarize(D_school = 1 - (sum(num)/unique(den))) %>% 
    ungroup() %>% 
    select(-total_enrollment) %>% 
    mutate(location = sprintf("%s, %s", name, state)) %>% 
    mutate_geocode(location = location, output = "latlon") 
   
} else {
  
  school_div <- readRDS(here("2020", "week11", "data", "school_div.RDS"))
  
}


racevars <- as.character(glue("P00{8003:8009}"))

states <- get_decennial(geography = "state", variables = racevars, summary_var = "P008001")

state_div <- states %>% 
  group_by(NAME, GEOID, summary_value) %>% 
  mutate(num = value * (value - 1),
         den = summary_value * (summary_value - 1)) %>% 
  group_by(NAME) %>% 
  summarize(D_state = 1 - (sum(num)/unique(den)))

albers <- usa_sf() %>% 
  filter(!name %in% c("Hawaii", "Alaska")) 

albers_outline <- albers %>% 
  st_union() %>% 
  st_buffer(dist = 0.1)

plot_data <- left_join(school_div, state_div, c("state" = "NAME")) %>% 
  mutate(ratio = D_school/D_state) %>% 
  mutate(color = if_else(D_school >= D_state, 1, 0)) %>% 
  replace_na(list(color = 0)) %>% 
  filter(!is.na(name), !state %in% c("Hawaii", "Alaska")) %>% 
  filter(!str_detect(name, "Le Cordon Bleu"))

dots <- ggplot(plot_data, aes(x = ratio, y = 0)) +
  geom_quasirandom(aes(fill = factor(color), color = factor(color)), groupOnX = FALSE, shape = 21, show.legend = FALSE) +
  geom_vline(xintercept = 1, color = "#656176") +
  annotate(GeomRichText, label = highlight_text("Overall Distribution", "#2E3440", 'b'), x = 2, y = 0.4, fill = NA, label.color = NA, family = "Oswald", hjust = 0, vjust = 1) +
  scale_color_manual(values = darken(c("#656176", "#1b998b"))) +
  scale_fill_manual(values = c("#656176", "#1b998b")) +
  labs(y = NULL,
       x = NULL) +
  theme_jk(grid = FALSE,
           dark = FALSE) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.background = element_rect(color = NA))

lon_dots <- ggplot(plot_data, aes(x = lon, y = 0)) +
  geom_quasirandom(aes(fill = factor(color), color = factor(color)), groupOnX = FALSE, shape = 21, show.legend = FALSE) +
  scale_color_manual(values = darken(c("#656176", "#1b998b"))) +
  scale_fill_manual(values = c("#656176", "#1b998b")) +
  scale_x_continuous(limits = range(plot_data$lon, na.rm = TRUE)) +
  labs(y = NULL,
       x = NULL) +
  theme_jk(grid = FALSE,
           dark = FALSE) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.background = element_rect(color = NA))

lat_dots <- ggplot(plot_data, aes(x = 0, y = lat)) +
  geom_quasirandom(aes(fill = factor(color), color = factor(color)), groupOnX = TRUE, shape = 21, show.legend = FALSE) +
  scale_color_manual(values = darken(c("#656176", "#1b998b"))) +
  scale_fill_manual(values = c("#656176", "#1b998b")) +
  scale_y_continuous(limits = range(plot_data$lat, na.rm = TRUE)) +
  labs(y = NULL,
       x = NULL) +
  theme_jk(grid = FALSE,
           dark = FALSE) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.background = element_rect(color = NA))




map <- ggplot(plot_data) +
  geom_sf(data = albers_outline, fill = lighten("#eceff4"), size = 0.5) +
  geom_sf(data = albers, fill = "#eceff4", size = 0.1) +
  geom_point(aes(x = lon, y = lat, fill = factor(color), color = factor(color)), shape = 21, show.legend = FALSE) +
  annotation_custom(ggplotGrob(dots), xmin = -128, ymin = 23, xmax = -104.7, ymax = 30) +
  scale_color_manual(values = darken(c("#656176", "#1b998b"))) +
  scale_fill_manual(values = c("#656176", "#1b998b")) +
  coord_sf(clip = "off") +
  labs(x = NULL,
       y = NULL) +
  theme_jk(grid = FALSE,
           dark = FALSE,
           markdown = TRUE) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())


layout <- "
#BB
ACC
ACC
"


out <- wrap_plots(lat_dots, lon_dots, map, design = layout, widths = c(0.1, 0.45, 0.45), heights = c(0.1, 0.45, 0.45)) +
  plot_annotation(title = glue("Are Institutions {highlight_text('more diverse','#1b998b', 'b')} or {highlight_text('less diverse','#656176', 'b')}, than the states they are in?"),
                  subtitle = glue("Illustrated below is a map of the continental US with points representing higher education institutions.  The marginal plots illustrate the<br>longitudinal/latitudinal distribution of institutions, with the overall distribution of diversity ratios shown on the inset plot. The color of each dot<br>represents whether or not the Simpsons Diversity Index ratio is {highlight_text('greater than','#1b998b', 'b')}, or {highlight_text('less than','#656176', 'b')} one."),
                  caption = "**Data**: Diversity - Chronicle for Higher Education (2016), State Diversity - US Census | **Graphic**: @jakekaupp",
                  theme = theme_jk(markdown = TRUE))

ggsave(here("2020", "week11", "tw11_plot.png"), out, width = 12, height = 8, device = ragg::agg_png())

