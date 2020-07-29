library(tidyverse)
library(jkmisc)
library(here)
library(glue)
library(readxl)
library(fs)
library(sf)
library(janitor)
library(ggforce)
library(cowplot)
library(grid)

animal_complaints <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-21/animal_complaints.csv') %>% 
  clean_names()

## Code and Shapefiles courtesy of @geokaramanis
subs <- st_read(here("2020", "week30", "data", "TCC_Suburbs-shp", "TCC_Suburbs.shp")) %>% 
  st_union() %>% 
  st_set_crs(4326)

divs <- st_read(here("2020", "week30", "data", "Townsville_Final_Divisions-ESRI", "Townsville_City_Divisions.shp")) %>% 
  st_as_sf() %>% 
  st_transform(4326)

# Get intersection of electoral divisions and city boundaries
divs_clipped <- st_intersection(divs, subs) %>% 
  clean_names()

# Population Tables https://profile.id.com.au/townsville/
files <- dir_ls(here("2020", "week30", "data"), regexp = "estimated")

population <- map(files, ~read_csv(.x)) %>% 
  reduce(left_join) %>% 
  pivot_longer(-Year, names_to = "electoral_division", values_to = "population") %>% 
  clean_names()
  
# Bivariate Choropleth on cats v dogs
complaint_data <- animal_complaints %>% 
  filter(electoral_division != "Unallocated") %>% 
  mutate(year = parse_number(date_received)) 

avg_pop <- population %>% 
  group_by(electoral_division) %>% 
  summarize(population = mean(population))

total_complaints <- count(complaint_data, animal_type, electoral_division) %>% 
  inner_join(avg_pop) %>% 
  inner_join(divs_clipped, ., by = c( "division" = "electoral_division")) %>% 
  mutate(cmp_per_cap = 1000 * n/population) %>% 
  filter(animal_type == "dog")

overall_cd <- count(complaint_data, animal_type, complaint_type, electoral_division) %>% 
  inner_join(avg_pop) %>% 
  inner_join(divs_clipped, ., by = c( "division" = "electoral_division")) %>% 
  mutate(cmp_per_cap = 1000 * n/population) %>% 
  filter(animal_type == "dog")

tot_cd_yearly <- complaint_data %>% 
  count(animal_type, complaint_type, electoral_division, year) %>% 
  inner_join(population) %>% 
  left_join(divs_clipped, ., by = c("division" = "electoral_division")) %>% 
  mutate(cmp_per_cap = 1000 * n/population) %>% 
  filter(animal_type == "dog") %>% 
  arrange(division_id, complaint_type, year) %>% 
  mutate(division = factor(division,  glue("Division {1:10}")))


big_map <- ggplot(total_complaints) +
  geom_sf(data = st_buffer(subs, 0.005, singleSide = TRUE), color = "white", size = 0.01) +
  geom_sf(aes(fill = cmp_per_cap), size = 0.1, color = "white") +
  geom_sf_text(data = filter(total_complaints, division_id %in% 5:9), aes(label = division_id), family = "Oswald", color = "#2E3440") +
  coord_sf(clip = "off", 
           xlim = c(145.5, 147.5),
           ylim = c(-20.1, -18.5)) +
  scale_fill_distiller("Complaints per 1000 people", palette = "RdPu", direction = 1, guide = guide_bins(title.position="top")) + 
  labs(x = NULL, y = NULL) +
  theme_jk(grid = FALSE,
           dark = TRUE) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = c(0.2, 0.5),
        legend.direction = "horizontal")

make_bars <- function(data) {
  
  ggplot(data, aes(y = complaint_type, x = n)) +
    geom_col(fill = "#c51b8a") +
    geom_text(aes(label = complaint_type, x = 1), hjust = 0, color = "white", family = "Oswald", size = 3) +
    labs(x = NULL,
         y = NULL,
         title = unique(data$division)) +
    scale_x_continuous(expand = c(0,0)) +
    theme_jk(grid = FALSE,
             dark = TRUE, 
             strip_text_family = "Anton",
             plot_title_size = 12) +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          plot.background = element_rect(color = NA,
                                         fill = "transparent"),
          legend.position = "none") 
  }
  
bars <- overall_cd %>% 
  filter(division_id %in% c(1:4, 10)) %>% 
  split(.$division) %>% 
  map(make_bars) %>% 
  map(ggplotGrob)


other_bars <- overall_cd %>% 
  filter(!division_id %in% c(1:4, 10)) %>% 
  ggplot(aes(y = complaint_type, x = n)) +
  geom_col(fill = "#c51b8a") +
  geom_text(aes(label = complaint_type, x = 1), hjust = 0, color = "white", family = "Oswald", size = 3) +
  labs(x = NULL,
       y = NULL) +
  scale_x_continuous(expand = c(0,0)) +
  theme_jk(grid = FALSE,
           dark = TRUE, 
           plot_title_size = 12) +
  facet_wrap(~division, nrow = 1) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.background = element_rect(color = NA,
                                       fill = "transparent"))
  

circle <- ggplot(subs) +
  geom_circle(aes(x0 = 0, y0 = 0, r = 5), color = "white", size = 1) +
  labs(x = NULL,
       y = NULL) +
  theme_jk(grid = FALSE,
           dark = TRUE) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.background = element_rect(color = NA,
                                       fill = "transparent"),
        legend.position = "none") +
  coord_equal()

rectangle <- roundrectGrob(gp = gpar(fill = "transparent", col = "white", lwd = 3), width = 3, height = 1.2)

circle_grob <- ggplotGrob(circle)


plot <- ggdraw(big_map) +
  #Div 1
  draw_grob(ggplotGrob(circle), scale = 0.275, x = -0.31, y = 0.35) +
  draw_grob(bars[[1]], scale = 0.15, x = -0.3, y = 0.35) +
  draw_line(x = c(0.25, 0.4),
            y = c(0.755, 0.675),
            color = "white") +
  #Div 10
  draw_grob(ggplotGrob(circle), scale = 0.275, x = 0.38, y = 0) +
  draw_grob(bars[[2]], scale = 0.15, x = 0.38, y = 0) +
  draw_line(x = c(0.7, 0.81),
            y = c(0.35, 0.42),
            color = "white") +
  #Div 2
  draw_grob(ggplotGrob(circle), scale = 0.275, x = 0.0, y = 0.35) +
  draw_grob(bars[[3]], scale = 0.15, x = 0.0, y = 0.35) +
  draw_line(x = c(0.57, 0.52),
            y = c(0.55, 0.74),
            color = "white") +
  #Div 3
  draw_grob(ggplotGrob(circle), scale = 0.275, x = 0.3, y = 0.35) +
  draw_grob(bars[[4]], scale = 0.15, x = 0.3, y = 0.35) +
  draw_line(x = c(0.61, 0.75),
            y = c(0.55, 0.75),
            color = "white") +
  #Div 4 
  draw_grob(ggplotGrob(circle), scale = 0.275, x = -0.31, y = 0) +
  draw_grob(bars[[5]], scale = 0.15, x = -0.31, y = 0) +
  draw_line(x = c(0.3, 0.57),
            y = c(0.5, 0.40),
            color = "white") +
  #Div 5-9
  draw_grob(ggplotGrob(other_bars), scale = 0.15, x = -0.75, y = -0.37, width = 3, height = 1) +
  draw_grob(rectangle, scale = 0.15, x = 0.25, y = -0.37) +
  draw_line(x = c(0.61, 0.63),
            y = c(0.5, 0.23),
            color = "white") +
  draw_text("Townsville Dog Complaints Per Captia by Electoral District", family = "Oswald", size = 20, x = 0.02, y = 0.208, color = "white", hjust = 0) +
  draw_label("The map above shows the total number of dog complaints per 100 people from\n2013-2020. The callouts show the complaints broken out by type for each\nelectoral district.\n\nData: data.gov.au | Graphic: @jakekaupp", fontfamily = "Lato", size = 12, x = 0.02, y = 0.125, color = "white", hjust = 0)

  
ggsave(here("2020", "week30", "tw30_plot.png"), plot = plot, width = 12, height = 10)

  
