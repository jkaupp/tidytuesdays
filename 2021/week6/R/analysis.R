library(tidyverse)
library(colorspace)
library(ggtext)
library(here)
library(ragg)
library(magick)

# Reading in Data ----

city_rural <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/city_rural.csv')


# Functions ----

make_text_coords <- function(label, x, y, start_at, nm, plot_ratio = 0.00027, font_size = 1)
{
  
  label_s <-  strsplit(label, split = "")[[1]]
  
  smooth_obj <- smooth.spline(x, y)
  
  # approximate at pair number
  n <- length(label_s)
  
  at_x <- rep(start_at, length.out = n + 1 - length(label_s)%%2)
  
  # forward steps
  for(i in 2:length(at_x)) {
    # slope - adjusted for xy plot ratio
    slop <- (predict(object = smooth_obj, x = at_x[i - 1], deriv = 1)$y)*plot_ratio
    # distance between the points, is cosine of slope angle
    dist_factor <- sin(atan(slop))/slop*font_size
    # update
    at_x[i] <- at_x[i - 1] + dist_factor
  }
  
  
  tibble(label = label_s,
         x = at_x) %>% 
    # cumulative points
    mutate(y = predict(object = smooth_obj, x = x)$y,
           # first derivative, need it for letter spacing and angles
           d1 = predict(object = smooth_obj, x = x, deriv = 1)$y,
           # angle is arctangent of 1st derivative
           # need to correct for xy ratio
           angle_rad = atan(d1*plot_ratio),
           angle = (angle_rad * 180)/pi,
           nm = nm)
  
}


# Simulating data to build plot ----

# Archimedes spiral
a <- 2
b <- 3
theta <- seq(0, 10*pi + 2*pi/8, 0.01)

r <- a + b*theta

spiral <- tibble(x = -r*cos(theta), 
                 y = r*sin(theta)) 


last_pt <- slice(spiral, nrow(spiral))

six_line <- tibble(x = 0:175 + last_pt$x,
       y = 0:175 + last_pt$y)

six_spiral <- spiral %>% 
  slice(-1:-400) %>% 
  bind_rows(six_line) %>% 
  mutate(color = "#DF213D") 

end_six <- slice(six_spiral, nrow(six_spiral))

yellow_line <- tibble(x = -(0:80) + end_six$x,
                      y = 0:80 + end_six$y,
                      color = "#F8BB12")

end_yellow <- slice(yellow_line, nrow(yellow_line))

blue_line <- tibble(x = 0:15 + end_yellow$x,
                    y = 0:15 + end_yellow$y,
                    color = "#4774AD")

end_blue <- slice(blue_line, nrow(blue_line))

green_line <- tibble(x = -(0:200) + end_blue$x,
                     y = end_blue$y,
                     color = "#3C6555")

dubois <- bind_rows(six_spiral, yellow_line, blue_line, green_line)

end_green <- slice(green_line, nrow(green_line))


# Labels ----

label_curve <- tibble(x = -110*cos(seq(0, 2*pi, 0.1)), 
                                y = 110*sin(seq(0, 2*pi, 0.1))) %>% 
  mutate(idx = row_number()) %>% 
  filter(y <= 0)

country_villages_label <- make_text_coords(x = label_curve$x, y = label_curve$y, label = "NEGROES LIVING IN THE COUNTRY AND VILLAGES.", start_at = -105.5, nm = "country_villages", font_size = 7, plot_ratio = 1)

cities_2_5_label <- "37,699\nNEGROES\nIN CITIES\nFROM\n2,000 to 5,000"

cities_5_10_label <- " NEGROES IN CITIES\nFROM 5,000 to 10,000"

cities_gt_10_label <- "78,139 NEGROES IN CITIES\nOF OVER 10,000 INHABITANTS"


# Plot ----

plot <- ggplot(dubois, aes(x = x, y = y)) +  
  geom_path(aes(color = color), size = 4, linejoin = "round", lineend = "round") +
  annotate("text", x = 0, y = 0, label = "734,952", family = "Rajdhani", vjust = 0, hjust = 0.3, size = 3) +
  geom_text(data = country_villages_label, aes(label = label, angle = angle), family = "Rajdhani", size = 3) +
  geom_text(data = end_yellow, label = cities_2_5_label, family = "Rajdhani", lineheight = 0.6, nudge_y = -40, size = 3) +
  geom_text(data = end_blue, label = cities_5_10_label, family = "Rajdhani", lineheight = 0.6, nudge_y = 0, size = 3, hjust = -0.4) +
  geom_text(data = end_blue, label = "8,025", family = "Rajdhani", lineheight = 0.6, nudge_y = 0, size = 3, hjust = -0.35) +
  geom_text(data = end_green, label = cities_gt_10_label, family = "Rajdhani", lineheight = 0.6, nudge_y = -14, size = 3, hjust = 0) +
  scale_color_identity() +
  labs(title = "CITY AND RURAL POPULATION.\n1890.",
       caption = "Original: W.E.B. Du Bois | Reproduction: @jakekaupp") +
  scale_x_continuous(limits = c(-300, 300)) +
  scale_y_continuous(limits = c(-300, 400)) +
  coord_equal() +
  theme_void() +
  theme(plot.background = element_rect(fill = lighten("#EDE0D0", 0.7), color = NA),
        plot.title = element_text(family = "Rajdhani Medium", size = 20, hjust = 0.5),
        plot.caption = element_text(family = "Rajdhani Medium"))

# Output ----
ggsave(here("2021", "week6", "tw6_plot.png"), plot, width = 8, height = 12, device = agg_png())

# Trim ----
image_read(here("2021", "week6", "tw6_plot.png")) %>% 
  image_trim() %>% 
  image_write(here("2021", "week6", "tw6_plot.png"))
