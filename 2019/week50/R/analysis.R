library(tidyverse)
library(lubridate)
library(here)
library(readxl)
library(scales)
library(jkmisc)
library(ggalt)
library(ggtext)
library(colorspace)
library(cowplot)
library(gridtext)


import_export <- here("2019","week50","data", "playfair.xlsx") %>% 
  read_excel(col_names = c("date", "imports", "exports")) %>% 
  mutate(year = as.numeric(str_sub(date, 1, 4)))

long_ie <- import_export %>% 
  pivot_longer(imports:exports) 

labels <- seq(0, 190000, 10000)/1000

labels[labels == 100] <- "100,000"
labels[labels == 0] <- ""




# Labels that follow text 

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
         year = at_x) %>% 
    # cumulative points
    mutate(value = predict(object = smooth_obj, x = year)$y,
           # first derivative, need it for letter spacing and angles
           d1 = predict(object = smooth_obj, x = year, deriv = 1)$y,
           # angle is arctangent of 1st derivative
           # need to correct for xy ratio
           angle_rad = atan(d1*plot_ratio),
           angle = (angle_rad * 180)/pi,
           nm = nm)
  
}


import_line_label <- make_text_coords(x = import_export$year, y = import_export$imports, label = "Line of Imports", start_at = 1720, nm = "Imports")
export_line_label <- make_text_coords(x = import_export$year, y = import_export$exports, label = "Line of Exports", start_at = 1720, nm = "Exports")

import_label <- make_text_coords(x = import_export$year, y = import_export$imports, label = "Imports", start_at = 1770, nm = "Imports")
export_label <- make_text_coords(x = import_export$year, y = import_export$exports, label = "Exports", start_at = 1763, nm = "Exports")

balance_against <- make_text_coords(x = import_export$year, y = import_export$exports, label = "Balance against", start_at = 1725, nm = "Exports", font_size = 1.6)

balance_in_favour <- tibble(year = 1770,
                            value = 122000,
                            name  = NA,
  label = "<span style='font-family: \"Playfair Display SC\"'><b>Balance</b></span> <span style='font-family: \"Playfair Display\"; font-size: 12px;'>in</span><br><span style='font-family: \"Playfair Display SC\"'><b>favour</b></span> <span style='font-family: \"Playfair Display\"; font-size: 12px;'>of</span><br><span style='font-family: \"Playfair Display SC\"'><b>England.</b></span>")

playfair <- ggplot(long_ie, aes(x = year, y = value, color = name)) +
  geom_hline(yintercept = 100000, size = 1.5, color = "grey50") +
  geom_ribbon(data = filter(import_export, year <= 1755), aes(x = year, ymin = imports, ymax = exports), inherit.aes = FALSE, fill = darken("#F7DAD7"), alpha = 0.4) +
  geom_ribbon(data = filter(import_export, year >= 1755), aes(x = year, ymin = imports, ymax = exports), inherit.aes = FALSE, fill = darken("#E7D9B5", amount = 0.3), alpha = 0.4) +
  geom_xspline(show.legend = FALSE, size = 2) +
  geom_text(data = import_line_label, aes(x = year, y = value, label = label, angle = angle), vjust = -0.2, inherit.aes = FALSE, family = "Playfair Display", fontface = "bold") +
  geom_text(data = export_line_label, aes(x = year, y = value, label = label, angle = angle), vjust = 1.1, inherit.aes = FALSE, family = "Playfair Display", fontface = "bold") +
  geom_text(data = import_label, aes(x = year, y = value, label = label, angle = angle), vjust = 1.1, inherit.aes = FALSE, family = "Playfair Display", fontface = "bold") +
  geom_text(data = export_label, aes(x = year, y = value, label = label, angle = angle), vjust = -0.2, inherit.aes = FALSE, family = "Playfair Display", fontface = "bold") +
  geom_richtext(data = balance_in_favour, aes(label = label), color = "black", fill = NA, label.color = NA, label.padding = grid::unit(rep(0, 4), "pt"), size = 6) +
  geom_text(data = balance_against, aes(x = year, y = value, label = label, angle = angle), vjust = -1.3, inherit.aes = FALSE, family = "Playfair Display SC", fontface = "bold", size = 6) +
  labs(x = NULL,
       y = NULL,
       title = "<span style='font-family: \"Playfair Display\"'>Exports and Imports to and from</span> <span style='font-family: \"Playfair Display SC\";'><b>Denmark & Norway</b></span> <span style='font-family: \"Playfair Display\"'>from 1700 to 1780.</span>",
       caption = "<span style='font-family: \"Playfair Display Italic\"'>The Bottom line is divided into Years, the Right hand line into L10,000 each.</span><br><br>") +
  scale_color_manual(values = c("imports" = "#E4B957", "exports" = "#B75266")) +
  scale_x_continuous(breaks = seq(1700, 1790, 10), expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0, 190000, 10000), labels = labels, limits = c(0, 190000), position = "right") +
  theme_jk(grid = "XY",
           markdown = TRUE,
           base_family = "Playfair Display SC",
           plot_title_family = "Playfair Display",
           caption_family = "Playfair Display",
           caption_size = 12,
           plot_title_size = 18) %+replace% 
  theme(panel.grid.major = element_line(color = "black")) +
  theme(plot.title = element_markdown(hjust = 0.5),
        axis.text = element_markdown(size = 12),
        plot.caption = element_markdown(hjust = 0.5),
        panel.border = element_rect(fill = NA, size = 1)) +
  coord_fixed(ratio = 0.00027, clip = "off")

left_label <- richtext_grob("<span style='font-family: \"Playfair Display Italic\"; font-size: 12px;'>Published as the Act directs, 12th December 2019, by @jakekaupp</span>")
right_label <- richtext_grob("<span style='font-family: \"Playfair Display Italic\"; font-size: 12px;'>Neele sculpt 352, Strand, London.</span>")

plot <- ggdraw(playfair) +
  draw_grob(left_label, x = 0.15, y = -0.02, width = 0.1, height = 0.1, hjust = 0) +
  draw_grob(right_label, x = 0.76, y = -0.02, width = 0.1, height = 0.1, hjust = 0)

ggsave(here("2019", "week50", "tw50_plot.png"), plot = plot, width = 12, height = 8)
  

               