library(tidyverse)
library(tricolore)
library(ggtern)
library(here)
library(jkmisc)
library(magick)

r4ds_members <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-16/r4ds_members.csv")


tern_plot <- Tricolore(r4ds_members, "percent_of_messages_public_channels",
          "percent_of_messages_private_channels",
          "percent_of_messages_d_ms", breaks = 5, show_data = FALSE)

legend <- tern_plot$key +
  labs(title = "Color Legend",
       x       = "Public\nChannels",
       y       = "Private\nChannels",
       z       = "Direct\nMessages") +
  theme_hidetitles() +
  theme_hidelabels() +
  theme_hideticks() +
  theme(plot.title = element_text(hjust = 0.5, family = "Scope One", size = 40),
        axis.text = element_text(family = "Scope One"),
        axis.title = element_text(family = "Scope One"))

png(here("2019", "week29", "legend.png")) 
legend
dev.off()

legend <- image_read(here("2019", "week29", "legend.png"))

plot <- r4ds_members %>% 
  mutate(color = tern_plot$rgb,
         year = lubridate::year(date)) %>% 
  ggtern(aes(x = percent_of_messages_public_channels, y = percent_of_messages_private_channels, z = percent_of_messages_d_ms, color = color)) +
  geom_point(size = 3) +
  scale_color_identity() +
  labs(title = str_to_title("The Dialogue in the R4DS Slack indicates an Open and Inclusive Learning Community"),
       subtitle = str_wrap("Below is a ternary digram presenting the message composition in public channels, private channels and direct messages as a percentage.  Each day is represented by a point with the composition represented by position relative to each axes.  Composition is additionally encoded by color as illustrated on the inset legend.", 100),
       x       = "Public\nChannels",
       xarrow  = "More Public Channel Messages",
       y       = "Private\nChannels",
       yarrow  = "More Private Channel Messages",
       z       = "Direct\nMessages",
       zarrow  = "More Direct Messages",
       caption = "Data: R4DS Community | Graphic: @jakekaupp") +
  theme(panel.background = element_rect(fill = "#2E3440"),
        panel.grid = element_line(color = "#ffffff", size = 0.1),
        panel.grid.minor = element_blank(),
        text = element_text(family = "Oswald"),
        plot.subtitle = element_text(family = "Scope One"),
        axis.text = element_text(family = "Scope One"),
        axis.title = element_text(family = "Scope One")) +
  theme_showarrows() +
  theme_arrowlong() 


png(here("2019", "week29", "tw29_plot.png"), width = 10, height = 8, units = "in", res = 200)
grid::grid.newpage()
plot
grid::grid.raster(legend, width = 0.18, height = 0.2, x = 0.75, y = 0.7)
dev.off()
