library(tidyverse)
library(magick)
library(imager)
library(scales)
library(here)
library(fs)
library(httr)
library(jkmisc)

artwork <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-12/artwork.csv')
artists <- readr::read_csv("https://github.com/tategallery/collection/raw/master/artist_data.csv")

# Code Source: https://chichacha.netlify.app/2019/01/19/extracting-colours-from-your-images-with-image-quantization/

get_colorPal <- function(url){
  
  im <- image_read(url) # added to read the image inside the function
  
  out <- im %>%
    image_quantize(max = 10, colorspace = "RGB") %>%
    magick2cimg() %>%  ## prep for making data frame
    RGBtoHSV() %>% # to get hue
    as.data.frame(wide = "c") %>%  ## making it wide
    mutate(hex = hsv(rescale(c.1, from = c(0,360)), c.2, c.3),
           hue = c.1) %>%
    count(hex, hue, sort = TRUE) %>%
    mutate(colorspace = 'RGB') %>%
    select(hex, hue, n)
  
  return(out)
  
}

# Get thumbnails

if (length(dir_ls(here("2021", "week3", "data", "thumbs"))) != 7091) {
  artwork %>% 
    filter(!is.na(thumbnailUrl), str_detect(medium, "Watercolour|watercolour")) %>% 
    pull(thumbnailUrl) %>% 
    walk(~GET(.x, write_disk(here("2021", "week3", "data", "thumbs", basename(.x)))))
} 

if (!file.exists(here("2021", "week3", "data", "water_colors.RDS"))) {
  
  water_colors <- tibble(path = as.character(dir_ls(here("2021", "week3", "data", "thumbs")))) %>%
    mutate(file = basename(path)) %>% 
    mutate(colors = map(path, get_colorPal))
  
  saveRDS(water_colors, here("2021", "week3", "data", "water_colors.RDS"))
  
} else {
   
  water_colors <- readRDS(here("2021", "week3", "data", "water_colors.RDS"))
  
}

plot_data <- artwork %>%
  mutate(file = basename(thumbnailUrl)) %>% 
  right_join(water_colors, by = "file") %>% 
  filter(!is.na(year)) %>% 
  select(id, accession_number, artist, title, medium, year, colors) %>% 
  mutate(y_idx = seq_along(id)) %>% 
  unnest(c("colors")) %>% 
  group_by(id) %>% 
  mutate(x_idx = row_number()) %>% 
  ungroup() 

plot <- ggplot(plot_data, aes(x = x_idx, y = y_idx, fill = hex)) +
  geom_tile() +
  annotate("text", x = -2, y = 0, label = "Watercolor Palettes at", family = "Aleo Light", size = 3) +
  annotate("text", x = -3, y = 0, label = "T H E  T A T E", family = "Aleo Bold", size = 5) +
  annotate("text", x = -4, y = 0, label = "7091 Works\n1596 to 2008", family = "Aleo Light", size = 3, vjust = 1) +
  scale_fill_identity() +
  labs(x = NULL,
       y = NULL,
       caption = "**Data**: Tate Collection | **Graphic**: @jakekaupp") +
  xlim(c(-4, 10)) +
  coord_polar(theta = "y") +
  theme_jk(grid = FALSE,
           markdown = TRUE) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())


ggsave(here("2021", "week3", "tw3_plot.png"), plot, width = 7, height = 7, dev = ragg::agg_png()) 
