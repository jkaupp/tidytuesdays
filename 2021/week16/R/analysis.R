library(tidyverse)
library(ggforce)
library(scico)
library(ggtext)
library(magick)
library(here)

post_offices <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-13/post_offices.csv')

negatives <- filter(post_offices, duration < 0) %>% 
  rename(discontinued = established,
         established = discontinued) 

no_end <- post_offices %>% 
  filter(is.na(duration)) %>% 
  mutate(duration = 2021 - established)


build_years <- post_offices %>% 
  filter(duration < 1000) %>% 
  filter(duration >=0, !is.na(duration)) %>% 
  bind_rows(negatives, no_end) %>% 
  filter(established >= 1000) %>% 
  replace_na(list(discontinued = 2021)) %>%
  filter(!is.na(established)) %>% 
  arrange(id) %>% 
  group_by(id) %>%  
  mutate(year = map2(established, discontinued, ~seq(.x, .y, 1)))

year_counts <- build_years %>% 
  unnest(year) %>%
  ungroup() %>% 
  count(year)
  
plot <- ggplot(year_counts) + 
  geom_circle(aes(x0 = 0, y0 = 0, r = seq_along(year) + 400, color = n), show.legend = FALSE) +
  annotate("text", x =  0, y = 100, label = "US POST OFFICES", family = "Anton", size = 20, color = "white", vjust = 1) +
  annotate("text", x =  0, y = -20, label = "1639 to 2021", family = "Quicksand", size = 6, color = "white", vjust = 1) +
  annotate("text", x =  0, y = -100, label = str_wrap("This graphic is read similar to tree rings.  Each ring is a year and the darker the ring, the more post offices were in service.  From the beginning in 1639, to the peak in 1901 of over 80,000 there exist only 29,055 post offices today.", 50), family = "Poppins", size = 4, color = "white", vjust = 1) +
  scale_color_scico(palette = "grayC") +
  labs(x = NULL,
       y = NULL,
    caption = "**Data**: Blevins, Cameron; Helbock, Richard W., 2021, 'US Post Offices' doi.org/10.7910/DVN/NUKCNA, Harvard Dataverse, V1, UNF:6:8ROmiI5/4qA8jHrt62PpyA== [fileUNF] | **Graphic**: @jakekaupp") + 
  coord_equal() +
  theme_jk(grid = FALSE,
           dark = TRUE,
           caption_family = "Poppins") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.caption = element_markdown(hjust = 0.5))

ggsave(here("2021", "week16", "tw16_plot.png"), plot, width = 12, height = 12)


here("2021", "week16", "tw16_plot.png") %>% 
  image_read() %>% 
  image_trim() %>% 
  image_write(here("2021", "week16", "tw16_plot.png"))


