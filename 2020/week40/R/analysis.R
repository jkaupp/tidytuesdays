library(tidyverse)
library(here)
library(jkmisc)
library(janitor)
library(glue)
library(fs)
library(magick)
library(rvest)
library(ggbump)
library(nord)

# Tidytuesday data ----
beyonce_lyrics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/beyonce_lyrics.csv')
taylor_swift_lyrics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/taylor_swift_lyrics.csv')
sales <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/sales.csv')
charts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/charts.csv')

# Billboard top 100 data ----
top100 <- here("2020", "week40", "data", "Hot Stuff.csv") %>% 
  read_csv() %>% 
  clean_names()

# 2020 Billboard top 100 data scraping ----
if (!file.exists(here("2020", "week40", "data", "top100_2020.RDS"))) {
  

  url <- "https://www.billboard.com/charts/hot-100/"
  
  dates <- seq(from=as.Date("2020-01-01"), to=as.Date("2020-9-30"), by = "day")
  
  month <- months(dates[chron::is.weekend(dates)])
  saturdays <- na.omit(dates[chron::is.weekend(dates)][rep(c(TRUE, FALSE), 78)])
  
  list_bb_2020 <- tibble(url = glue("{rep(url, 39)}{saturdays}"),
                         weekid = strftime(saturdays, format = "%m/%d/%Y"))
  
  content <- list_bb_2020 %>% 
    mutate(html = map(url, read_html))
  
  top100_2020 <- content %>% 
    mutate(performer = map(html, ~ html_nodes(.x, "li.chart-list__element > button:nth-child(1) > span:nth-child(2) > span:nth-child(2)") %>% 
                             html_text()),
           song = map(html, ~html_nodes(.x, "li.chart-list__element > button:nth-child(1) > span:nth-child(2) > span:nth-child(1)") %>% 
                        html_text()),
           week_position = map(html, ~html_nodes(.x, "li.chart-list__element > button:nth-child(1) > span:nth-child(1) > span:nth-child(1)") %>% 
                                 html_text()),
           weeks_on_chart = map(html, ~ html_nodes(.x, "li.chart-list__element > button:nth-child(1)") %>% 
                                  html_text() %>% 
                                  map(~str_extract(.x, "(\\d+)(?= Weeks on Chart)")))) %>% 
    select(weekid, performer, song, week_position, weeks_on_chart) %>% 
    unnest(cols = c(performer, song, week_position, weeks_on_chart)) %>% 
    mutate(song_id = paste0(song, performer),
           across(c(week_position, weeks_on_chart), as.numeric))
  
  saveRDS(top100_2020, here("2020", "week40", "data", "top100_2020.RDS")) 
  
  } else {
    
    top100_2020 <- readRDS( here("2020", "week40", "data", "top100_2020.RDS"))
    
  }

# Data Manipulation and Joining ----

peak_position_2020 <- top100_2020 %>% 
  group_by(song_id) %>% 
  summarize(peak_position = min(week_position))

full_top_100 <- top100_2020 %>% 
  left_join(peak_position_2020) %>% 
  bind_rows(top100)

top100_end <- full_top_100 %>%  
  group_by(song_id) %>% 
  filter(weeks_on_chart == max(weeks_on_chart))

#Get Albums/Songs
taylor_data <- taylor_swift_lyrics %>% 
  select(Album, song = Title) %>%
  clean_names() %>% 
  mutate(song = if_else(album == "folklore", str_to_title(song), song)) %>% 
  mutate(across(c(album), str_to_title)) %>% 
  left_join(top100_end) %>% 
  filter(performer == "Taylor Swift" | is.na(performer)) %>% 
  replace_na(list(peak_position = 100))

album <- taylor_data %>% 
  distinct(album, song) %>%
  group_by(album) %>% 
  mutate(track = row_number()) %>% 
  mutate(song = str_trim(song))

spacer <- tibble(album = unique(album$album)) %>% 
  mutate(track = list(1:30)) %>% 
  unnest(cols = c(track)) %>% 
  anti_join(album) %>% 
  mutate(song = NA_character_)

album_spaced <- bind_rows(album, spacer) %>% 
  arrange(album, track) %>% 
  ungroup() %>% 
  mutate(y_pos = row_number()) %>% 
  filter(!is.na(song))

weeks <- taylor_data %>% 
  arrange(peak_position, desc(weeks_on_chart)) %>% 
  select(album, song, peak_position, weeks_on_chart) %>% 
  complete(peak_position = 1:100) %>% 
  mutate(y_pos = row_number()) %>% 
  filter(!is.na(album))

strands <- left_join(weeks, album_spaced, c("album", "song"))
  
# Album covers ----

album_order <- unique(album$album)

pal <- set_names(c("#16A7A1", "#075171", "#C9D3E7", "#844D6C", "#C9B5C8", "#F8C585", "#626466", "#F4D451"), album_order)

covers <- here("2020", "week40", "data") %>% 
  dir_ls(glob = "*.png") %>% 
  enframe(name = "album") %>% 
  mutate(album = str_replace(str_remove(tools::file_path_sans_ext(basename(album)), "Taylor_Swift_-_"), "_", " ")) %>% 
  mutate(album = factor(album, album_order, album_order)) %>% 
  arrange(album) %>% 
  mutate(artwork = map(value, image_read),
         w_border = map2(artwork, pal, ~image_border(.x, .y)),
         raster = map(w_border, as.raster)) 
  


cover_annotations <- album_spaced %>% 
  group_by(album) %>% 
  summarize(ymin = -min(y_pos) + 1,
            ymax = -max(y_pos) - 1) %>% 
  right_join(covers, by = "album") %>% 
  mutate(xmin = -100,
         xmax = -85) %>% 
  select(raster, xmin, xmax, ymin, ymax) %>% 
  pmap(annotation_raster)



plot <- ggplot() +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 170), size = 0.1, color = "#8E8F92") +
  geom_segment(aes(x = -40, xend = -40, y = 0, yend = 230), size = 0.1, color = "#8E8F92") +
  geom_segment(aes(x = 0, xend = 50, y = 120, yend = 120), size = 0.1, color = "#8E8F92") +
  geom_segment(aes(x = seq(0, 50, 10), xend = seq(0, 50, 10), y = 0, yend = 120), size = 0.1, color = "#8E8F92") +
  geom_text(aes(x = seq(0, 50, 10), y = 119, label = seq(0, 50, 10)),  color = "#8E8F92", family = "Silka Mono", size = 2, hjust = 1.1) +
  geom_text(aes(x = 50, y = 122, label = "Weeks on the Billboard Top 100"),  family = "Silka Mono", size = 3, hjust = 1) +
  geom_text(aes(x = 52, y = 1, label = "#1"),  family = "Silka Mono", size = 3, hjust = 0) +
  geom_text(aes(x = 52, y = 119, label = "#100"),  family = "Silka Mono", size = 3, hjust = 0) +
  geom_text(aes(x = 50, y = 138, label = "Never Appeared on the Billboard Top 100"),  family = "Silka Mono", size = 3, hjust = 1) +
  geom_segment(aes(x = 2, xend = 7, y = 138, yend = 138), size = 0.1, color = "#8E8F92", arrow = arrow(length = unit(1, "mm"), type = "closed", ends = "first")) +
  geom_text(data = album_spaced, aes(x = -41, y = y_pos, label = song), hjust = 1, vjust = 0.5, family = "Silka Mono", size = 3, position = position_dodge(width = 0.1)) +
  geom_point(data = album_spaced, aes(x = -40, y = y_pos, color = album), size = 1) +
  geom_col(data = weeks, aes(x = weeks_on_chart, y = y_pos, fill = album), orientation = "y", color = "#FAF4EE") +
  geom_point(data = weeks, aes(x = 0, y = y_pos, color = album), size = 1)  +
  geom_sigmoid(data = strands, aes(x = -40, xend = 0, y = y_pos.y, yend = y_pos.x, color = album, group = song)) +
  labs(x = NULL,
       y = NULL,
       title = "Taylor Swift's Evolution from Country Darling to Pop Star",
       subtitle = "Shown below is a combined alluvial diagram and bar chart comapring and ranking the eight studio albums for Taylor Swift<br>by chart position and duration.  This illustrates her acceptance into mainstream pop that began with her self-titled debut.",
       caption = "**Data**: @Rosie\\_Baillie\\_ & @sastoudt | **Graphic**: @jakekaupp") +
  scale_y_continuous(limits = c(230, 0), trans = "reverse") +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
  expand_limits(x = c(-100, 50)) +
  theme_jk(grid = FALSE,
           markdown = TRUE,
           plot_title_family = "Poppins",
           subtitle_family = "Silka Mono",
           caption_family = "Poppins") +
  cover_annotations +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = 'none',
        plot.background = element_rect(fill = "#FAF4EE", color = "#FAF4EE"))

ggsave(here("2020", "week40", "tw40_plot.png"), plot, width = 14, height = 20, dev = ragg::agg_png())
 