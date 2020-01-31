library(tidyverse)
library(jkmisc)
library(glue)
library(ggtext)
library(here)

spotify_songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')

rush <- spotify_songs %>% 
  filter(!is.na(track_name), track_artist %in% c("Rush", "The Who", "Led Zeppelin"), track_name %in% c("Tom Sawyer", "Baba O'Riley", "Immigrant Song - Remaster")) %>% 
  distinct(track_name, .keep_all = TRUE) %>% 
  select(playlist_genre, track_name, track_artist, danceability, energy, speechiness, acousticness, instrumentalness, liveness, valence, tempo) %>% 
  mutate(color = c("#f7b801","#f18701","#f35b04")) %>% 
  mutate_at(vars(contains("track")), as.character) %>% 
  mutate_at(vars(danceability:tempo), function(x) x/max(x)) %>% 
  pivot_longer(danceability:tempo) 


bg <- spotify_songs %>% 
          distinct(track_id, .keep_all = TRUE) %>% 
          distinct(track_name, track_artist, .keep_all = TRUE) %>% 
          filter(!is.na(track_name)) %>% 
          select(track_name, track_artist, danceability, energy, speechiness, acousticness, instrumentalness, liveness, valence, tempo) %>% 
          mutate(color = "#d8dee9") %>% 
  mutate_at(vars(danceability:tempo), function(x) x/max(x)) %>% 
  pivot_longer(danceability:tempo) 


plot <- ggplot(bg, aes(x = name, y = value)) +
  geom_line(aes(color = color,  group = track_name), alpha = 0.1, size = 0.1) +
  geom_line(data = rush, aes(color = color, group = track_artist), size = 0.5) +
  geom_point(data = rush, aes(color = color, group = track_artist), size = 1) +
  geom_text(data = filter(rush, name == "valence"), aes(label = glue("{str_replace_all(str_to_upper(track_artist), '(?<=.)(?!$)', ' ')}\n{track_name}"), colour = color), family = "Oswald", hjust = 0, nudge_x = 0.1, nudge_y = c(-0.05, 0, 0)) +
  scale_color_identity() +  
  scale_fill_identity() +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("L O W", "A V E R A G E", "H I G H")) +
  scale_x_discrete(labels = function(x) str_replace_all(str_to_upper(x), "(?<=.)(?!$)", " ")) +
  expand_limits(x = c(1, 10)) +
  labs(title = "Characteristics Of My Favourite Arcade Songs",
       subtitle = glue("Ilustrated below is a parallel coordinates plot of Spotify playlist data showing normalized measurements for a variety of audio features<br>accessible via Spotify API.  I was introduced {highlight_text('Rush', '#f18701', 'b')}, {highlight_text('Led Zeppelin', '#f7b801', 'b')} and {highlight_text('The Who', '#f35b04', 'b')} at my local arcade, and look back fondly on those times.<br>These songs still have me look for a quarter and a Tempest machine any time I hear them."),
       x = NULL,
       y = NULL,
       caption = "**Data**: Spotify | **Graphic**: @jakekaupp") +
  theme_jk(grid = "X",
           dark = TRUE,
           markdown = TRUE) +
  theme(legend.position = "none")


ggsave(here("2020", "week4", "tw4_plot.png"), plot, width = 12, height = 6, dev = ragg::agg_png())
