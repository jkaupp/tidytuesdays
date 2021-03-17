library(tidyverse)
library(ggridges)
library(janitor)
library(jkmisc)
library(colorspace)
library(ggfx)
library(ggtext)
library(here)
library(ragg)

games <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-16/games.csv')

game_data <- games %>% 
  mutate(pct_avg_peak = parse_number(avg_peak_perc)) %>% 
  filter(!is.na(pct_avg_peak)) %>% 
  complete(gamename, year, month, fill = list(avg = 0,
                                              gain = 0,
                                              peak = 0,
                                              avg_peak_perc = 0,
                                              pct_avg_peak = 0)) %>% 
  mutate(month_num = map_dbl(month, ~which(month.name == .x))) %>% 
  group_by(gamename) %>% 
  arrange(gamename, year, month_num) %>% 
  mutate(idx = row_number(),
         gamename = iconv(gamename, "latin1", 'ASCII', sub = "")) 

top_games <- game_data %>% 
  group_by(gamename) %>% 
  slice_max(avg) %>% 
  arrange(-avg) %>% 
  ungroup() %>% 
  slice(1:3) %>% 
  pull(gamename)


plot_data <- game_data %>% 
  filter(gamename %in% top_games) %>% 
  mutate(gamename = factor(gamename, rev(top_games))) 

labels <- plot_data %>% 
  group_by(gamename) %>% 
  summarize(idx = 1,
            avg = 0) 

year_labels <- plot_data %>% 
  group_by(year) %>% 
  summarize(idx = min(idx))


plot <- ggplot() +
  as_reference(geom_text(data = labels, aes(x = idx, y = gamename, label = gamename), vjust = -0.1, hjust = 0, family = "Anton", size = 14, color = "#37323e"), 
               id = "text") +
  with_blend(geom_ridgeline(data =  plot_data, aes(y = gamename, x = idx, height = avg/1000000, group = gamename, color = gamename, fill = gamename), size = 0.1, alpha = 1, show.legend = FALSE, scale = 1.5),
             bg_layer = "text", 
             blend_type = 'xor')  + 
  annotate("text", x = 1, y = 5, label = "A Three Player Battle Royale", family = "Anton", size = 16, hjust = 0, color = "#37323e") +
  annotate("text", x = 1, y = 4.7, label = str_wrap("Taking the top spot in average players on Steam early in 2012, Dota 2 and CS:GO have been fighting for top draw in multiplayer online gaming. Then in 2017 a new contender, PUBG, entered the ring taking the crown for years until waning interest and a surge of more battle royale games entered the market.", 65), family = "Raleway", size = 6, hjust = 0, vjust = 1, color = "#37323e") +
   labs(x = NULL, 
       y = NULL,
       caption = "**Data**: Steam | **Graphic**: @jakekaupp") +
  scale_x_continuous(breaks = year_labels$idx, labels = year_labels$year, limits = c(0, 117)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_fill_manual(values = c("#bfbdc1","#6d6a75","#37323e")) +
  scale_color_manual(values = darken(c("#bfbdc1","#6d6a75","#37323e"))) +
  theme_jk(markdown = TRUE,
           grid = FALSE,
           base_family = "Bebas Neue",
           ticks = TRUE,
           base_size = 20) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.caption = element_markdown(hjust = 0.95))

ggsave(here('2021', 'week12', 'tw12_plot.png'), plot, width = 16, height = 9, device = ragg::agg_png())
