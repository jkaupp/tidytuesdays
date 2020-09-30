library(tidyverse)
library(readxl)
library(ggbump)
library(ggforce)
library(jkmisc)
library(appa)
library(here)
library(glue)

atla_stream <- read_excel(here("2020", "week32", "data", "avatar streams.xlsx")) %>% 
  pivot_longer(-character, names_to = "episode", values_to = "y_pos", names_transform = list(episode = as.integer)) 

character_data <- appa %>% 
  filter(character != "Scene Description") %>% 
  group_by(book, book_num, chapter, chapter_num) %>% 
  distinct(character) %>% 
  filter(character %in% c("Aang", "Katara", "Sokka", "Toph", "Zuko", "Iroh", "Suki", "Zhao", "Bumi", "Ozai", "Hakoda", "Bato", "Jeong Jeong", "Pakku", "Azula", "Mai", "Ty Lee", "Piandao", "King Bumi")) %>% 
  mutate(character = case_when(character == "King Bumi" ~ "Bumi",
                               TRUE ~ character),
         in_episode = 1) %>% 
  group_by(chapter) %>% 
  mutate(episode = cur_group_id())

colors <- c("Mai" = "#591B29",
               "Toph" = "#3A7714",
               "Aang" = "#E2AC4C",
               "Katara" = "#6876A6",
               "Pakku" = "#182B65",
               "Suki" = "#17494A",
               "Sokka" = "#62698F",
               "Zhao" = "#7F1902",
               "Bumi" = "#414D1F",
               "Ozai" = "#9E0003",
               "Hakoda" = "#0A1F38",
               "Bato" = "#080B16",
               "Iroh" = "#7C7A7B",
               "Zuko" = "#D4A860",
               "Azula" = "#BB1504",
               "Ty Lee" = "#B26B61",
               "Jeong Jeong" = "#6F4F2D",
               "Piandao" = "#571D00")

points <- semi_join(atla_stream, character_data)

book_labels <- filter(character_data, episode %in% c(1, 21, 41)) %>% 
  distinct(book, book_num, episode) %>% 
  mutate(episode = episode - if_else(episode == 1, 0, 0.5))

plot <- ggplot(atla_stream, aes(x = episode, y = y_pos)) +
  #geom_text(data = distinct(atla_stream, y_pos), aes(x = 0, y = y_pos, label = y_pos)) +
  #geom_text(data = distinct(atla_stream, episode), aes(x = episode, y = 0, label = episode)) +
  geom_vline(data = book_labels, aes(xintercept = episode), size = 0.1, color = "grey60") +
  geom_text(data = book_labels, aes(x = episode, y = 35, label = glue("Book {book_num}: {book}")), hjust = -0.05, family = "Herculanum", size = 8) +
  geom_bump(smooth = 5, size = 1.5, show.legend = FALSE, aes(color = character)) +
  geom_text(data = filter(atla_stream, episode == -1), aes(x = -1.5, y = y_pos, label = character), hjust = 1, show.legend = FALSE, family = "Herculanum") +
  geom_text(data = filter(atla_stream, episode == 61), aes(x = 63.5, y = y_pos, label = character), hjust = 0, show.legend = FALSE, family = "Herculanum") +
  geom_point(data = points, aes(fill = character), color = '#F6F5F3', shape = 21, stroke = 1, size = 2, show.legend = FALSE) +
  geom_mark_ellipse(data = points, aes(x = episode, filter = episode == 1), fill = "grey60", color = NA, expand = unit(3, "mm")) +
  geom_text(data = points, aes(x = 0.5, y = 17, label = "A Boy in the Iceberg"), family = "Optima") +
  geom_mark_ellipse(data = points, aes(x = episode, filter = episode == 8), fill = "grey70", color = NA, expand = unit(3, "mm")) +
  geom_text(data = points, aes(x = 8, y = 10.5, label = "Avatar Roku"), family = "Optima") +
  geom_mark_ellipse(data = points, aes(x = episode, filter = episode %in% c(19,20) & character != "Ozai"), fill = "grey70", color = NA) +
  geom_text(data = points, aes(x = 19, y = 8, label = "Siege of the North"), family = "Optima") +
  geom_mark_ellipse(data = points, aes(x = episode, filter = episode == 26), fill = "grey70", color = NA, expand = unit(3, "mm")) +
  geom_text(data = points, aes(x = 26, y = 8, label = "The Blind Bandit"), family = "Optima", hjust = 0) +
  geom_mark_ellipse(data = points, aes(x = episode, filter = episode == 30), fill = "grey70", color = NA, expand = unit(3, "mm")) +
  geom_text(data = points, aes(x = 30, y = 9, label = "The Library"), family = "Optima", hjust = 0) +
  geom_mark_ellipse(data = points, aes(x = episode, filter = episode == 34), fill = "grey70", color = NA, expand = unit(3, "mm")) +
  geom_text(data = points, aes(x = 35, y = 22, label = "City Walls and Secrets"), family = "Optima") +
  geom_mark_ellipse(data = points, aes(x = episode, filter = episode == 40), fill = "grey70", color = NA, expand = unit(3, "mm")) +
  geom_text(data = points, aes(x = 40, y = 15.5, label = "Crossroads of Destiny"), family = "Optima", hjust = 0) +
  geom_mark_ellipse(data = points, aes(x = episode, filter = episode %in% 50:51), fill = "grey70", color = NA, expand = unit(3, "mm")) +
  geom_text(data = points, aes(x = 50, y = 13, label = "The Day of Black Sun"), family = "Optima", hjust = 1) +
  geom_mark_ellipse(data = points, aes(x = episode, filter = episode %in% 54:55), fill = "grey70", color = NA, expand = unit(3, "mm")) +
  geom_text(data = points, aes(x = 54.5, y = 3.5, label = "The Boiling Rock"), family = "Optima") +
  geom_mark_ellipse(data = points, aes(x = episode, filter = episode %in% 58:61), fill = "grey70", color = NA, expand = unit(3, "mm")) +
  geom_text(data = points, aes(x = 59.5, y = 23, label = "Sozin's Comet"), family = "Optima") +
  labs(x = NULL,
       y = NULL,
       title = "Avatar: The Last Airbender Character Interactions",
       subtitle = "Illustrated below are the character interactions ATLA. The horizontal axis is time (by episode) and The dots in each line represent a character's appearance in an episode. The vertical grouping represents which characters are together.",
       caption = '**Data**: {appa} | **Graphic**: @jakekaupp') +
  expand_limits(x = c(-5, 65)) +
  scale_y_continuous(trans = "reverse") +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  theme_jk(grid = FALSE,
           markdown = TRUE,
           plot_title_family = "Herculanum",
           plot_title_size = 30,
           subtitle_family = "Optima") +
  theme(plot.background = element_rect(fill = '#F6F5F3', color = NA),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())

ggsave(here('2020', "week33", "tw33_plot.png"), plot = plot, device = ragg::agg_png(), width = 20, height = 10)
