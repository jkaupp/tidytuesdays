library(tidyverse)
library(friends)
library(jkmisc)
library(sentimentr)
library(magrittr)
library(here)
library(ggbeeswarm)
library(colorspace)
library(glue)
library(ggforce)


friends_sentences <- friends %>% 
  filter(speaker != "Scene Directions", !is.na(speaker)) %>% 
  mutate(splits = get_sentences(text)) 

if (file.exists(here("2020", "week37", "data", "friends_sents.RDS"))) {
  
  friends_sents <- readRDS(here("2020", "week37", "data", "friends_sents.RDS"))
} else {
  
  friends_sents <- friends_sentences %>% 
    split(.$season) %>% 
    map_dfr(~ .x %$% sentiment_by(splits, by = list(season, episode, scene, speaker)))
  
  saveRDS(friends_sents, here("2020", "week37", "data", "friends_sents.RDS"))
  
}


friends_pal <- c("#FF4238", "#FFDC00", "#42A2D6", "#9A0006", "#FFF580", "#00009E")

plot_data <- friends_sents %>% 
  filter(speaker %in% c("Chandler Bing", "Joey Tribbiani", "Monica Geller", "Ross Geller", "Phoebe Buffay", "Rachel Green"))

overall_sentiment <- plot_data %>% 
  summarize(ave_sentiment = mean(ave_sentiment)) %>% 
  pull()

character_labels <- plot_data %>% 
  group_by(speaker) %>% 
  summarize(start = -1.4,
            end = min(ave_sentiment),
            avg = mean(ave_sentiment))
 
min <- plot_data %>% 
  group_by(speaker) %>% 
  filter(ave_sentiment == min(ave_sentiment)) %>% 
  left_join(friends_info)

max <- plot_data %>% 
  group_by(speaker) %>% 
  filter(ave_sentiment == max(ave_sentiment)) %>% 
  left_join(friends_info)


plot <- ggplot(plot_data, aes(x = ave_sentiment, y = speaker, color = speaker, fill = speaker)) +
  geom_vline(aes(xintercept = 0), color = "#E5E9F0", size = 0.2, alpha = 0.5) +
  geom_vline(aes(xintercept = overall_sentiment), color = "#E5E9F0", size = 0.2) +
  geom_segment(data = character_labels, aes(x = start, xend = end, y = speaker, yend = speaker)) +
  geom_text(data = character_labels, aes(label = speaker, x = start), color = "#E5E9F0", alpha = 0.5, vjust = -0.2, hjust = 0, family = "Bebas Neue", fontface = "bold", size = 10) +
  geom_quasirandom(aes(color = speaker), groupOnX = FALSE, size = 1.5, shape = 21) +
  geom_point(data = character_labels, aes(x = avg), shape = 21, size = 5, color = "#2E3440", stroke = 1) +
  geom_mark_circle(data = min, aes(x = ave_sentiment, y = speaker, label = glue("{title} Scene: {scene}"), group = speaker), inherit.aes = FALSE, label.family = "Poppins", color =  "#E5E9F080",  label.fill = NA, label.colour = "#E5E9F080", label.fontface = "plain", con.colour = "#E5E9F080", con.type = "elbow", expand = 0.004) +
  geom_mark_circle(data = max, aes(x = ave_sentiment, y = speaker, label = glue("{title} Scene: {scene}"), group = speaker), inherit.aes = FALSE,  label.family = "Poppins", color =  "#E5E9F080", label.fill = NA, label.colour = "#E5E9F080", label.fontface = "plain", con.colour = "#E5E9F080", con.type = "elbow", expand = 0.004) +
  annotate("text", x = 0.15, y = 7, label = glue("Overall Average Sentence Sentiment: {round(overall_sentiment, 3)}"), family = "Poppins", color = "#E5E9F080", hjust = 0) +
  annotate("curve", x = 0.2, xend = 0.1, y = 6.9, yend = 6.5, color = "#E5E9F080", arrow = arrow(length = unit(0.07, "inch"), type = "closed"), curvature = -0.5) +
  labs(x = NULL,
       y = NULL,
       title = "Friends Character Dialogue Sentiment by Episode and Scene",
       subtitle = "Illustrated below is the sentence level sentiment, aggregated by episode and scene, for the main characters in Friends with each dot representing a scene in an episode. Sentiment scoring and aggregation<br>produced using {sentimentr} for incorporation of valence shifting in natural language processing.",
       caption = "**Data**:{friends} by Emil Hvitfeldt | **Graphic**: @jakekaupp") +
  scale_x_continuous(limits = c(-1.41, 1.2), labels = c("","", "More Negative", "Netural", "More Positive", "" , "")) +
  expand_limits(y = c(0, 7.5)) +
  scale_color_manual(values = darken(friends_pal)) +
  scale_fill_manual(values = friends_pal) +
  theme_jk(grid = FALSE,
           dark = TRUE,
           markdown = TRUE,
           base_family = "Poppins",
           base_size = 16,
           plot_title_family = "Bebas Neue",
           plot_title_size = 30,
           subtitle_family = "Poppins",
           caption_family = "Poppins") +
  theme(legend.position = "none",
        axis.text.y = element_blank())

ggsave(here("2020", "week37", "tw37_plot.png"), plot = plot, width = 18, height = 12, dev = ragg::agg_png())
