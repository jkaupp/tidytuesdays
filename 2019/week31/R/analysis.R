library(tidyverse)
library(here)
library(ggbeeswarm)
library(jkmisc)

video_games <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-30/video_games.csv")  
 
all_games <- video_games %>% 
  filter(!is.na(game), !is.na(metascore))  %>% 
  mutate(developer = tolower(developer),
         idx = row_number())

plot_data <- tibble(facet = c("bioware", "valve", "ubisoft", "rockstar", "square enix"),
       data = list(all_games)) %>% 
  mutate(filtered = map2(data, facet, ~mutate(.x, option = case_when(str_detect(tolower(developer), .y) ~ "selected", 
                                                                     TRUE ~ "other")))) %>% 
  unnest(filtered) %>% 
  mutate(facet = case_when(facet == "bioware" ~ "BioWare",
                           facet == "valve" ~ "Valve",
                           facet == "ubisoft" ~ "Ubisoft",
                           facet == "rockstar" ~ "Rockstar",
                           facet == "square enix" ~ "Square Enix")) %>% 
  mutate(option = factor(option, c("other", "selected"))) %>% 
  arrange(facet, option)



mean <- all_games %>% 
  summarize(metascore = mean(metascore, na.rm = TRUE)) %>% 
  pull(metascore)

min_labels <- plot_data %>% 
  filter(option == "selected", !is.na(metascore)) %>% 
  group_by(facet) %>% 
  filter(metascore == min(metascore))

max_labels <- plot_data %>% 
  filter(option == "selected", !is.na(metascore)) %>% 
  group_by(facet) %>% 
  filter(metascore == max(metascore)) %>% 
  mutate(game = if_else(str_detect("FINAL FANTASY", game), "Final Fantasy IX", game)) %>% 
  slice(1)

plot <- ggplot(plot_data) +
  geom_quasirandom(aes(y = metascore, x = 0, alpha = option, fill = option, size = option), shape = 21, method = "tukey", show.legend = FALSE) +
  geom_label(data = min_labels, aes(x = 0, y = metascore, label = game), family = "Oswald", nudge_y = -2, fill = "#E5E4E2", label.r = unit(0, "lines"), alpha = 0.5) +
  geom_label(data = max_labels, aes(x = 0, y = metascore, label = game), family = "Oswald", nudge_y = +2, fill = "#E5E4E2", label.r = unit(0, "lines"), alpha = 0.5) +
  geom_hline(yintercept = mean, color = "firebrick", size = 0.5, linetype = "dashed") +
  labs(x = NULL, 
       y = NULL,
       title = "How Do The Big Developers Score Against The Competition on Steam?",
       subtitle = str_wrap("Presented below is a jittered strip plot of metascore by developer.  Titles worked on by that developer are highlighted in yellow, the average metascore (72) is shown as a dashed red line. Annotations show the top an bottom rated titles for each developer.  Sqaure and Ubisoft have the most titles with less than average reviews amongst the large developers.", 180),
       caption = "Data: SteamSpy | Graphic: @jakekaupp") +
  scale_size_manual(values = c("other" = 2, "selected" = 2)) +
  scale_y_continuous(limits = c(20, 100), breaks = seq(20, 100, 20)) +
  scale_fill_manual(values = c("other" = "#E5E4E2", "selected" = "#ffd644")) +
  scale_alpha_manual(values = c("other" = 0.05, "selected" = 1)) +
  theme_jk(grid = "Y", dark = TRUE) +
  facet_wrap(~facet, nrow = 1) +
  theme(strip.text = element_text(color = "white"),
      axis.text.x = element_blank())

ggsave(here("2019", "week31", "tw31_plot.png"), plot, width = 14, height = 8)
