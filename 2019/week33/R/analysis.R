library(tidyverse)
library(jkmisc)
library(lubridate)
library(here)
library(patchwork)


emperors <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-13/emperors.csv") 

ad_births <- c("Augustus", "Tiberius", "Claudius", "Galba")

emp_numeric_years <- emperors %>% 
  mutate_if(is.Date, list(year = year)) %>% 
  mutate(birth_year = if_else(name %in% ad_births, -birth_year, birth_year),
         reign_start_year = if_else(name == "Augustus", -reign_start_year, reign_start_year))


missing_birth_estimates <- emp_numeric_years %>% 
  filter(is.na(birth_year)) %>% 
  mutate(birth_year = case_when(name == "Florian" ~ 202,
                                name == "Numerian" ~ 248,
                                name == "Carinus" ~ 245,
                                name == "Severus II" ~ 260,
                                name == "Vetranio" ~ 325))


plot_data <- emp_numeric_years %>% 
  filter(!is.na(birth_year)) %>% 
  bind_rows(missing_birth_estimates)


dynasties <- plot_data %>% 
  group_by(dynasty) %>% 
  summarize(reign_start_year = min(reign_start_year),
               reign_end_year = max(reign_end_year))

roman_palette <- set_names(colorRampPalette(c("#191970", "#FF7F50"))(8), unique(plot_data$dynasty))


overall <- ggplot(plot_data, aes(y = 0)) +
  geom_segment(aes(x = reign_start_year, xend = reign_end_year, yend = 0, color = dynasty), size = 4) +
  scale_color_manual("Dynasty", values = roman_palette, breaks = names(roman_palette)) +
  scale_x_continuous(breaks = c(-62, 0, 100, 200, 300, 400), labels = c("62 BC", "1 AD", "100 AD", "200 AD", "300 AD", "400 AD")) +
  expand_limits(x = c(-62, 450)) +
  labs(x = NULL, y = NULL,
       caption = "Data: Wikipedia via @geokaramanis | Graphic: @jakekaupp") +
  theme_jk(grid = "X") +
  theme(axis.text.y = element_blank(),
        legend.position = "none")

bars <- ggplot(plot_data, aes(y = reorder(name, reign_start_year))) +
  geom_segment(aes(x = birth_year, xend = death_year, yend = name), size = 2, color = "grey90") +
  geom_segment(aes(x = reign_start_year, xend = reign_end_year, yend = name, color = dynasty), size = 2) +
  geom_segment(data = filter(plot_data, reign_start_year == reign_end_year), aes(x = reign_start_year - 0.5, xend = reign_start_year + 0.5, y = name, yend = name, color = dynasty), size = 2) +
  geom_text(aes(x = death_year, label = name), hjust = 0, family = "Scope One", size = 2, nudge_x = 3) +
  scale_color_manual("Dynasty", values = roman_palette, breaks = names(roman_palette)) +
  scale_x_continuous(breaks = c(-62, 0, 100, 200, 300, 400), labels = c("62 BC", "1 AD", "100 AD", "200 AD", "300 AD", "400 AD")) +
  expand_limits(x = c(-62, 450)) +
  labs(x = NULL, 
       y = NULL,
       title = str_to_title("When in Rome: The Game of Imperial Thrones. You Win or You Die."),
       subtitle = str_wrap("Illustrated below is a timeline of the life and reigns of Roman Emperors from 62 BC to 395 AD.  The light grey bar depicts the liftime of the emperor, the colored bar (by dynasty) indicates the duration of their reign. An overall timeline by dynasty is shown near the horizontal axis. Unsurprisingly, the majority of emperors reign ending also coincides with the end of their life.", 100)) +
  theme_jk(grid = "X") +
  theme(axis.text = element_blank(),
        legend.position = c(0.2, 0.7),
        legend.background = element_rect(fill = "white", size = 0))


out <- wrap_plots(bars, overall, ncol = 1, heights = c(0.9, 0.1))

ggsave(here("2019", "week33", "tw33_plot.png"), out, width = 7.5, height = 8)
