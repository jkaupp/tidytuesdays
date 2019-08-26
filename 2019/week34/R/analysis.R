library(tidyverse)
library(lubridate)
library(here)
library(ggtext)
library(jkmisc)
library(waffle)
library(ggforce)
library(glue)
library(ragg)

nuclear_explosions <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-20/nuclear_explosions.csv")




plot_data <- nuclear_explosions %>% 
  mutate(date_long = ymd(date_long)) %>% 
  group_by(country, date_long) %>% 
  summarize(n = n(),
            total_yield = sum(yield_upper, na.rm = TRUE)) %>% 
  group_by(country) %>% 
  mutate(c_sum = cumsum(n),
         c_yield = cumsum(total_yield)/1000) %>% 
  filter(country %in% c("USSR", "USA"))

items <- nuclear_explosions %>% 
  mutate(date_long = ymd(date_long)) %>% 
  filter(country %in% c("USSR", "USA")) %>% 
  group_by(country) %>% 
  top_n(1, yield_upper) %>% 
  slice(1) %>%
  semi_join(plot_data, .) %>% 
  mutate(date_long = ymd(date_long) - 1) %>% 
  mutate(name = if_else(country == "USA", "March 1954: Castle Bravo", "October 1961: Tsar Bomba"),
         description  = if_else(country == "USA", "2nd most powerful nuclear test explosion, 3 times over the predicted 5 MT yield.", "Most powerful nuclear test explosion, twice the predicted yield of 25 MT."))

explosions <- ggplot(plot_data, aes(x = c_sum, y = c_yield, color = country)) +
  geom_step(linetype = "solid", size = 1, direction = "hv") +
  geom_point(data = filter(plot_data, date_long %in% range(date_long))) +
  geom_text(data = filter(plot_data, date_long == last(date_long)), aes(label = year(ymd(date_long))), family = "Oswald", hjust = -0.5) +
  geom_text(data = filter(plot_data, date_long == first(date_long)), aes(label = year(ymd(date_long))), family = "Oswald", nudge_y = c(10, -10)) +
  geom_mark_circle(data = items, aes(color = country, label = name, description = description), expand = unit(3, "mm"), label.margin = margin(5, 5, 5, 5, "mm"), con.colour = c("#0052A5", "#FF2400"), label.family = c("Oswald","Scope One"), label.fill = NA, label.minwidth = unit(50, "mm"), label.fontsize = 10, con.type = "straight") +
  scale_color_manual(values = c('USSR' = "#FF2400", "USA" = "#0052A5")) +
  scale_fill_manual(values = c('USSR' = "#FF2400", "USA" = "#0052A5")) +
  scale_y_continuous(labels = function(x) scales::comma(x, suffix = " MT")) +
  labs(x = 'Cumulative Number of Explosions',
       y = "Cumulative Yield (MT)",
       title = "Nuclear Weapons Research Race During And After The Cold War",
       subtitle = "Illustrated below is a step chart showing the number and yield of nuclear explosions for weapons research for <span style='color:#0052A5'>**USA**</span> and <span style='color:#FF2400'>**USSR**</span>.  During this race nearly<br>500 MT of nuclear explosions and accompanying fallout blanketed the world. The effects are still being dealt with to this date.",
       caption = "Data: Our World in Data | Graphic: @jakekaupp") +
  theme_jk(subtitle_family = "PT Serif",
           caption_family = "PT Serif") +
  theme(plot.title = element_markdown(), 
        plot.subtitle = element_markdown(),
        legend.position = "none")
  
ggsave(here("2019", "week34", "tw34_plot_2.png"), plot = explosions, width = 12, height = 6, device = agg_png())


# Waffle ----

waffle_data <- nuclear_explosions %>% 
  mutate(purpose = case_when(grepl("WR", purpose) ~ "WR",
                             grepl("WE", purpose) ~ "WE",
                             grepl("PNE", purpose) ~ "PNE",
                             TRUE ~ purpose)) %>% 
  filter(country %in% c("USSR", "USA")) %>% 
  count(year, purpose) %>% 
  group_by(purpose) %>% 
  mutate(c_sum = cumsum(n))


pal <- set_names(sample(grey.colors(50),10), unique(waffle_data$purpose))

pal["WR"] <- "#8A0303"

waffle <- ggplot(waffle_data, aes(fill = purpose, values = c_sum)) +
  geom_waffle(color = "white", size = .25, n_rows = 10, flip = TRUE, show.legend = FALSE) +
  facet_wrap(~year, strip.position = "bottom", nrow = 1) +
  scale_x_discrete() + 
  scale_y_continuous(labels = function(x) x * 10,
                     expand = c(0,0)) +
  coord_equal() +
  scale_fill_manual(values = pal) +
  labs(y = "Cumulative Number of Explosions",
       title = "Nuclear Weapons Research Testing During the Cold War Was the Primary Driver of Controlled  Nuclear Explosions",
       subtitle = "Illustrated below is a timeline of waffle charts showing the distribution of cumulative explosions from <span style='color:#8A0303'>**weapons research**</span> or <span style='color:#4D4D4D'>**other purposes**</span>. Widescale Nuclear testing ceased in the mid-'90's. The only active country<br>conducting nuclear testing in this era is North Korea, weathering the disapproval and ire of the global community.",
       caption = "Data: Our World in Data | Graphic: @jakekaupp") +
  theme_jk(strip_text_size = 10,
           subtitle_family = "PT Serif",
           caption_family = "PT Serif") +
  theme(panel.grid = element_blank(), 
        axis.ticks.y = element_line(),
        strip.text = element_text(hjust = 0.5),
        plot.title = element_markdown(), 
        plot.subtitle = element_markdown())
  
ggsave(here("2019", "week34", "tw34_plot.png"), plot = waffle, width = 18, height = 6, device = agg_png())
