library(tidyverse)
library(lubridate)
library(here)
library(jkmisc)
library(glue)
library(scales)
library(ggtext)

grosses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/grosses.csv', guess_max = 10000)
cpi <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/cpi.csv')


corrected <- grosses %>% 
  mutate(year_month = as.Date(glue("{year(week_ending)}-{month(week_ending)}-01"))) %>% 
  left_join(cpi, by = "year_month") %>% 
  mutate(cpi = cpi/first(cpi),
         corr_avg_price = avg_ticket_price/cpi,
         corr_weekly_gross = weekly_gross/cpi,
         year = year(week_ending)) 

long_shows <- corrected %>% 
  group_by(show) %>% 
  summarize(runs = n_distinct(year),
            shows = sum(performances)) %>% 
  arrange(desc(runs)) %>% 
  filter(runs >= 5) %>% 
  select(show, runs, shows)

money <- corrected %>% 
  semi_join(long_shows) %>% 
  group_by(show) %>% 
  mutate(corr_weekly_gross = cumsum(corr_weekly_gross)) 

top10 <- money %>% 
  filter(corr_weekly_gross == max(corr_weekly_gross)) %>% 
  ungroup() %>% 
  top_n(10, corr_weekly_gross) %>% 
  arrange(desc(corr_weekly_gross)) 

order <- pull(top10, show)

top10 <- top10 %>% 
  left_join(long_shows) %>% 
  mutate(idx = show,
         idx = factor(show, order, order))
  

plot_data <- imap_dfr(pull(top10, show), ~left_join(money, select(top10,  show, corr_weekly_gross, runs, shows, -corr_weekly_gross), by = "show") %>%
          mutate(color = if_else(show == .x, "#AA3377", NA_character_),
                 alpha = if_else(show == .x, 1, NA_real_),
                 size = if_else(show == .x, 1.5, NA_real_),
                 idx = .x) %>% 
           replace_na(list(color = "#eceff4",
                           alpha = 0.2,
                           size = 0.3)) %>% 
           mutate(color = factor(color, rev(c("#AA3377", "#eceff4"))),
                  idx = factor(idx, levels = order, labels = order)) %>% 
           arrange(idx, color))

labels <- tibble(show = pull(top10, show),
                 idx = factor(show, order, order),
                 x = ymd("1985-06-09"),
                 y = 790000000)

lines <- tibble(show = pull(top10, show),
                 idx = factor(show, order, order),
                 x = ymd("2020-03-01"),
                 y = list(seq(0, 800000000, 200000000))) %>%
  unnest(y) %>% 
  mutate(label = if_else(y == 0, "", dollar(y, scale = 1/1000000, suffix = "M"))) %>% 
  filter(show %in% c("Chicago", "Jersey Boys"))


 plot <- ggplot(plot_data, aes(x = week_ending, y = corr_weekly_gross, group = show, color = color, alpha = alpha, size = size)) +
  geom_hline(yintercept = seq(0, 800000000, 200000000), alpha = 0.1, size = 0.2, color = "#eceff4") +
  geom_step() +
  geom_point(data = top10, aes(x = week_ending, y = corr_weekly_gross, group = show), inherit.aes = FALSE, color = "#AA3377") +
  geom_text(data = labels, aes(x = x, y = y, label = str_wrap(toupper(show), 15)), inherit.aes = FALSE, hjust = 0, vjust = 1, family = "Anton", size = 8, color = "#e5e9f0") +
  geom_text(data = lines, aes(x = x, y = y, label = label), inherit.aes = FALSE,  vjust = 1.1, hjust = -0.2, family = "Antonio", alpha = 0.2, color = "#eceff4") +
  scale_y_continuous(labels = function(x) dollar(x, scale = 1/1000000, suffix = "M"), position = "right", limits = c(0, 850000000), expand = c(0,0.1)) + 
  scale_x_date(limits = ymd(c("1983-06-09", "2025-03-01"))) +
  labs(x = NULL,
       y = NULL,
       title = "Gross Earnings Trajectories of the Top 10, Multi-Run Broadway Productions",
       subtitle = glue("Shown below is a step graph of the cummulative earnings (corrected for inflation) of broadway shows with more than four runs from 1985 to 2020. {highlight_text('The top 10 shows', '#AA3377', 'b', 18)} are shown against the field.<br>Of the newer shows, Hamilton exhibits the same trend to break the $400 million barrier."),
       caption = "**Data**: Playbill via @alexcookson | **Graphic**: @jakekaupp") +
  facet_wrap(~idx, nrow = 2, as.table = TRUE) +
  scale_color_identity() +
  scale_alpha_identity() +
  scale_size_identity() +
  theme_jk(grid = FALSE,
           markdown = TRUE,
           dark = TRUE, 
           plot_title_size = 24) +
  theme(axis.text.y = element_blank(),
        strip.text = element_blank(),
        axis.text.x = element_markdown(color = alpha("#eceff4", 0.2), family = "Antonio", size = 12, vjust = 1))

ggsave(here("2020", "week18", "tw18_plot.png"), plot, width = 16, height = 10, device = ragg::agg_png()) 
 
