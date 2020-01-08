library(tidyverse)
library(googlesheets4)
library(lubridate)
library(here)
library(glue)
library(jkmisc)

sheet <- sheets_find("SMB_SR") 

sr_data <- sheets_read(sheet$id, sheet = "cleaned") %>% 
  mutate(time = as.character(time)) %>% 
  arrange(date) %>% 
  replace_na(list(time = "28:44:00")) %>% 
  mutate(id_y = row_number()) %>% 
  mutate(run_time = ms(str_sub(time, 1, 5)),
         time = as.numeric(str_replace(str_sub(time, 1, 5), "\\:", "\\.")),
         day = ymd(as_date(date)),
         days_bt = as.numeric(day - lag(day)),
         days_bt = replace(days_bt, is.na(days_bt), 10)) %>% 
  mutate(start = cumsum(50*days_bt),
         end = start + 500*time,
         end_rec = start + 500*last(time),
         hjust = c(rep(1, 13), rep(1, 25)))

h_lines <- tibble(yintercept = c(1, 3, 9, 14.5, 36, 38),
                  label = c("22:42:00","22:00:00", "20:00:00","19:30:00", "19:00:00", "18:59:85"))

v_lines <- tibble(xintercept = c(500, 26450, 38550, 51050, 72850, 110300, 158950, 161250, 202600, 217350, 242700, 253350, 276250),
                  label = c("2004","2005", "2006", "2007", "2008", "2010", "2012", "2013", "2015", "2016", "2017", "2018", "2019"),
                  hjust = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 1, 0, 0.5, 0.5, 0.5, 0.5, 0.5))


plot <- ggplot(sr_data) +
  geom_hline(data = h_lines, aes(yintercept = yintercept), size = 0.1,alpha = 0.3) +
  geom_vline(data = v_lines, aes(xintercept = xintercept), size = 0.1, alpha = 0.3) +
  geom_label(data = h_lines, aes(y = yintercept, x = Inf, label = label), hjust = 1, fill = "white", label.size = 0, family = "Press Start 2P", size = 2) +
  geom_label(data = v_lines, aes(x = xintercept, y = Inf, label = label, hjust = hjust), vjust = 0, fill = "white", label.size = 0, family = "Press Start 2P", size = 2) +
  geom_step(aes(x = (start + end)/2, y = id_y, color = ruleset), direction = "hv", size = 2) +
  geom_segment(aes(x = start, xend = end, y = id_y, yend = id_y), size = 4) +
  geom_segment(aes(x = start, xend = end_rec, y = id_y, yend = id_y), size = 4, color = "#fbd000") +
  geom_label(aes(x = start, y = id_y, label = paste0(runner, " ")), family = "Press Start 2P", size = 2, hjust = 1.1, fill = "white", label.size = 0, label.padding = unit(0, "lines")) +
  scale_y_reverse() +
  coord_cartesian(clip = "off") +
  scale_x_continuous(limits = c(-500, 300000)) +
  scale_color_manual(values = c("#049cd8", "#e52521", "#43b047")) +
  labs(x = NULL,
       y = NULL,
       title = "Super Mario Bros. Warpless Speedrun World Record Progression",
       subtitle = glue("Presented below is the world record progression for Super Mario Bros. warpless category, which entails completing the game as fast as possible without using any of the in-game warp zones.
       <br>The segements represent the **record at that time** compared to the {highlight_text('current record','#fbd000', 'b')}. The connecting lines represent the different rulesets used during the progression: {highlight_text('Twin Galaxies','#049cd8', 'b')}, {highlight_text('Peercast','#e52521', 'b')}
       <br>and {highlight_text('SpeedDemosArchive/Speedrun.com','#43b047', 'b')}."),
       caption = "**Data**: speedrun.com/sbm1 | **Graphic**: @jakekaupp") +
  theme_jk(grid = FALSE,
           markdown = TRUE,
           plot_title_family = "Press Start 2P") +
  theme(axis.text = element_blank(),
        legend.position = "none")

ggsave(here("2020", "week1", "tw1_plot.png"), plot, width = 16, height = 8, device = ragg::agg_png())
