library(tidyverse)
library(jkmisc)
library(nord)
library(glue)
library(here)
library(ragg)

cpu <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-03/cpu.csv")

gpu <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-03/gpu.csv")

ram <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-03/ram.csv")


plot_data <- list(cpu = cpu, gpu = gpu) %>% 
  imap_dfr(~select(.x, date_of_introduction, transistor_count, area, process) %>% 
             mutate(type = .y)) %>% 
  group_by(date_of_introduction, type) %>% 
  summarize_at(vars(transistor_count, area, process), mean, na.rm = TRUE) %>% 
  arrange(date_of_introduction, type)

strip_labels <- tibble(type = c("cpu", "gpu"))

plot <- ggplot(plot_data, aes(x = area, y = transistor_count)) +
  geom_text(data = strip_labels, aes(label = toupper(type)), x = 400, y = 4, family = "Oswald Bold", size = 18, color = "grey90") +
  geom_smooth(method = "auto", formula = y ~ log10(x), se = FALSE, size = 0.5,  color = nth(nord_palettes$victory_bonds, 3)) +
  geom_hline(aes(yintercept = 10^10), linetype = "dotted", color = first(nord_palettes$victory_bonds)) +
  geom_point(aes(color = log10(process)), size = 3) +
  scale_color_nord(name = "Process Size",
                        discrete = FALSE,
                        palette = "lumina",
                        reverse = TRUE,
                        labels = function(x) glue("{scales::comma(10^x)} nm"),
                        breaks = c(1, 2, 3, 4)) +
  scale_y_log10(breaks = c(1, 10^4, 10^6, 10^8, 10^10),
                labels = c("1", "10K", "1M", "100M", "10B")) +
  scale_x_continuous(labels = function(x) glue("{x} {expression(mm^2)}")) +
  facet_wrap(~type) +
  labs(x = NULL, 
       y = NULL,
       title = "Moore's Law May Be Dead, Killed By The Tension Between Manufacturing and Transistor Density",
       subtitle = "*Moore's law*, the observation that the **number of transistors** on integrated circuits **doubles every two years**<br>
       hasn't held.  Transistor density is reaching a plateau, requiring manufacturing changes of an increase in available<br>
       chip size or a decrease in process size.",
       caption = "**Data:** Wikipedia | **Graphic:** @jakekaupp") +
  theme_jk(grid = "XY",
          markdown = TRUE) +
  theme(strip.text = element_blank())

ggsave(here("2019", "week36", "tw36_plot.png"), plot = plot, device = agg_png(), width = 10, height = 6)
