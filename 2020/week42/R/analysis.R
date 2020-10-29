library(tidyverse)
library(datasauRus)
library(ggpolypath)
library(jkmisc)
library(snakecase)
library(patchwork)
library(ggtext)
library(here)


dino <- ggplot(filter(datasaurus_dozen, dataset == "dino"), aes(x = x, y = y)) +
  geom_polypath(fill = "#cc2936") +
  scale_color_identity() +
  facet_wrap(~dataset, nrow = 5, labeller = as_labeller(to_title_case)) +
  coord_polar() +
  labs(x = NULL,
       y = NULL) +
  theme_jk(grid = FALSE,
           plot_title_family = "Teko",
           plot_title_size = 50,
           strip_text_family = "Teko",
           strip_text_size =  30)+
  theme(plot.title = element_text(color ="#08415C"),
        strip.text = element_text(color = "#08415C", hjust = 0.5),
        plot.background = element_rect(fill = "#eee5e9", color = "#eee5e9"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())


dozen <- ggplot(filter(datasaurus_dozen, dataset != "dino"), aes(x = x, y = y)) +
  geom_polypath(fill = "#cc2936") +
  scale_color_identity() +
  facet_wrap(~dataset, nrow = 5, labeller = as_labeller(to_title_case)) +
  labs(x = NULL,
       y = NULL,
       title = NULL,
       subtitle = NULL) +
  coord_polar() +
  theme_jk(grid = FALSE,
           strip_text_family = "Teko",
           strip_text_size =  30) +
  theme(strip.text = element_text(color = "#08415C", hjust = 0.5),
        plot.background = element_rect(fill = "#eee5e9", color = "#eee5e9"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())


plot <- wrap_plots(dino, dozen, ncol = 2) + plot_annotation(title = "Go Home Anscombosaurus, You're Drunk.",
                                                    subtitle = "Sometimes coordinate projections just ruin everyone's fun. Shown below are the Datasaurus dozen, 13 datasets with identical summary statistics but with unique layouts.  These datasets are<br>a variant of Anscombe's Quartet and are often used to highlight the importance of visualizing data.",
                                                    caption = "**Data**: {datasauRus} | **Graphic**: @jakekaupp",
  theme =  theme_jk(grid = FALSE,
                    plot_title_family =  "Teko",
                    plot_title_size =  50,
                    caption_family = "Poppins",
                    subtitle_family = "Poppins",
                    markdown = TRUE) +
    theme(plot.background = element_rect(fill = "#eee5e9", color = "#eee5e9"),
          plot.title = element_markdown(color = "#08415C"),
          plot.subtitle = element_markdown(color = "#08415C"),
          plot.caption = element_markdown(color = "#08415C"),
          axis.text.x = element_blank(),
          axis.text.y = element_blank()))

ggsave(here("2020", "week42", "tw42_plot.png"), plot = plot, dev = ragg::agg_png(), width = 16, height = 10)
