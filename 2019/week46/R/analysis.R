library(tidyverse)
library(here)
library(ggforce)
library(jkmisc)
library(waffle)
library(glue)

cran_code <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-12/loc_cran_packages.csv")

plot_data <- cran_code %>% 
  group_by(pkg_name) %>% 
  summarize(total = sum(code)) %>% 
  left_join(cran_code, .) %>% 
  mutate(percent = code/total) %>% 
  filter(language == "R") %>% 
  replace_na(list(percent = 0)) %>%
  arrange(percent) %>% 
  split(rep(1:196, each = 75)) %>% 
  imap_dfr(~mutate(.x, x = as.numeric(.y),
                  y = row_number())) %>% 
  arrange(x, y)

threshold <- filter(plot_data, percent >= 1) %>% 
  slice(1)

gunmetal_pal <- c("#AAA9AD", "#848689", "#5B676D", "#2A3439", "#1F262A")

plot <- ggplot(plot_data, aes(x = x, y = y, fill = percent)) +
  geom_tile() +
  geom_segment(data = threshold, aes(x = x, xend = x, y = -1, yend = y), inherit.aes = FALSE, color = "#fdca40", size = 0.5) +
  geom_segment(data = threshold, aes(x = x-1, xend = x-1, y = y, yend = 77), inherit.aes = FALSE, color = "#fdca40", size = 0.5) +
  geom_segment(data = threshold, aes(x = x-1, xend = x-1, y = y, yend = 77), inherit.aes = FALSE, color = "#fdca40", size = 0.5) +
  geom_segment(data = threshold, aes(x = x, xend = x-1, y = y, yend = y), inherit.aes = FALSE, color = "#fdca40", size = 0.5) +
  geom_text(data = threshold, aes(x = 150, y = 37.5, label = "37.5% (5509)\nPackages"), inherit.aes = FALSE, color = "#fdca40", family = "Oswald", hjust = 0, vjust = 0, fontface = "bold") +
  geom_text(data = threshold, aes(x = 10, y = 37.5, label = "62.5% (9180)\nPackages"), inherit.aes = FALSE, color = "black", family = "Oswald", hjust = 0, vjust = 0, fontface = "bold") +
  coord_equal() +
  labs(x = NULL,
       y = NULL,
       title = glue("Only {highlight_text('37.5%', '#fdca40', 'b')} of the Packages on CRAN contain {highlight_text('100%', '#1F262A', 'b')} R Code"),
       subtitle = str_wrap("Each tile in the chart below represents one of the almost 15,000 R packages on CRAN. Fill colour indicates the percentage of R code within each package. While this may be a shock to some users, this purposeful extensibility, alongside the vibrant and welcoming community, is a defining characteristic of the language.", 130),
       caption = "Data: **CRAN** | Graphic: **@jakekaupp**") +
  scale_fill_gradientn(colours = gunmetal_pal, name = "Percent R", labels = scales::percent) +
  theme_jk(grid = FALSE) +
  theme(axis.text = element_blank(),
        plot.title = ggtext::element_markdown(),
        plot.caption = ggtext::element_markdown(),
        legend.position = "bottom",
        legend.key.width = unit(1, "cm"),
        legend.justification = "top",
        legend.key.height = unit(2.5, "mm"))

ggsave(here("2019", "week46", "tw46_plot.png"), width = 10, height = 6) 

