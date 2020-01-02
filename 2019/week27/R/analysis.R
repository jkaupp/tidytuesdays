library(tidyverse)
library(jkmisc)
library(ggchicklet)
library(viridis)
library(glue)
library(here)

text_bc <- function(text, color) {
  
  glue("<span style = color:{color}>**{text}**</span>")
  
}


media_franchises <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-02/media_franchises.csv")

order <- media_franchises %>% 
  count(franchise, wt = revenue, sort = TRUE) %>% 
  ungroup() %>% 
  top_n(20, n) %>% 
  pull(franchise)

named_colors <- set_names(unique(media_franchises$revenue_category), viridis(8))

legend <- imap(named_colors[1:3], ~text_bc(.x, .y)) %>% 
  glue_collapse(sep = ', ') %>% 
  glue(",<br>") %>% 
  glue(" {glue_collapse(imap(named_colors[4:7], ~text_bc(.x, .y)), sep = ', ')}") %>% 
  glue(" and {imap(named_colors[8], ~text_bc(.x, .y))}") 


plot <- media_franchises %>% 
  filter(franchise %in% order) %>% 
  ggplot(aes(x = factor(franchise, rev(order)), y = revenue, fill = revenue_category)) +
  geom_chicklet(size = 0.1) +
  coord_flip() +
  labs(x = NULL,
       y = NULL,
       title = "Top 20 Franchises by Revenue",
       subtitle = glue("Illustrated below is the revenue by category: {legend}.<br> Merchandising is the undisputed king of revenue generation."),
       caption = "**Data**: Wikipedia | **Graphic**: @jakekaupp") +
  scale_fill_viridis_d() +
  scale_y_continuous(limits = c(0, 120), breaks = seq(0, 100, 25), labels = c("$0", "$25B","$50B", "$75B", "$100B"), expand = c(0, 0.1)) +
  theme_jk(grid = "X",
           markdown = TRUE) +
  theme(legend.position = "none")

ggsave(here('2019', "week27", "tw27_plot.png"), plot, width = 11, height = 8)
