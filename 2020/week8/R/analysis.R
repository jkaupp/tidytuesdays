library(tidyverse)
library(jkmisc)
library(ggforce)
library(patchwork)
library(glue)
library(ggtext)
library(pBrackets)
library(scales)
library(here)

food_consumption <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv')


plot_data <- food_consumption %>%
  group_by(food_category) %>%
  summarize_at(vars(consumption:co2_emmission), list(sum)) %>% 
  mutate(color = case_when(food_category %in% c("Beef", "Lamb & Goat", "Milk - inc. cheese", "Pork") ~ "#DD2A7B",
                           TRUE ~ "#81a1c1"),
         size = case_when(food_category %in% c("Beef", "Lamb & Goat", "Milk - inc. cheese", "Pork") ~ 1,
                           TRUE ~ 0.2)) %>% 
  mutate(food_category = case_when(food_category == "Milk - inc. cheese" ~ "Milk (Including Cheese)",
                                   food_category == "Nuts inc. Peanut Butter" ~ "Nuts (Including Peanut Butter)",
                                   food_category == "Wheat and Wheat Products" ~ "Wheat & Wheat Products",
                                   TRUE ~ food_category)) %>% 
  pivot_longer(cols = -c("food_category", "size", "color")) %>% 
  mutate(name = factor(name, c("consumption", "co2_emmission")),
         hjust = if_else(name == "consumption", 1.1, -0.1)) %>% 
  arrange(food_category, name) 


other_foods_co2 <- filter(plot_data, size != 1) %>% 
  filter(name == "co2_emmission") %>% 
  arrange(desc(value)) %>% 
  summarize(label = paste0(food_category, collapse ="\n"),
            value = value[food_category == "Wheat & Wheat Products"])

other_foods_consumption <- filter(plot_data, name == "consumption") %>% 
  filter(!(food_category %in% c("Milk (Including Cheese)", "Wheat & Wheat Products"))) %>% 
  arrange(desc(value)) %>% 
  summarize(label = paste0(food_category, collapse ="\n"),
            value = value[food_category == "Rice"])

labels <- plot_data %>% 
  filter(size == 1 | food_category %in% c("Wheat & Wheat Products")) %>% 
  filter(!(size != 1 & name == "co2_emmission")) %>% 
  filter(!(food_category %in% c("Beef", "Pork", "Lamb & Goat") & name == "consumption"))


grid_lines <- tibble(x = rep(1, length(seq(0, 50000, 10000))),
               xend = rep(2, length(seq(0, 50000, 10000))),
               y = seq(0, 50000, 10000),
               yend = seq(0, 50000, 10000)) %>% 
  mutate(label = comma(y)) %>% 
  mutate(label = if_else(y %in% range(y), glue("{comma(y)} kg/person per year"), label))
               
               
# https://stackoverflow.com/questions/35633239/add-curly-braces-to-ggplot2-and-then-use-ggsave
# Credit to one of the masters of grid graphics @baptiste
bracketsGrob <- function(...){
  l <- list(...)
  e <- new.env()
  e$l <- l
  
  
  grid:::recordGrob(  {
    do.call(grid.brackets, l)
  }, e)
}

b1 <- bracketsGrob(x1 = 0.25, y1 = 0.05, x2 = 0.25, y2 = 0.12, h = 0.025, lwd = 1, col = "#E5E9F0")
b2 <- bracketsGrob(x1 = 0.75, y1 = 0.05, x2 = 0.75, y2 = 0.138, h = -0.025,  lwd = 1, col = "#E5E9F0")



plot <- ggplot(plot_data, aes(x = name, y = value)) +
  geom_vline(xintercept = c(1,2), color = "#E5E9F0") +
  geom_segment(data = grid_lines, aes(x = x, xend = xend, y = y, yend = yend), color = "#E5E9F0", linetype = "dashed", size = 0.2) +
  geom_text(data = grid_lines, aes(x = 1, y = y, label = label), color = "#E5E9F0", family = "Oswald", nudge_y = c(-1000, rep(700, 5)), nudge_x = c(-0.02 , rep(0, 4), -0.025), hjust = -0.1) +
  geom_path(aes(color = color, size = size, group = food_category)) +
  geom_point(aes(color = color)) +
  geom_text(data = filter(labels, name == "consumption"), aes(label = food_category, x = 0.95), family = "Oswald", hjust = 1, color = "white") +
  geom_text(data = filter(labels, name == "co2_emmission"), aes(label = food_category, x = 2.05), family = "Oswald", hjust = 0, color = "white") +
  geom_text(data = other_foods_co2, aes(label = label, x = 2.2), family = "Oswald",  hjust = 0, color = "white") +
  geom_text(data = other_foods_consumption, aes(label = label, x = 0.8), family = "Oswald",  hjust = 1, nudge_y = -1000, color = "white") +
  annotation_custom(b1) + 
  annotation_custom(b2) + 
  scale_x_discrete(breaks = c("consumption", "co2_emmission"), labels = c("Consumption", expression(paste("C", O[2]," Emissions",sep=""))), position = "top") +
  scale_y_continuous(limits = c(-1000, 55000)) +
  scale_color_identity() +
  scale_size_identity() +
  labs(x = NULL,
       y = NULL,
       title = expression(paste('While Low in Consumption, Meat & Dairy Staggeringly Outweigh Other Foods in Their Contribution to C', O[2],' Emissions',sep='')),
       subtitle = glue("Illustrated below is a slopegraph contrasting **consumption** and **carbon dioxide emmissions** in different food products measured in 2019.<br>{highlight_text('Foods with high emmissions', '#DD2A7B', 'b')} relative to their consumption are beef, lamb, goats, pork along with milk and cheese. {highlight_text('Foods with low emmissions', '#81a1c1', 'b')}<br>relative to their consumption include grains, nuts, beans, eggs, fish and poultry."),
       caption = "**Data**: nu3.de | **Graphic**: @jakekaupp") +
  theme_jk(grid = FALSE,
           dark = TRUE) +
  theme(axis.text.x = element_markdown(size = 16),
        plot.caption = element_markdown(),
        plot.subtitle = element_markdown(),
        axis.text.y = element_blank())
  
ggsave(here("2020", "week8", "tw8_plot.png"), plot, height = 14, width = 11.5, dev = ragg::agg_png())
