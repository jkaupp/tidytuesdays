library(tidyverse)
library(ggalt)
library(jkmisc)
library(lubridate)
library(here)
library(scales)
library(janitor)
library(ggrepel)
library(patchwork)

state_hpi <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-05/state_hpi.csv")

prime_rates <- read_csv(here("2019","week6","data","MPRIME.csv")) %>% 
  clean_names() %>% 
  mutate(year = year(date)) %>% 
  select(-date) %>% 
  group_by(year) %>% 
  summarize_all(mean) %>% 
  filter(year %in% min(state_hpi$year):max(state_hpi$year))

highs <- filter(prime_rates, mprime %in% range(mprime)) %>% 
  distinct(mprime, .keep_all = TRUE)

plot_data <- state_hpi %>% 
  group_by(year, state) %>% 
  summarize_all(mean, na.rm = TRUE) 

prime <- ggplot(prime_rates, aes(x = year, y = mprime)) +
  geom_line(color = viridis_pal()(1), size = 0.5) +
  geom_point(data = highs, color = viridis_pal()(1)) +
  geom_text_repel(data = highs, aes(label = paste0(mprime, "%")), color = viridis_pal()(1), nudge_x = 2, nudge_y = 2, family = "Oswald", segment.size = 0) +
  scale_x_continuous(breaks = c(1975, seq(1980, 2010, 10), 2018)) +
  theme_jk(grid = "XY") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  labs(x = NULL, 
       y = NULL,
       title = "Interest Rates Fall, Housing Prices on the Rise",
       subtitle = str_wrap("The top chart shows the average prime interest rate by year since 1975.  The bottom heatmap illustrates the yearly average housing price index by State since 1975.", 100))


heatmap <- ggplot(plot_data, aes(x = year, y = fct_reorder(state, price_index, .fun = mean), fill = price_index)) +
  geom_tile(color = "white", size = 0.05) +
  scale_x_continuous(breaks = c(1975, seq(1980, 2010, 10), 2018)) +
  scale_fill_viridis_c("House Price Index", option = "viridis", direction = 1, breaks = pretty_breaks(5)) +
  scale_color_identity() +
  labs(x = NULL, y = NULL, caption = "Data: FRED | Graphic: @jakekaupp") +
  theme_jk(grid = "XY") +
  theme(legend.position = "bottom",
        legend.key.width = unit(1, "cm"))


out <- patchwork::wrap_plots(prime, heatmap, heights = c(0.2,1), ncol = 1)

ggsave(here("2019", "week6", "tw6_plot.png"), out, width = 8, height = 10)
