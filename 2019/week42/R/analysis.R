library(tidyverse)
library(here)
library(janitor)
library(jkmisc)
library(ggalt)
library(ggtext)
library(glue)
library(ragg)

big_epa_cars <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-15/big_epa_cars.csv")

top <- big_epa_cars %>% 
  clean_names() %>% 
  count(make, year) %>% 
  count(make) %>% 
  filter(n == 37)

plot_data <- big_epa_cars %>% 
  clean_names() %>% 
  select(make, v_class, year, you_save_spend) %>% 
  semi_join(top) %>% 
  group_by(year, make) %>% 
  summarize(total_save_spend = mean(you_save_spend)) %>%
  group_by(year) %>% 
  mutate(rank = min_rank(desc(total_save_spend))) %>% 
  ungroup() %>% 
  mutate(size = if_else(make == "Ford", 1, 0.5),
         make = factor(make, pull(top, make)),
         make = fct_relevel(make, "Ford", after = Inf),
         make = fct_recode(make, "**Ford**" = "Ford"))


grid <- tibble(rank = 1:22)

colors <- set_names(grey.colors(22), pull(top, make) %>%
                      factor() %>%
                      fct_recode("**Ford**" = "Ford"))

colors[["**Ford**"]] <- "#DD2A7B"


plot <- ggplot(plot_data, aes(x = year, y = rank)) +
  geom_segment(data = grid, aes(x = 1983, xend = 2021, y = rank, yend = rank), color = "#cccccc", alpha = 0.5, size = 0.1) +
  geom_xspline(aes(color = make, size = size), show.legend = FALSE) +
  geom_point(aes(fill = make), shape = 21, color = "white", show.legend = FALSE) +
  geom_richtext(data = filter(plot_data, year == 2020), aes(label = as.character(make), x = 2021, color = make), hjust = 0, family = "Lora", size = 4, show.legend = FALSE,  fill = NA, label.color = NA, 
                label.padding = grid::unit(rep(0, 4), "pt")) +
  geom_text(data = filter(plot_data, year == 1984), aes(label = rank, x = 1983), hjust = 1, family = "Oswald", size = 4) +
  labs(x = NULL,
       y = NULL,
       title = "From Chugging to Sipping: Fuel Cost Savings of Major Automakers since 1984",
       subtitle = glue("Shown below is a rankings chart of average fuel cost savings, measured over 5 years, from 1984 to 2020.  {highlight_text('Ford','#DD2A7B', 'b')} has had quite the journey, battling from the bottom<br>of the list to the second-best North American manufacturer.")) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_size_identity() +
  scale_x_continuous(breaks = 1984:2020) +
  scale_y_continuous(trans = "reverse", breaks = NULL) +
  expand_limits(x = 2025) +
  theme_jk(grid = "X", 
           markdown = TRUE)

ggsave(here("2019", "week42", "tw42_plot.png"), plot = plot, width = 13, height = 6)
