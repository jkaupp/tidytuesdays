library(tidyverse)
library(lubridate)
library(jkmisc)
library(patchwork)

bike_traffic <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-02/bike_traffic.csv")

clean_bikes <- bike_traffic %>% 
  mutate(date = mdy_hms(date),
         month = month(date),
         year = year(date)) %>% 
  filter(between(year, 2014, 2018))

by_year <- clean_bikes %>% 
  group_by(year, month, crossing) %>% 
  summarize(total_bikes = sum(bike_count, na.rm = TRUE)) 

glyph <- ggplot(by_year, aes(x = crossing, y = total_bikes, group = year)) +
  geom_path() +
  geom_point(size = 1, aes(color = crossing)) +
  facet_grid(year ~ month, labeller = labeller(.cols = set_names(month.abb, 1:12)), switch = "y") +
  scale_color_manual(values = tol7qualitative) +
  theme_jk(grid = FALSE) +
  labs(x = NULL, 
       y = NULL) +
  theme(axis.text = element_blank(),
        strip.text.y = element_text(angle = 180),
        legend.position = "none")

main <- by_year %>% 
  filter(year == 2015, month == 1) %>% 
  ggplot(aes(x = crossing, y = total_bikes, group = year)) +
  geom_path() +
  geom_point(size = 3, aes(color = crossing)) +
  scale_color_manual("Crossing", values = tol7qualitative) +
  theme_jk(grid = FALSE) +
   labs(x = NULL, 
       y = NULL) +
  theme(axis.text = element_blank(),
        strip.text.y = element_text(angle = 180))

wrap_plots(list(main, glyph), widths = c(0.25, 0.75)) +
  plot_annotation(title = "Annual Patterns in Seatle Bicycle Traffic", 
                  subtitle = str_wrap("This chart is a parallel coordinates plot that illustrates the monthly bike traffic at Seattle crossings.  A  colored dot represents each crossing and vertical position represents the total number of riders counted each month.  You can observe the year over year trends, as well as see which crossings experience cyclical patterns and which remain stable.", 170),
                  theme = theme_jk())

