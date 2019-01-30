library(tidyverse)
library(ggalt)
library(jkmisc)
library(here)
library(scales)

milk_cow_data <- read_csv("https://github.com/rfordatascience/tidytuesday/raw/master/data/2019/2019-01-29/milkcow_facts.csv")

milk_product_facts <- read_csv("https://github.com/rfordatascience/tidytuesday/raw/master/data/2019/2019-01-29/milk_products_facts.csv")

us_pop <- read_csv(here("2019", "week5", "data","us-population-1990-to-2016.csv"))

totals <- milk_product_facts %>% 
  mutate(total_consumption = rowSums(select(., -year))) %>% 
  select(year, total_consumption)

full_data <- left_join(milk_cow_data, totals) %>% 
  left_join(us_pop) %>% 
  mutate(total_consumption_lbs = total_consumption * population)


ggplot(full_data, aes(y = total_consumption_lbs, x = milk_production_lbs)) +
  geom_xspline2(aes(s_open = TRUE, s_shape = 0.5)) +
  geom_point(shape = 21, fill = "black", color = "white", stroke = 1) +
  scale_y_continuous(labels = scales::unit_format(unit = "B", scale = 10e-10, sep = ""), breaks = pretty_breaks(6)) +
  scale_x_continuous(labels = scales::unit_format(unit = "B", scale = 10e-10, sep = ""), breaks = pretty_breaks(6)) +
  labs(x = "US Milk Production (lbs)",
       y = "US Average Dairy Consumption (lbs)",
       title = "100 Slices of American Cheese or, the Fable of Supply Management",
       subtitle = str_wrap("The connected scatterplot below illustrates the relationship between total average dairy consumption and total milk production over the past 25 years.
                           US supply far exceeds the demand, highlighting overproduction and a case for supply management.", 120)) +  
  theme_jk()
