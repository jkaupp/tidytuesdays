library(tidyverse)
library(jkmisc)
library(lubridate)
library(here)
library(nord)
library(colorspace)
library(glue)
library(patchwork)

loans_raw <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-26/loans.csv")

loans <- loans_raw %>% 
  mutate(year = as.numeric(paste0('20', year)),
         quarter = case_when(quarter == 1 ~ 3,
                             quarter == 2 ~ 6,
                             quarter == 3 ~ 9,
                             quarter == 4 ~ 12),
         date = ymd(paste(year, quarter, "01", sep ="-"))) 

loans_by_type <- loans %>% 
  select(-starting, -added) %>% 
  pivot_longer(total:wage_garnishments) %>% 
  filter(name != "total") %>% 
  group_by(year, name, date) %>% 
  summarize(value = sum(value, na.rm = TRUE))

order <- loans_by_type %>% 
  group_by(name) %>% 
  summarize(max = max(value)) %>% 
  arrange(max) %>% 
  pull(name)

loans_total <- loans_by_type %>% 
  group_by(date) %>% 
  summarize(total = sum(value)) 


plot <- loans_by_type %>% 
  left_join(loans_total) %>% 
  ungroup() %>% 
  mutate(percent = value/total) %>% 
  mutate(name = factor(name, order)) %>% 
  ggplot(aes(x = date, y = percent)) +
  geom_area(aes(y = 1), fill = nord("mountain_forms")[3]) +
  geom_area(aes(fill = name), show.legend = FALSE) +
  geom_line(aes(color = name), size = 0.2, show.legend = FALSE) +
  facet_wrap(~name, nrow = 1, labeller = as_labeller(snakecase::to_title_case)) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = darken(nord("mountain_forms")[-3])) +
  labs(x = NULL, 
       y = NULL,
       title = str_to_title("The Stark Difference in How Student Loan Dollars are Repaid"),
       subtitle = glue("Illlustrated below is the categorization of debt repayments {highlight_text('Voluntary Payments', nord('mountain_forms')[1], 'b')}, {highlight_text('Wage Garnishments', nord('mountain_forms')[2], 'b')}, {highlight_text('Consolidation', nord('mountain_forms')[4], 'b')}
       and {highlight_text('Rehabilitation', nord('mountain_forms')[5], 'b')} as a<br>percent of total repayments by year and fiscal quarter.  The continual high percentage of rehabilitated potentially indicates of how unsustainable<br>the current loan repayment structure is for a lot of graduates, enrolling these programs to avoid negative penalities to their personal credit."),
      caption =  "Data: **US Department of Education** | Graphic: **@jakekaupp**") +
  scale_fill_nord("mountain_forms") +
  theme_jk(grid = FALSE,
           ticks = TRUE,
           markdown = TRUE) 

ggsave(here('2019', 'week48', 'tw48_plot.png'), plot, width = 12, height = 6, dev = ragg::agg_png())


