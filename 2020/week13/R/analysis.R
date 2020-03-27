library(tidyverse)
library(here)
library(jkmisc)
library(janitor)
library(waffle)
library(glue)
library(scales)

tbi_age <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_age.csv')

injury_mechanisms <- set_names(unique(tbi_age$injury_mechanism), c("Motor Vehicle", "Struck By/Against", "Struck By/Against", "Other/Unknown", "Self-Harm", "Assault", "Other/Unknown"))

age_groups <- c("0-4", "5-14", "15-24", "25-34", "35-44", "45-54",  "55-64", 
                "65-74", "75+")

injury_labels <- c("Motor Vehicle", "Self-Harm", "Assault", "Other/Unknown", "Struck By/Against")

tbi_age_pruned <- tbi_age %>% 
  filter(age_group != "0-17", age_group != "Total") %>% 
  mutate(injury_mechanism = fct_recode(injury_mechanism, !!!injury_mechanisms),
         age_group = factor(age_group, age_groups)) %>% 
  count(age_group, injury_mechanism, wt = number_est) %>% 
  mutate(per_capita = n/1000)

pal <- c(RColorBrewer::brewer.pal(6, "Greys")[c(-1, -6)],"#048ba8") %>%  
  set_names(injury_labels)

titles <- imap(pal, ~highlight_text(.y, .x, 'b'))

plot <- ggplot(tbi_age_pruned) +
  geom_waffle(aes(fill = fct_rev(injury_mechanism), values = per_capita), n_rows = 10, flip = TRUE, size = 0.1, color = "white", show.legend = FALSE) +
  facet_wrap(~age_group, nrow = 1, strip.position = "bottom") +
  scale_x_discrete() + 
  scale_y_continuous(labels = function(x) glue("{x * 10}K"),
                     expand = c(0,0),
                     limits = c(0, 50)) +
  coord_equal() +
  scale_fill_manual(values = pal) +
  labs(title = "Traumatic Brain Injuries Peak in Late Teens/Early Adults and again in Seniors.",
       subtitle = glue("{paste0(flatten_chr(titles[-4:-5]), collapse = ', ')} and {titles[4]} all pose risks for brain injury, but accidents classified as<br>{titles$`Struck By/Against`} are the greatest risk across all ages."),
       caption = "**Data**: CDC | **Graphic**: @jakekaupp") +
  theme_jk(grid = FALSE, 
           ticks = TRUE,
           markdown = TRUE) +
  theme(strip.text.x = element_text(hjust = 0.5, size = 9))

ggsave(here("2020", "week13", "tw13_plot.png"), plot, width = 10, height = 6, dev = ragg::agg_png())
