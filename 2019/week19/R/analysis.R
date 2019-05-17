library(tidyverse)
library(readxl)
library(here)
library(fs)
library(jkmisc)
library(janitor)
library(countrycode)
library(patchwork)

ratio_data <- here("2019", "week19", "data") %>% 
  dir_ls(regexp = "student_teacher_ratio") %>% 
  read_excel(na = c("..")) %>% 
  set_names(tolower(names(.))) %>% 
  gather(year, ratio, -1:-2,convert = TRUE)



plot_region <- function(data) {
  
  ggplot(data, aes(x = year, y = ratio, group = region, color = continent)) +
    geom_line(size = 0.3, show.legend = FALSE) +
    geom_ribbon(aes(ymin = 0, ymax = ratio, fill = continent), alpha = 0.5, color = NA, show.legend = FALSE) +
    facet_wrap(~region, nrow = 1) +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    scale_x_continuous(breaks = seq(1970, 2010, 10)) +
    scale_y_continuous(limits = c(0, 40)) +
    labs(x = NULL,
         y = NULL,
         title = unique(data$continent)) +
    theme_jk(grid = "XY") 
  
  }

plot_continent <- function(data) {
  
  ggplot(data, aes(x = year, y = ratio, color = continent)) +
    geom_line(size = 0.3, show.legend = FALSE) +
    geom_ribbon(aes(ymin = 0, ymax = ratio, fill = continent), alpha = 0.5, color = NA, show.legend = FALSE) +
    facet_wrap(~continent, ncol = 1) +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    scale_x_continuous(breaks = seq(1970, 2010, 10)) +
    scale_y_continuous(limits = c(0, 40)) +
    scale_shape_identity() +
    labs(x = NULL,
         y = NULL,
         title = unique(data$continent)) +
    theme_jk(grid = "XY") +
    theme(strip.text = element_blank())
  
}

plot_data <- ratio_data %>% 
  filter(type == "Countries") %>% 
  mutate(country_code = countrycode(country, "country.name", "iso3c")) %>% 
  mutate(region = countrycode(country_code, "iso3c", "region")) %>% 
  mutate(continent = countrycode(country_code, "iso3c", "continent")) %>% 
  filter(!is.na(region))

colors <- set_names(c("#171635", "#00225D", "#763262", "#CA7508", "#E9A621"), c(unique(plot_data$continent)))

individual <- plot_data %>% 
  group_by(year, region, continent) %>% 
  summarize(ratio = mean(ratio, na.rm = TRUE)) %>% 
  filter(!is.nan(ratio)) %>% 
  split(.$continent) %>% 
  map(plot_region) 

summary <- plot_data %>% 
  group_by(year, continent) %>% 
  summarize(ratio = mean(ratio, na.rm = TRUE)) %>% 
  filter(!is.nan(ratio)) %>% 
  split(.$continent) %>% 
  map(plot_continent) 
  
plots <- map2(summary, individual, ~wrap_plots(.x, .y, nrow = 1, widths = c(1, 1)))
  
out <- wrap_plots(plots, ncol = 1) +
  plot_annotation(title = "Working to Two Sigma: Student Teacher Ratios Improving Since the 1970s",
                  subtitle = str_wrap("Illustrated below is the average student to teacher ratio across each continent (left column) and region (right column).  Continent and region assigned from iso3c coding of country name and are consistent with the World Bank Dvelopment Indicators.", 210),
                  caption = "Data: UNESCO Institute of Statistics | Graphic: @jakekaupp",
                  theme = theme_jk())

ggsave(here("2019","week19","tw19_plot.png"), out, width = 16, height = 10)
