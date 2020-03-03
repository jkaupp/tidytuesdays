library(tidyverse)
library(fs)
library(here)
library(jkmisc)
library(readxl)
library(janitor)
library(scales)
library(ggtext)
library(glue)
library(worldtilegrid)
library(patchwork)

read_indicators <- function(x) {
  
  data <- read_excel(x, sheet = "Data", col_names = FALSE)
  
  names <- data[4,] %>% 
    t() %>% 
    as.character()
  
  data %>% 
    slice(-1:-4) %>% 
    set_names(names)
}

markup_facets <- function(x) {
  
  if (x == "Low Income") {
    
    highlight_text("Low Income", "#bb0a21", 'b')
    
  } else if (x == "Lower Middle Income") {
    
    highlight_text("Lower Middle Income", "#f6bd60", 'b')
    
    
  } else if (x == "Upper Middle Income") {
    
    highlight_text("Upper Middle Income", "#7d8491", 'b')
    
    
  } else if (x == "High Income") {
    
    highlight_text("High Income", "#dad6d6", 'b')
    
  } }


classification_data <- here("2020", "week9", "data") %>% 
  dir_ls(regexp = "CLASS") %>% 
  read_excel()

class_names <- classification_data[4,] %>% 
  t() %>% 
  as.character()

class_data <- classification_data %>% 
  slice(-1:-5) %>% 
  set_names(class_names) %>% 
  clean_names() %>% 
  remove_empty("rows") %>% 
  select(-contains("x")) %>% 
  rename(country_name = economy,
         country_code = code) %>% 
  filter(!is.na(country_name))

immunization_data <- here("2020", "week9", "data") %>% 
  dir_ls(regexp = "API",) %>%
  map_df(read_indicators) %>% 
  clean_names() %>% 
  pivot_longer(contains("x"), names_to = "year", values_to = "percent") %>% 
  mutate(year = parse_number(year),
         percent = parse_number(percent)/100) %>% 
  left_join(class_data) %>% 
  mutate(income_group = factor(income_group, levels = c("Low income", "Lower middle income", "Upper middle income", "High income"), labels = c("Low Income", "Lower Middle Income", "Upper Middle Income", "High Income")))

averages <- immunization_data %>% 
  filter(!is.na(income_group)) %>% 
  group_by(year, income_group, indicator_name) %>% 
  summarize(percent = mean(percent, na.rm = TRUE))

hits <- tibble(year = 2018) %>% 
  mutate(data = list(tibble(indicator_name = unique(immunization_data$indicator_name),
               percent = c(0.92, 0.92)))) %>% 
  unnest(data) %>% 
  mutate(income_group = "Low income") %>% 
  mutate(income_group = factor(income_group, levels = c("Low income", "Lower middle income", "Upper middle income", "High income"), labels = c("Low Income", "Lower Middle Income", "Upper Middle Income", "High Income")))

facet_labels <- tibble(year = rep(2023,2),
                       percent = 0.1,
                       indicator_name = unique(immunization_data$indicator_name),
                       income_group = rep("High income", 2),
                       label = c("DPT", "MEASLES")) %>% 
  mutate(income_group = factor(income_group, levels = c("Low income", "Lower middle income", "Upper middle income", "High income"), labels = c("Low Income", "Lower Middle Income", "Upper Middle Income", "High Income")))

world_grid <- immunization_data %>% 
  filter(!is.na(income_group), year == 2018) %>% 
  ggplot(aes(country = country_code, fill = income_group)) +
  geom_wtg(border_col = "#333333", show.legend = FALSE) +
  geom_text(aes(label = stat(alpha.2)), stat = "wtg", size = 2, family = "Oswald") +
  scale_fill_manual(values = c("#bb0a21", "#f6bd60", "#7d8491", "#dad6d6"), na.value = "grey95") +
  theme_jk() +
  theme(plot.background = element_rect(fill = "#eff1f3", color = "#eff1f3")) +
  theme_enhance_wtg() 



history <- immunization_data %>% 
  filter(!is.na(region)) %>% 
  ggplot(aes(x = year, y = percent)) +
  geom_path(color = "#7d8491", alpha = 0.1, size = 0.3) +
  geom_segment(linetype = "dashed",  x = 1980, xend = 2018, y = 0.92, yend = 0.92, size = 0.3) +
  geom_path(data = averages, color = "#bb0a21", size = 0.75) +
  geom_text(data = hits, aes(label = percent(percent)), hjust = 0, family = "Oswald", nudge_x = 1) +
  geom_point(data = filter(averages, year == 2018), color = "#bb0a21") +
  geom_text(data = filter(averages, year == 2018), aes(label = percent(percent, accuracy = 0.1)), color = "#bb0a21", nudge_x = 1, hjust = 0, family = "Oswald") +
  geom_text(data = facet_labels, aes(label = label), hjust = 1, family = "Oswald", fontface = 'bold', size = 12) +
  scale_y_continuous(limits = c(0, 1), labels = scale_percent_labels, expand = c(0.1,0)) +
  scale_x_continuous(limits = c(1980, 2023), c(0, 0)) +
  facet_grid(indicator_name ~ income_group) +
  labs(x = NULL,
       y = NULL,
       title = NULL,
       subtitle = NULL,
       caption = NULL) +
  theme_jk(base_family = "Oswald",
           grid = "XY",
           markdown = TRUE) +
  theme(plot.background = element_rect(fill = "#eff1f3", color = "#eff1f3"),
        axis.text.x = element_markdown(color = "#7d8491"),
        axis.text.y = element_markdown(color = "#7d8491"),
        axis.title.x = element_blank(),
        strip.text.y = element_blank(),
        strip.text.x = element_markdown())

labels <- paste0(map2_chr(c("Low Income", "Lower Middle Income", "Upper Middle Income", "High Income"), c("#bb0a21", "#f6bd60", "#7d8491", "#dad6d6"), ~highlight_text(.x, .y, "b")), collapse = ", ")

out <- wrap_plots(world_grid, history, nrow = 1, widths = c(0.25, 0.75)) +
  plot_annotation(title = "Global Immunization Rates for Diptheria-Pertussis-Tetanus (DPT) and Measles (MMR) by Income Groups",
                  subtitle = glue("Illustrated below on the left is the World Bank Income Groups ({labels}) and on the right is the {highlight_text('group average', '#bb0a21', 'b')} immunization rate for each vaccine.<br>The {highlight_text('dashed line', 'b')} indicates the herd immunity threshold (Pertussis for DPT) for each disease the vaccine addresses."),
                  caption = "**Data**: World Bank | **HIT Thresholds**: CDC | **Graphic**: @jakekaupp",
                  theme = theme_jk(markdown = TRUE) +
                    theme(plot.background = element_rect(fill = "#eff1f3", color = "#eff1f3")))

ggsave(here("2020", "week9", "tw9_plot.png"),plot = out, width = 17, height = 6, dev = ragg::agg_png())
