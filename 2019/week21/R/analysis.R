library(tidyverse)
library(here)
library(readxl)
library(fs)
library(janitor)
library(jkmisc)
library(patchwork)


# Plotting Function to make separate ordered stacked bars by group----
make_bars <- function(df, pals) {
  
  order <- df %>% 
    arrange(desc(other)) %>% 
    pull(country)
  
  labs <- c("HIC" = "High Income Group",
            "UMI" = "Upper Middle Income Group",
            "LMI" = "Lower Middle Income Group",
            "LI" = "Low Income Group")
  
  
  df %>% 
    gather(type, value, c("other", "plastic_waste", "inadequate_waste", "littered_waste")) %>% 
    mutate(type = factor(type, c("other", "inadequate_waste", "plastic_waste", "littered_waste"), c("Other", "Inadequately Managed Waste", "Plastic Waste", "Littered Waste"))) %>% 
    mutate(country = factor(country, order)) %>% 
    mutate(alpha = if_else(type == 'Other', 0.5, 0.8)) %>% 
    ggplot() +
    geom_col(aes(x = country, y = value, fill = type, alpha = alpha), width = 0.90, size = 0.1) +
    coord_flip() +
    scale_fill_manual("", values = pals) +
    scale_alpha_identity() +
    # scale_x_discrete(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0.05), labels = scales::percent) +
    labs(x = NULL, y = NULL) +
    facet_wrap(~economic_status, scales = "free_y", labeller = as_labeller(labs)) +
    theme_jk(grid = FALSE) +
    theme(legend.direction = "horizontal")
  
}


# Function to extract ggplot legends ----
extract_legend <- function(ggp){
  
  tmp <- ggplot_gtable(ggplot_build(ggp))
  
  leg <- which(map_lgl(tmp$grobs, function(x) x$name == "guide-box"))
  
  legend <- tmp$grobs[[leg]]
  
  return(legend)}


# Read in Coastal Waste Data----
# Plastic waste inputs from land into the ocean
# BY JENNA R. JAMBECK, ROLAND GEYER, CHRIS WILCOX, THEODORE R. SIEGLER, MIRIAM PERRYMAN, ANTHONY ANDRADY, RAMANI NARAYAN, KARA LAVENDER LAW
# 
# SCIENCE13 FEB 2015 : 768-771

coastal_waste <- here("2019", "week21", "data") %>% 
  dir_ls(regexp = "xlsx") %>% 
  read_excel() %>% 
  clean_names() %>% 
  set_names(str_remove(names(.), "_*[0-9]$")) %>% 
  mutate(country = str_remove(country, "[0-9]")) %>% 
  mutate(country = case_when(str_detect(country, "Palestine") ~ "Palestine",
                             str_detect(country, "Korea, South") ~ "South Korea",
                             str_detect(country, "Korea, North") ~ "North Korea",
                             str_detect(country, "Congo") ~ "Congo",
                             TRUE ~ country)) %>% 
  filter(!grepl("Burma", country)) %>% 
  filter(complete.cases(.)) %>% 
  mutate(other = 100 - (percent_plastic_in_waste_stream + percent_inadequately_managed_waste + percent_littered_waste)) %>% 
  rename(plastic_waste = percent_plastic_in_waste_stream, inadequate_waste  = percent_inadequately_managed_waste, littered_waste = percent_littered_waste) %>% 
  mutate_at(c("other", "plastic_waste", "inadequate_waste", "littered_waste"), function(x) x/100) 


# Palette for plot----
pal <- c("#F5F0F6", "#629460", "#385F71", "#2B4162")


# Make plots----
list <- coastal_waste %>% 
  split(.$economic_status) %>% 
  map(make_bars, pal)

# Extract legend----
legend <- extract_legend(list[[1]])

# Remove legend from list of plots----
list <- map(list, ~.x + theme(legend.position = "none"))
  
# Finish plot----
out <- wrap_plots(list[c("HIC", "UMI", "LMI", "LI")], nrow = 1) / legend + plot_layout(heights = c(0.95, 0.05)) +
  plot_annotation(title = "The Relationship Between World Bank Income Classification and Mismanaged Waste",
                  subtitle = str_wrap("Illustrated below is the percentage of waste by category for each country by World Bank income classification.  The lower the classification, the higher the mismanaged waste.  Much of this mismanaged waste (especially plastics) ends up in waterway that ultimately lead to our oceans, suggesting that global income inequality plays a role in ocean pollution by hampering the implementation of effective waste management strategies.", 200),
                  caption = "Data: Jambeck, Jenna R., et al. 'Plastic waste inputs from land into the ocean.' Science 347.6223 (2015): 768-771. | Graphic: @jakekaupp",
                  theme = theme_jk())

ggsave(here("2019", "week21", "tw21_plot.png"), out, width = 16, height = 12)


