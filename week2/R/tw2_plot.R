library(here)
library(readxl)
library(tidyverse)
library(glue)
library(janitor)
library(rvest)
library(nord)
library(jkmisc)
library(viridis)

# Function to scrape the top avg cap salary by player ----
pull_position_data <- function(year, position) {
  
  Sys.sleep(5)
  
  url <- glue("http://www.spotrac.com/nfl/positional/{year}/{position}")
  
  read_html(url) %>% 
    html_nodes("#main > div.teams > table:nth-child(6)") %>% 
    html_table() %>%
    flatten_df() %>% 
    set_names(c("rank","player","cap_dollars", "cap_percent"))
}


# Formatter for 538 year labels 
labels_538 <- function(labels) {
  labels_out <- sprintf("20%s", str_sub(labels, 3, 4))
  labels_out <- c(labels_out[1], glue("'{str_sub(labels_out[-1], 3, 4)}"))
  return(labels_out)
}

# Create the data scaffold 
years <- 2011:2018
positions <- c("quarterback", "running-back", "fullback", "guard", "center", "left-tackle", "right-tackle", "tight-end","wide-receiver","defensive-end","cornerback","defensive-tackle", "inside-linebacker", "outside-linebacker", "free-safety", "strong-safety", "kicker","punter","long-snapper")

scaffold <- tibble(year = years,
                   position = list(positions)) %>% 
  unnest() 

# Populate the scaffold
if(!file.exists(here("week2", "data", "position_cap_data_named.RDS"))) {
  
  scaffold <- scaffold %>% 
    mutate(data = map2(year, position, ~pull_position_data(.x, .y))) %>% 
    unnest() %>% 
    mutate_at(c("cap_dollars", "cap_percent"), parse_number) %>% 
    mutate(side = case_when(position %in% c("quarterback", "running-back", "fullback", "guard", "center", "left-tackle", "right-tackle", "tight-end","wide-receiver") ~ "Offense",
                            position %in% c("kicker","punter","long-snapper") ~ "Special Teams",
                            TRUE ~ "Defense"))
  
  
  # Save it to avoid re-scraping 
  saveRDS(scaffold, file = here("week2", "data", "position_cap_data_named.RDS"))
} else {
  
  scaffold <- readRDS(here("week2", "data", "position_cap_data_named.RDS"))
  
}


# Make data for the plot
plot_data <- scaffold %>% 
  group_by(year, position, side) %>% 
  top_n(16, cap_dollars) %>% 
  summarize(avg_pay = mean(cap_dollars))
  
# Make a heatmap!
ggplot(plot_data, aes(x = year, y = position, fill = avg_pay)) +
  geom_tile(color = "white", size = 0.1) +
  coord_equal() +
  labs(x = NULL, y = NULL, title = "The Fullback Gets No Respect", subtitle = "Average cap value of the 16 highest payed players in all positions", caption = "Data: http://www.spotrac.com/ | Graphic: @jakekaupp") +
  scale_x_continuous(labels = labels_538, breaks = 2011:2018) +
  scale_y_discrete(labels = function(x) str_to_title(gsub("[[:punct:]]", " ", x))) +
  scale_fill_viridis(discrete = FALSE, labels = scales::dollar, name = "Average Salary") +
  theme_jk(grid = FALSE, base_size = 14)

ggsave(here("week2", "tw2_heatmap.png"), width = 8, height = 8)
