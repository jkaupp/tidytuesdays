library(tidyverse)
library(here)
library(jkmisc)
library(scales)
library(ggiraph)
library(glue)
library(waffle)

make_tooltip <- function(occupation, female, male, income_gap, ...) {
 
  glue('<div class="tipchart">
      <h3>{occupation}</h3>
      <h4>Mens taxable income {ifelse(income_gap <= 1, percent(1-round(income_gap, 2)), percent(round(income_gap, 2)))} {ifelse(income_gap <= 1, "less", "more")} than womens</h4>
      <table>
      <tr class="tiprow">
      <td class="tipheader">Average Male Taxable Income:</td>
      <td class="tiptext">{dollar(male)}</td>
      </tr>
      <tr class="tiprow">
      <td class="tipheader">Average Female Taxable Income:</td>
      <td class="tiptext">{dollar(female)}</td>
      </tr>
      </table>
      </div>')
  
}


# Read in the data
income_data <- dir(here("week4","data"), pattern = "salary", full.names = TRUE) %>% 
  read_csv(locale = locale("en"))
 
# Clean occupation up a bit.  Some rouge unicodes in there.
tidy_gap <- income_data %>% 
  mutate(occupation = iconv(occupation, "UTF-8", "UTF-8",sub='')) %>% 
  spread(gender, average_taxable_income) %>%
  set_names(tolower(names(.))) %>% 
  group_by(occupation) %>% 
  summarize_at(c("female", "male"), sum, na.rm = TRUE) %>% 
  filter(female != 0, male != 0) %>% 
  mutate(income_gap = male/female)

plot_data <- tidy_gap %>% 
 mutate(fill = if_else(income_gap >= 1, "grey80", "#ffd700"),
         alpha = if_else(income_gap >= 1, 0.2, 1)) %>% 
  mutate(tooltip = pmap(., make_tooltip)) %>% 
  mutate(tooltip = gsub("\\\n", "", tooltip)) %>% 
  mutate(tooltip = gsub("'", "", tooltip)) %>% 
  mutate(idx = row_number())

tooltip_css <- "background-color:white;padding:10px;border-radius:20px 20px 20px 20px;border-color:black;border-style:solid;border-width:1px"

plot <- ggplot(plot_data, aes(x = female, y = male, fill = fill)) +
  geom_segment(x = 0, xend = 600000, y = 0, yend = 600000, size = 0.05, color = "grey80") +
  geom_point_interactive(aes(alpha = alpha, tooltip = tooltip, data_id = idx), shape = 21, color = "grey30", size = 3) +
  scale_y_continuous(labels = dollar, limits = c(0, 600000)) +
  scale_x_continuous(labels = dollar, limits = c(0, 600000)) +
  scale_fill_identity() +
  scale_alpha_identity() +
  labs(x = NULL, 
       y = NULL, 
       title = "Gender Differences in Taxble Income in Australia",
       subtitle = str_wrap("Average male taxable income plotted against average female taxable income by occupation. Yellow dots indicate occupations where women have more taxable income than their male counterparts, 
       line indicates income equality. Hover over points for occupation, % difference and detailed income.", 100),
       caption = "Data: data.gov.au | Graphic: @jakekaupp") +
  theme_jk()

ggiraph(ggobj = plot, width_svg = 9, width = 1, tooltip_extra_css = tooltip_css)

waffle_data <- tidy_gap %>% 
  ungroup() %>% 
  mutate(category = case_when(income_gap > 1 ~ "Men have more income",
                              income_gap < 1 ~ "Women have more income"))%>% 
  count(category) %>% 
  pull(n) %>% 
  set_names(., c("Men have more income", "Women have more income"))

waffle(waffle_data, 
       rows = 14,
       size = 1,
       colors = c("dodgerblue3", "deeppink"), 
       legend_pos = "bottom", 
       title = "Out of 1092 occupations on record, men have more taxable income than women in 1011 of them.  That's 92.5% of occupations for those counting at home.") + 
  theme_jk() +
  labs(caption = "Data: data.gov.au | Graphic: @jakekaupp") +
  theme(axis.text = element_blank(),
        legend.position = "bottom")
