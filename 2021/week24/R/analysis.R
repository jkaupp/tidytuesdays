library(tidyverse)
library(ggstream)
library(janitor)
library(ggtext)
library(colorspace)
library(here)

stocked <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-08/stocked.csv') %>% 
  clean_names()

lakes <- tibble(lake = unique(stocked$lake),
                label = spaced_title(c("Michigan", "Superior", "Ontario", "Eerie", "Huron", "SC"))) %>% 
  filter(lake != "SC")


colors <- tibble(species = unique(stocked$species)) %>% 
  mutate(color = if_else(species == "CHS", "#fa8072", sample(grey.colors(30), 18))) 

stocked %>% 
  filter(lake != "SC", between(year, 1968, 2020)) %>% 
  group_by(year, lake, species) %>% 
  summarize(total = sum(no_stocked)) %>% 
  left_join(colors) %>% 
  left_join(lakes) %>% 
  ggplot(aes(x = year, y = total)) +
  geom_stream(type = "proportional", sorting = "onset",aes(color = darken(color), fill = color)) +
  #geom_text(data = lakes, aes(x = 1968, y = 0, label = label), family = "Oswald", size = 10, hjust = 0, vjust = -0.1) +
  labs(x = NULL,
       y =  "<span style = 'font-family:Oswald;font-size:30pt'>The Delicate Balance to Maintain Great Lakes Fisheries</span><br><br>
      <span style = 'font-family:Bitter;font-size:14pt'>Shown on the right is a porportional area chart showing the fish stocks from 1968 to 2020. <span style = 'font-family:Bitter;font-size:14pt;color:#fa8072'><b>Chinnok Salmon</b></span> were introduced in 1966 to curtail an overabundance of Alewife and have seen a reduction in yearly stocks since 2003. This reduction is to help balance the predator/prey relationships in the Great Lakes and avoid throwing ecosystem into a downward sprial in the face of the increasing threats of invasive species. </span>
      <br><br><br><br><span style = 'font-family:Bitter;font-size:12pt'>**Data**:Great Lakes Fishery Commission<br>**Graphic**: @jakekaupp</span>") +
  scale_fill_identity() +
  scale_color_identity() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), labels = scale_percent_labels) +
  facet_wrap(~label, nrow = 1) +
  theme_jk(grid = FALSE, 
           base_family = "Bitter", 
           base_size = 14, 
           strip_text_family = "Oswald", 
           strip_text_size = 30) +
    theme(axis.title.y = element_textbox_simple(width = unit(3, 'in'), 
                                                vjust = 1,  
                                                padding = margin(0, 0, 0, 0),
                                                margin = margin(0, 0, 0, 0))) +
  ggsave(here("2021", "week24", "tw24_plot.png"), width = 16, height = 6)



alt_text <- "This is a graphic showing a porportional area chart of fish stocks in 18 species from 1968 to 2020. Chinnok salmon are highlighted in a pink color while the other 17 are different shades of grey. The chinnok salmon were introduced in 1966 to curtail an overabundance of Alewife and have seen a reduction in yearly stocks since 2003. This reduction is to help balance the predator/prey relationships in the Great Lakes and avoid throwing ecosystem into a downward sprial in the face of the increasing threats of invasive species. The data is from the Great Lakes Fishery Commission (http://www.glfc.org/great-lakes-databases.php) and the graphic is by Jake Kaupp"


