library(tidyverse)
library(colorspace)
library(ggtern)
library(scico)
library(grid)
library(gridtext)
library(jkmisc)
library(here)
library(ragg)

allCategories <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-30/allCategories.csv')
allShades <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-30/allShades.csv')
allNumbers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-30/allNumbers.csv')

#Ideas
# Colour distance similarities between brands
#

hcl_shades <- allShades %>% 
 mutate(rgb = map(hex, hex2RGB),
        hcl = map(rgb, ~as(.x, "polarLUV")),
        hcl = map(hcl, ~as_tibble(attr(.x, "coords")))) %>% 
  unnest(c(hcl))

avg_tone <- hcl_shades %>% 
  group_by(brand) %>% 
  summarize(across(c(H,C,L,hue, sat, lightness), mean)) %>% 
  rowwise() %>% 
  mutate(hsl = list(HLS(hue, lightness, sat))) %>% 
  mutate(hex = flatten_chr(list(hex(hsl))))

top <- hcl_shades %>% 
  add_count(H, C, L, sort = TRUE)

hex <- ggtern(hcl_shades, aes(x = H, y = C, z = L)) +
  geom_hex_tern(binwidth = 0.01, size = 0.1, color = "white", show.legend = FALSE) +
  scale_L_continuous(name = "H U E") +
  scale_T_continuous(name = "C H R O M A") +
  scale_R_continuous(name = "L U M I N A N C E") +
  labs(title = spaced_title("Color Frequency")) +
  scale_fill_scico(palette = "devon") +
  theme_jk() +
  theme(plot.background = element_rect(fill = '#f4f4f6', color = "#f4f4f6"),
        plot.title = element_text(hjust = 0.5))

dots <- ggtern(hcl_shades, aes(x = H, y = C, z = L)) +
  geom_point(aes(color = hex), show.legend = FALSE) +
  scale_color_identity() +
  scale_L_continuous(name = "H U E") +
  scale_T_continuous(name = "C H R O M A") +
  scale_R_continuous(name = "L U M I N A N C E") +
  labs(title = spaced_title("Color Distribution")) +
  theme_jk() +
  theme(plot.background = element_rect(fill = '#f4f4f6', color = "#f4f4f6"),
        plot.title = element_text(hjust = 0.5))

avg <- ggtern(avg_tone, aes(x = H, y = C, z = L)) +
  geom_point(aes(color = hex), show.legend = FALSE) +
  scale_color_identity() +
  scale_L_continuous(name = "H U E") +
  scale_T_continuous(name = "C H R O M A") +
  scale_R_continuous(name = "L U M I N A N C E") +
  labs(title = spaced_title("Average Brand Color")) +
  theme_jk() +
  theme(plot.background = element_rect(fill = '#f4f4f6', color = "#f4f4f6"),
        plot.title = element_text(hjust = 0.5))


png(filename = here("2021", "week14", "tw14_plot2.png"), width = 2000, height = 725, units = "px")

grid_draw(grobTree(rectGrob(gp=gpar(fill="#f4f4f6", lwd=0)), 
         grid.arrange(dots, hex, avg, 
                      nrow = 1, 
                      top = textGrob(label = "Three Views of Bias in Beauty",gp = gpar(fontsize = 28, fontfamily = "Oswald", fill = '#f4f4f6')),
                      bottom = richtext_grob(text = "**Data**: The Pudding | **Graphic**: @jakekaupp", gp = gpar(fontsize = 12, fontfamily = "Oswald", fill = '#f4f4f6'), hjust = -3.5)
                      )
         )
         )

dev.off()

alt_text <- "In this image are 3 separate triagnular ternary diagrams.  Each ternary digram plots hue, chroma and luminance on respective axes of the triangle.  The first is a plot of all makeup colors in the makeup dataset.  The second is a hexbin of those colors indicating higher frequency items, which are concentrated or biased towards lighter shades.  The third presents the average color for each make-up brand, which again bias towards ligther shades.  The data can be found at https://github.com/the-pudding/data/tree/master/foundation-names"


  