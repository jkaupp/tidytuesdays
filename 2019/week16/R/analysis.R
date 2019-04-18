library(tidyverse)
library(here) 
library(jkmisc)
library(ggalt)
library(grid)
library(Cairo)

dogs <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-16/dogs.csv")

png(file = here("2019", "week16", "tw16_plot.png"), width = 4, height = 4, units = "in", res = 300, type = "cairo")
                       
ggplot(dogs, aes(x = avg_weight, y = avg_neck)) +
  geom_xspline(size = 1) +
  geom_point(shape = 21, fill = "black", color = "white", stroke = 0.5, size = 2) +
  geom_text(data = filter(dogs, year == min(year)), aes(label = year), hjust = 0, nudge_x = 0.1, nudge_y = 0.01, family = "Oswald", size = 3) +
  geom_text(data = filter(dogs, year == max(year)), aes(label = year), hjust = 1, nudge_x = -0.1, family = "Oswald", size = 3) +
  annotate("segment", arrow = arrow(length = unit(0.2, "cm"), type = "closed"), x = 20.48, xend = 20.2, y = 44.3, yend = 44.03) +
  scale_y_continuous(limits = c(42, 45), breaks = 42:45) +
  expand_limits(x = c(17.5, 21)) +
  labs(title = "Fit as a butcher's dog",
       subtitle = "Characteristics of dogs registered with the UK's\nKennel Club, average when fully grown",
       x = bquote("Weight*, kg"),
       y = NULL,
       caption = "Sources: Kennel Club;\n The Economist ") +
  theme_jk(grid = "XY") +
  theme(plot.caption = element_text(hjust = -0.1))

grid.text(expression(paste(Neck~size, ", ", cm^"\u2020")), x = 0.1, y = 0.78, gp = gpar(fontfamily = "Oswald", cex = 0.8))
grid.text(bquote("* Where at leat 50 are registered per year"), x = 0.98, y = 0.075, gp = gpar(fontfamily = "Scope One", cex = 0.8), hjust = 1)
grid.text(expression("\u2020"~Where~at~least~100~are~registered~per~year), x = 0.98, y = 0.040, gp = gpar(fontfamily = "Scope One", cex = 0.8), hjust = 1)

dev.off()
