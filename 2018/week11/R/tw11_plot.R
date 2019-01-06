library(here)
library(tidyverse)
library(treemap)
library(sysfonts)
library(showtext)
library(grid)
library(nord)

raw_data <- read_csv("https://github.com/rfordatascience/tidytuesday/raw/master/data/week11_fifa_audience.csv") %>% 
  select(-X1)

font_add_google("Oswald","Oswald-Light", regular.wt = 300)
font_add_google("Scope One","Scope One")

showtext_auto()

vplayout <- function(x, y) viewport(width=11/3, height=8.5, layout.pos.row = x, layout.pos.col = y)

build_treemap <- function(x, y, size)  {
  
  title <- set_names(c("Population Share", "TV Audience Share", "GDP Weighted Share"), c("population_share","tv_audience_share", "gdp_weighted_share"))
  
  treemap(raw_data,
          index = c("confederation","country"),
          vSize = size,
          vColor = "confederation",
          type = "categorical",
          title = title[size],
          title.legend = "",
          fontfamily.title = "Oswald-Light",
          fontsize.labels = c(20, 10),
          fontfamily.labels = "Oswald-Light",
          fontcolor.labels = "#f0f0f0",
          lowerbound.cex.labels = 1,
          bg.labels = 0,
          inflate.labels = FALSE,
          border.col = "white",
          border.lwds = 1,
          position.legend = "none",
          palette = nord("baie_mouton"),
          align.labels = list(c("left","top"), c("right","bottom")),
          drop.unused.levels = TRUE,
          vp = vplayout(x,y))
  
  
  
}

fifa_maps <- function() {
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(3, 3, heights = c(0.1, 0.8, 0.1))))
  par(mai=c(0,0,0,0))
  
  grid.text("Comparing FIFA Share Differences by Confederation and Country", x = 0.1, hjust = 0, vp = vplayout(1,1), gp = gpar(fontfamily = "Oswald-Light", fontsize = 30))
  build_treemap(2, 1, "population_share")
  build_treemap(2, 2, "tv_audience_share")
  build_treemap(2, 3, "gdp_weighted_share")
  grid.text("Data: fivethirtyeight.com | Graphic: @jakekaupp", x = 0.5, vp = vplayout(3,3), gp = gpar(fontfamily = "Scope One", fontsize = 10))
  
}

png(here("week11", "Fifa Treemaps.png"), width = 11, height=8.5, units = "in", res = 100)
fifa_maps()
dev.off()

