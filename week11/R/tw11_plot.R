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
          fontsize.labels = c(20, 10),
          fontfamily.labels = "Oswald-Light",
          fontcolor.labels = "#f0f0f0",
          lowerbound.cex.labels = 1,
          bg.labels = 0,
          inflate.labels = FALSE,
          border.col = "white",
          border.lwds = 1,
          position.legend = "none",
          palette = c("#ffd700",
          "#fa8775",
          "#ea5f94",
          "#cd34b5",
          "#9d02d7",
          "#0000ff"),
          align.labels = list(c("left","top"), c("right","bottom")),
          drop.unused.levels = TRUE,
          vp = vplayout(x,y))
  
  
  
}

fifa_maps <- function() {
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(1, 3)))
  par(mai=c(0,0,0,0))
  
  build_treemap(1, 1, "population_share")
  build_treemap(1, 2, "tv_audience_share")
  build_treemap(1, 3, "gdp_weighted_share")
  
}

png(here("week11", "Fifa Treemaps.png"), width = 11, height=8.5, units = "in", res = 100)
fifa_maps()
dev.off()


