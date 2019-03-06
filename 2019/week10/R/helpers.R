# Functions from an old friend.
# https://github.com/jkeirstead/r-slopegraph/blob/master/slopegraph.r

build_slopegraph <- function(df, x, y, group, min.space=0.05) {
  
  ## First rename the columns for consistency
  ids <- match(c(x, y, group), names(df))
  
  df <- df[,ids]
  
  names(df) <- c("x", "y", "group")
  
  ## Expand grid to ensure every combination has a defined value
  tmp <- expand.grid(x=unique(df$x), group=unique(df$group))
  
  tmp <- merge(df, tmp, all.y=TRUE)
  
  df <- mutate(tmp, y=ifelse(is.na(y), 0, y))
  
  spaced_sort(df, min.space=min.space)
  
}



spaced_sort <- function(df, min.space=0.05) {
  ## Define a minimum spacing (5% of full data range)
  min.space <- min.space*diff(range(df$y))
  
  ## Transform the data
  
  df <- split(df, "x") %>% 
    map_df(~calc_spaced_offset(.x, min.space))
  
  return(df)
}

##' Calculates the vertical offset between successive data points
##' 
##' @param df a data frame representing a single year of data
##' @param min.space the minimum spacing between y values
##' @return a data frame
calc_spaced_offset <- function(df, min.space) {
  
  ## Sort by value
  ord <- order(df$y, decreasing=T)
  ## Calculate the difference between adjacent values
  delta <- -1*diff(df$y[ord])
  ## Adjust to ensure that minimum space requirement is met 
  offset <- (min.space - delta)
  offset <- replace(offset, offset<0, 0)
  ## Add a trailing zero for the lowest value
  offset <- c(offset, 0)
  ## Calculate the offset needed to be added to each point
  ## as a cumulative sum of previous values
  offset <- rev(cumsum(rev(offset)))
  ## Assemble and return the new data frame
  df.new <- data.frame(group=df$group[ord],
                       x=df$x[ord],
                       y=df$y[ord],
                       ypos=offset+df$y[ord])
  return(df.new)
}
