
pack_bars <- function(data, number_rows, value_column, fill_color = "#4B384C", border_color = "white") {

value_column <- ensym(value_column)  
  
color_bar_data <- data %>% 
  top_n(number_rows, !!value_column) %>% 
  arrange(desc(!!value_column))

# calc row height based on num rows
bar_h = 1/number_rows

color_bars <- color_bar_data %>% 
  mutate(fill = fill_color,
         color = border_color,
         xmin = 0,
         xmax = !!value_column,
         ymin = map_dbl(1:number_rows, ~1 - bar_h*(.x-1)),
         ymax = map_dbl(1:number_rows, ~1 - bar_h*.x))

gray_bar_data <- data %>% 
  anti_join(color_bar_data) %>% 
  arrange(desc(!!value_column))

#get max x level for each bar level
row_sums <- pull(color_bar_data, !!value_column)

#gen gray ramp function
gray_gen <- colorRampPalette(c("#E8E8E8", "#cccccc"))

#gen gray ramp
grays <- gray_gen(105)
low_grays <- grays[1:50]
hi_grays  <- grays[56:105]

last_gray <- sample(c(low_grays, hi_grays), number_rows, replace = TRUE)

gray_bar_list <- vector('list', nrow(gray_bar_data))


for (i in 1:nrow(gray_bar_data)) {
  
  row <- gray_bar_data[i,]
  
  # Determine placing of each block by looking at the minium starting values of colored bars
  # adding on the new block and setting value to represent the new block length
  vert_pos <- which.min(row_sums + pull(row, !!value_column))
  
  # Assign alternating random grays to fill
  if (i == 1) {
    
    gray_fill <- sample(low_grays, 1)
    
  } else {
    
    last_gray <- last_gray[vert_pos]
    
    gray_fill <- ifelse(last_gray %in% low_grays, sample(hi_grays, 1), sample(low_grays, 1))
  }
  
  last_gray[vert_pos] <- gray_fill
  
  # Generate aes for geom_rect
  gray_bar_list[[i]]  <- mutate(row, 
                                fill = gray_fill,
                                color = border_color,
                                xmin = row_sums[[vert_pos]],
                                xmax = row_sums[[vert_pos]] + !!value_column,
                                ymin = map_dbl(1:number_rows, ~1 - bar_h*(.x-1))[[vert_pos]],
                                ymax = map_dbl(1:number_rows, ~1 - bar_h*.x)[[vert_pos]]
  )
  
  # Assign the new color_bar + rectangle as the max value for that row 
  row_sums[[vert_pos]] <- gray_bar_list[[i]]$xmax
  
}

gray_bars <- bind_rows(gray_bar_list)

bind_rows(color_bars, gray_bars)

}


create_wc <- function(iso2, data) {
  
  cntry_mask <- png::readPNG(here("2019", "week22", "data", "png maps", iso2, "1024.png"))
  
  ggplot(data, aes(label = word, size = n)) +
    geom_text_wordcloud(family = "Oswald", mask = cntry_mask, rm_outside = TRUE) +
    scale_radius(range = c(0, 40)) +
    theme_jk() 
  
  
}
