share_packed_circle <- function(df) {
 
  nodes <- make_nodes(df)
  
  edges <- make_edges(df)
 
  mygraph <- tbl_graph(nodes = nodes, edges = edges)
  
  # Make the plot
  plot <- ggraph(mygraph, layout = 'circlepack', weight = "size") + 
    geom_node_circle(aes(fill = depth)) +
    theme_void() +
    labs(title = unique(df$year)) +
    coord_equal() +
    scale_fill_nord("lumina", discrete = FALSE, reverse = TRUE) +
    #scale_fill_viridis(option = "plasma") +
    theme(legend.position = "none", 
          plot.background = element_rect(fill = "#4C566A"),
          plot.title = element_text(family = "Oswald", hjust = 0.5, color = "white"),
          )
  
  return(plot)
}


make_nodes <- function(df) {
  
  size <- df %>% 
    group_by(title, genres, year) %>% 
    summarize(share = mean(share)) %>% 
    distinct(genres, title, share) %>% 
    rename(name = title, size = share)
  
  genre_size <- size %>% 
    group_by(genres) %>% 
    summarize(size = sum(size)) %>% 
    rename(name = genres)
  
  title_size <- size %>% 
    ungroup() %>% 
    distinct(name, size) %>% 
    mutate(size = size)
  
  total_size <- df %>% 
    distinct(title, share) %>% 
    summarize(name = as.character(unique(df$year)),
              size = sum(share))
  
  sizes <- bind_rows(genre_size, title_size, total_size)
  
  nodes <- df %>% 
    group_by(title, genres, year) %>% 
    summarize(share = mean(share)) %>% 
    gather(variable, name, title, genres, year) %>% 
    arrange(variable, name) %>% 
    distinct(name) %>% 
    left_join(sizes, by = "name") %>% 
    mutate(size = if_else(size == 0, 0.001, size)) %>% 
    arrange(size)
  
  return(nodes)
  
  
}

make_edges <- function(df) {
  
  base <- tibble(from = as.character(unique(df$year)), to = unique(df$genres))
  
  inner <- df %>% 
    select(from = genres, to = title) %>% 
    distinct() 
  
  edges <- bind_rows(base, inner) 
  
  return(edges)
  
}
