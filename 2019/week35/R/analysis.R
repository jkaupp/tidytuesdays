library(tidyverse)
library(tidygraph)
library(ggraph)
library(colorspace)
library(glue)
library(jkmisc)
library(ggtext)
library(ragg)
library(here)


html_string <- glue("Shown below is a co-occurance network of guest stars in The Simpsons, best explained as a 'Who co-stars together?'. {subtitle_names} are the most frequent guest stars in the series.")

str_break <- function (html_string, width = 80, indent = 0, exdent = 0) {

tags <- str_extract_all(html_string, "<.*?>") %>% 
  flatten_chr() 

index <- sprintf("tag_%s", seq_along(tags))

string <- str_replace_all(html_string, set_names(index, tags))

  if (width <= 0) 
    width <- 1
  
  out <- stringi::stri_wrap(string, width = width, indent = indent, 
                   exdent = exdent, simplify = FALSE)
  
  broken <- vapply(out, str_c, collapse = "<br>", character(1))
  
  str_replace_all(broken, set_names(tags, index))
  
}

text_bc <- function(text, color) {
  
  glue("<span style = color:{color}>**{text}**</span>")
  
}


simpsons <- read_delim("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-27/simpsons-guests.csv", delim = "|", quote = "")

nodes <- count(simpsons, guest_star) 

top5 <- top_n(nodes, 5, n) %>% 
  arrange(desc(n)) %>% 
  rownames_to_column(var = "color") %>% 
  select(-n)

nodes <- nodes %>% 
  left_join(top5) %>% 
  replace_na(list(color = 7)) %>% 
  mutate(guest_star = str_remove(guest_star, "'")) %>% 
  mutate(alpha = if_else(color < 7, 1, 0.5)) 


edges <- simpsons %>% 
  mutate(guest_star = str_remove(guest_star, "'")) %>% 
  group_by(production_code) %>% 
  mutate(co_stars = map(guest_star, ~str_subset(guest_star, .x, negate = TRUE))) %>%
  ungroup() %>% 
  mutate(co_stars = map(co_stars, ~ifelse(length(.x) == 0, NA_character_,.x))) %>% 
  unnest(co_stars) %>% 
  count(guest_star, co_stars) %>% 
  filter(!is.na(n), !is.na(co_stars)) %>% 
  set_names(c("from", "to", "n")) %>% 
  left_join(top5, by = c("from" = "guest_star")) %>% 
  left_join(top5, by = c("to" = "guest_star")) %>% 
  mutate(color = coalesce(color.x, color.y)) %>% 
  replace_na(list(color = "grey80")) 

  
colors <- set_names(tol6qualitative, top5$guest_star)  

subtitle_names <- imap(colors[1:5], ~text_bc(.y, .x)) %>% 
  glue_collapse(sep = ', ') %>% 
  glue(" and {imap(colors[6], ~text_bc(.y, .x))}")

co_star_graph <- tbl_graph(nodes = nodes, edges = edges, directed = FALSE)

co_star_plot <- co_star_graph %>% 
  activate(nodes) %>% 
  arrange(n) %>% 
  mutate(degree = centrality_degree()) %>% 
  filter(degree > 1) %>%  
  ggraph(layout = "fr") + 
  geom_edge_arc(edge_width = 0.5, curvature = 0.2, aes(alpha = stat(index), edge_colour = color)) +
  geom_node_point(aes(size = n, color = color, alpha = alpha, fill = color), shape = 21) +
  scale_color_manual(values = c(darken(tol6qualitative), "grey80")) + 
  scale_alpha_identity() +
  scale_edge_color_manual(values = c(tol6qualitative, "grey85")) +
  scale_fill_manual(values = c(tol6qualitative, "grey80")) + 
  scale_size(range = c(2,6)) +
  labs(x = NULL,
       y = NULL,
       title = "The Guest Star Backbone Of A Simpsons Co-Star Network",
       subtitle = glue("Shown below is a co-occurance network of guest stars in The Simpsons, best explained as a 'Who co-stars together?'.<br> {subtitle_names} <br>are the most frequent guest stars in the series."),
       caption = "**Data**: Wikipedia via @datawookie | **Graphic**: @jakekaupp") +
  theme_jk(grid = FALSE,
           subtitle_family = "Lora",
           caption_family = "Lora",
           markdown = TRUE) +
  theme(legend.position = "none",
        axis.text = element_blank())

ggsave(here("2019", "week35", "tw35_plot.png"), plot = co_star_plot, device = agg_png(), width = 9, height = 8)

ggplot(mtcars, aes(x = mpg, y = disp)) +
  geom_point() +
  labs(title = paste0(highlight_text("This is bold", style = "bi"), "This isn't"),
       subtitle = paste0(highlight_text("This is bold", style = "bi"), "This isn't")) +
  theme_jk() +
  theme(plot.title = ggtext::element_markdown(),
        plot.subtitle = ggtext::element_markdown())
