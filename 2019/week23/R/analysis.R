library(tidyverse)
library(glue)
library(rvest)
library(xml2)
library(lubridate)
library(here)
library(jkmisc)



#Scraping functions----

get_urls <- function(sitemap_url) {
  
  read_xml(sitemap_url) %>%
    xml_children() %>% 
    xml_children() %>% 
    xml_text() %>% 
    keep(~str_detect(.x, "https://www.theramenrater.com/[0-9]{4}/[0-9]{2}/[0-9]{1,}/\\w+"))
  
}

get_post_title <- function(url, idx, rows) {
  
  print(sprintf("Progress: %s/%s", idx, rows))
  
  read_html(url) %>% 
    html_node(".entry-title") %>% 
    html_text()
  
  
}

slowly_get_post_title <- slowly(~ get_post_title(.x, .y, rows), rate = rate_delay(pause = 0.5), quiet = TRUE)

#Week of month
wom <- function(date) { # week-of-month
  first <- wday(as.Date(paste(year(date), month(date), 1, sep="-")))
  return((mday(date) + (first - 2)) %/% 7 + 1)
}

#Plotting functions----
month_outline <- function(df) {
  
  top1 <- with(df, tibble(x = min(wmonth) - 0.5,
                          xend = wday[day == min(day)] - 0.5,
                          y = wmonth[day == min(day)] + 0.5,
                          yend = wmonth[day == min(day)] + 0.5,
                          line = "top1")) 
  
  top2 <- with(df, tibble(x = wday[day == min(day)] - 0.5,
                          xend = max(wday) + 0.5,
                          y = min(wmonth) - 0.5,
                          yend = min(wmonth) - 0.5,
                          line = "top2")) 
  
  left1 <- with(df, tibble(x = wday[day == min(day)] - 0.5,
                           xend = wday[day == min(day)] - 0.5,
                           y = wmonth[day == min(day)] + 0.5,
                           yend = min(wmonth) - 0.5,
                           line = "left1"))
  
  left2 <- with(df, tibble(x = min(wmonth) - 0.5,
                           xend = min(wmonth) - 0.5,
                           y = wmonth[day == min(day)] + 0.5,
                           yend = wmonth[day == max(day)] + 0.5,
                           line = "left2"))
  

  right1 <- with(df, tibble(x = max(wday) + 0.5,
                            xend = max(wday) + 0.5,
                            y = min(wmonth) - 0.5,
                            yend = wmonth[day == max(day)] - 0.5,
                            line = "right1"))
  
  right2 <- with(df, tibble(x = wday[day == max(day)] + 0.5,
                            xend = wday[day == max(day)] + 0.5,
                            y = wmonth[day == max(day)] - 0.5,
                            yend = wmonth[day == max(day)] + 0.5,
                            line = "right2"))

  
  bottom1 <- with(df, tibble(x = min(wmonth) - 0.5,
                             xend = wday[day == max(day)] + 0.5,
                             y = wmonth[day == max(day)] + 0.5,
                             yend = wmonth[day == max(day)] + 0.5,
                             line = "bottom1"))
  
  bottom2 <- with(df, tibble(x = wday[day == max(day)] + 0.5,
                             xend = max(wday) + 0.5,
                             y = wmonth[day == max(day)] - 0.5,
                             yend = wmonth[day == max(day)] -0.5,
                             line = "bottom2"))
  
  top <- bind_rows(top1, top2)
  left <- bind_rows(left1, left2)
  bottom <- bind_rows(bottom1, bottom2) 
  right <- bind_rows(right1, right2) 
    
    bind_rows(top, left, right, bottom) %>% 
      mutate(year = unique(df$year),
             month = unique(df$month)) 
    
  
}


if(!file.exists(here("2019","week23", "data", "full_ramen_data.RDS"))) {
  
  # Add Recent ratings #3181-319
  recent_ratings <- tibble(review_number = c(3181:3189, 1676, 2745, 2991),
                           stars = c(3.75, 3.25, 4.0, 3.25, 2.0, 3.75, 3.75, 3.5, 2.25, 4.25, 5, 3.25),
                           brand = c("Nissin Yakisoba", "Maruchan", "Uni-President", "Maruchan", "Sakruai Foods", "Nissin Mago", "Big Bon", "Sapporo Ichiban", "Canton", "A1", "Nissin", "Big Bon"),
                           variety = c("Instant Panict Savory Beef Flavour", "Maruchan Ramen Noodle Soup Roast Beef Flavour", "Imperial Big Meal Super Hot Pot Beef Flavour",
                                       "Ramen Noodle Soup Pork Beef Flavour", "Vegetarian Stir Fry Noodles", "Nissin Lamen Light Legumes ", "Spice Mix Piquant", "Momosan Ramen Tokyo Chicken", "Instant Noodles Spicy Tomato",
                                       "Emperor Herbs Chicken Noodle", "U.F.O. Big Wasabi-Mayo Yakisoba", "Chicken & Salsa Sauce Instant Noodles"),
                           country = c("Phillipines", "United States", "Taiwan", "United States", "Japan", "Brazil", "Russia" , "United States", "India", "Malaysia", "Japan", "Russia"),
                           style = c("Cup", "Pack", "Pack", "Pack", "Pack", "Pack", "Pack", "Pack", "Pack", "Pack", "Pack", "Pack"))
  
  # Read tidytesday data----
  ramen_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-04/ramen_ratings.csv") %>% 
    bind_rows(recent_ratings)
  
  out <- tibble(sitemap_url = glue("https://www.theramenrater.com/post-sitemap{1:5}.xml"),
                contents = map(sitemap_url, get_urls)) %>% 
    unnest() 
  
  rows <- nrow(out)
  
  # Scrapin der web purges----
  out <-  out %>% 
    mutate(title = imap(contents, ~slowly_get_post_title(.x, .y, rows))) %>% 
    mutate(date = parse_date(str_extract(contents, "[0-9]{4}/[0-9]{2}/[0-9]{1,}"), "%Y/%m/%d"),
           review_number = as.numeric(str_extract(title, "(?!#)[0-9]{1,4}(?=\\:)"))) %>% 
    filter(!is.na(review_number)) %>% 
    left_join(ramen_ratings) %>% 
    filter(review_number < 4000) %>% 
    select(-sitemap_url)
  
  # Saving the new dataset----
  saveRDS(out, here("2019","week23", "data", "full_ramen_data.RDS"))
  
} else {
  
  ramen_data <- readRDS(here("2019","week23", "data", "full_ramen_data.RDS"))
  
}

#Make months
all_dates <- tibble(date = seq.Date(from = ymd("2009/01/01"), to =ymd("2019/12/31"), by ="day")) %>% 
  mutate(day = day(date),
         month = month(date),
         year = year(date))

plot_data <- ramen_data %>%
  mutate(day = day(date),
         month = month(date),
         year = year(date)) %>% 
  group_by(year, month, day) %>% 
  summarize(brands = toString(sprintf("%s: %s", brand, variety)),
            count = n(),
            avg_stars = mean(stars)) %>% 
  right_join(all_dates) %>% 
  ungroup() %>% 
  mutate(wday = wday(date, label = TRUE, week_start = 7),
         month = month(date,label = TRUE),
         wmonth = wom(date),
         week = week(date))
  
outlines <- all_dates %>% 
  mutate(wday_label = wday(date, label = TRUE),
         wday = wday(date),
         month = month(date,label = TRUE),
         wmonth = wom(date),
         week = week(date)) %>% 
  split(list(.$year, .$month), drop = TRUE) %>% 
  map_df(month_outline)



plot <- ggplot(data = plot_data, aes(x = wday, y = wmonth, fill = avg_stars)) +
  geom_tile(color = "grey80", size = 0.1) +
  geom_segment(data = outlines, aes(x = x, xend = xend, y = y, yend = yend, group = line), color = "grey30", inherit.aes = FALSE) +
  scale_y_continuous(trans = "reverse", labels = NULL) +
  scale_x_discrete(labels = NULL) +
  scale_fill_gradientn("Average Stars", colors = rev(parula(100)), na.value = "grey95") +
  facet_grid(month ~ year, switch = "y") +
  labs(x = NULL,
       y = NULL,
       title = "The Prolfic Nature of the Ramen Rater and a Birds-Eye View of Ramen Quality",
       subtitle = str_wrap("Below is a heatmap calendar of the all the Ramen Raters ramen ratings by the published date of the review.  In the early days, multiple reviews were posted in a single day, until reaching the usual pattern of a single review per day.  However, there are still some reviews that get posted en masse.", 100),
       caption = "Data: The Ramen Rater | Graphic: @jakekaupp") +
  theme_jk(grid = FALSE) +
  theme(strip.text.y = element_text(angle = 180),
        panel.spacing.y = unit(-0.2, "lines"),
        legend.position = "bottom")

ggsave(here("2019", "week23", "tw23_plot.png"), height = 11, width = 8.5, type = "cairo")





  
