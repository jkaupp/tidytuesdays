library(tidyverse)
library(lubridate)
library(here)
library(jkmisc)


# week-of-month function
wom <- function(date) { 
  first <- wday(as.Date(paste(year(date),month(date),1,sep="-")))
  return((mday(date)+(first-2)) %/% 7+1)
}

# Get the TidyTuesday Tweets Data
tt_tweet_data <- readRDS(here("2019", "week1", "data", "tidytuesday_tweets.rds"))

# Get the R tweet data 
r_tweet_data <- readRDS(here("2019", "week1", "data", "rstats_tweets.rds"))

# Most that tweet about R tweet about the r4ds tidy tuesday.
no_rstats <- anti_join(tt_tweet_data, r_tweet_data, by = "screen_name") %>% 
  mutate(rstats_tag = case_when(grepl("rstats", text, ignore.case = TRUE) ~ TRUE,
                                grepl("r4ds", text, ignore.case = TRUE) ~ TRUE,
                                grepl("visualization", text, ignore.case = TRUE) ~ TRUE,
                                grepl("data", text, ignore.case = TRUE) ~ TRUE,
                                grepl("code", text, ignore.case = TRUE) ~ TRUE,
                                grepl("plot", text, ignore.case = TRUE) ~ TRUE,
                                grepl("chart", text, ignore.case = TRUE) ~ TRUE,
                                grepl("graph", text, ignore.case = TRUE) ~ TRUE,
                                grepl("drob", text, ignore.case = TRUE) ~ TRUE,
                                grepl("ggplot", text, ignore.case = TRUE) ~ TRUE,
                                grepl("rstudio", text, ignore.case = TRUE) ~ TRUE,
                                grepl("model", text, ignore.case = TRUE) ~ TRUE,
                                grepl("median", text, ignore.case = TRUE) ~ TRUE,
                                grepl("average", text, ignore.case = TRUE) ~ TRUE,
                                grepl("week \\d+", text, ignore.case = TRUE) ~ TRUE,
                                grepl("@thomas_mock", text, ignore.case = TRUE) ~ TRUE,
                                TRUE ~ FALSE)) %>% 
  mutate(rstats_tag = case_when(screen_name %in% c("NosyOwl", "sebastianhwells", "JenniferCai7", "matthwong",
                                                   "scrite_jones", "jrosenblum123", "zlipp") ~ TRUE,
                                TRUE ~ rstats_tag)) %>% 
  filter(rstats_tag == FALSE)
  

plot_data <- anti_join(tt_tweet_data, no_rstats, by = "screen_name") %>% 
  mutate(created_at = as_date(created_at)) %>% 
  mutate(day = wday(created_at, label = TRUE, abbr = FALSE),
         week = wom(created_at),
         iweek = isoweek(created_at),
         month = month(created_at, label =  TRUE, abbr = FALSE),
         year = year(created_at))


count(plot_data, day, iweek) %>% 
  complete(day, iweek = 1:52, fill = list(n = NA)) %>% 
  ggplot(aes(x = iweek, y = day, fill = n)) +
  geom_tile(color = "white", size = 0.1) +
  scale_fill_viridis_c("Tweet Frequency", option = "cividis", na.value = "grey95", labels = seq(0,25,5), breaks = seq(0,25,5), limits = c(0,25)) +
  coord_equal() +
  labs(title = "Tidy Tuesday or Tardy Tuesday?",
       subtitle = "A glance at when the community decides to submit their work.",
       y = NULL,
       x = "Week of the Year",
       caption = "Data: rtweet | Analysis: @jakekaupp") +
  scale_x_continuous(limits = c(1, 53), breaks = c(1,10,20,30,40,50), expand = c(0, 0)) +
  theme_jk(grid = FALSE, ticks = FALSE) +
  theme(legend.position = c(0.5,-0.7),
        legend.direction = "horizontal",
        legend.title = element_text(family = "Scope One", vjust = 0.8))

ggsave(here("2019", "week1","tidy_or_tardy.png"), width = 8, height = 4)
