library(tidyverse)
library(tuber)
library(jkmisc)
library(lubridate)
library(here)
library(glue)


yt_oauth(app_id = Sys.getenv("TUBER_ID"), app_secret = Sys.getenv("TUBER_SECRET"))

marbles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-02/marbles.csv')

yt_marbles <- marbles %>%
  distinct(date, race, site, source) %>%
  transmute(id = str_remove(source, "\\?t=\\d+"),
         id = str_remove(id, "https://youtu.be/"),
         color = "highlight",
         alpha = 1,
         size = 1) 


video_stats <- get_all_channel_video_stats(channel_id = "UCYJdpnjuSWVOLgGT9fIzL0g")

plot_data <- video_stats %>% 
  mutate(publication_date = ymd_hms(publication_date)) %>% 
  mutate(across(contains("count"), as.numeric)) %>% 
  mutate(popularity = likeCount/viewCount,
         engagement = commentCount/viewCount) %>% 
  pivot_longer(names_to = 'measure', values_to = 'value', popularity:engagement) %>% 
  arrange(publication_date) %>% 
  left_join(yt_marbles) %>% 
  replace_na(list(color = "background",
                  alpha = 0.2,
                  size = 0.5))


plot <- ggplot(plot_data, aes(x = publication_date, y = value)) +
  geom_line(aes(color = measure, alpha = alpha, size = size)) +
  geom_point(aes(color = measure, alpha = alpha)) +
  geom_text(data = filter(plot_data, publication_date == last(publication_date)), aes(label = spaced_title(measure), color = measure), hjust = -0.2, family = "Lato", fontface = "bold") +
  labs(x = NULL,
       y = NULL,
       title = glue("Jelle's Marble Runs Bulding {highlight_text('Popularity', '#3CA8D8', 'b', 26)} and {highlight_text('Engagment', '#FAC500', 'b', 26)} Through It's Unique Content"),
       subtitle = "Illustrated below are rudimentary analytics for Jelle's Marble Runs YouTube channel.  Popularity is defined as the ratio between likes and views, and engagement is the ratio of comments to<br>views.  The inaugural season of Marbula One has been very benefial to the channel, boosting both metrics and increasing subscribership.",
       caption = "**Data**: @randal_olson | **Graphic**: @jakekaupp") +
  scale_y_continuous(labels = scales::label_percent()) +
  scale_x_datetime(limits = c(min(plot_data$publication_date), as.POSIXct("2020-9-01 00:00:00 CET"))) +
  scale_alpha_identity() +
  scale_size_identity() +
  scale_color_manual(values = c("popularity" = "#3CA8D8",
                                "engagement" = "#FAC500")) +
  annotate("text", x = as.POSIXct("2020-03-15 00:00:00 CET"), y = 0.015, label = spaced_title("Marbula One\nInagurual Season"), family = "Lato", fontface = "bold") +
  theme_jk(grid = "XY",
           markdown = TRUE) +
  theme(legend.position = "none",
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.background = element_rect(fill = '#F2FDFF'))

ggsave(here("2020", "week23", "tw23_plot.png"), plot, width = 14, height = 8, device = ragg::agg_png()) 
