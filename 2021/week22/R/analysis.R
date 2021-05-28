library(tidyverse)
library(rvest)
library(janitor)
library(glue)
library(parsedate)
library(lubridate)
library(ggtext)
library(here)
library(fs)

scraper <- function(url) {
  
  read_html(url) %>% 
    html_table() %>% 
    last() %>% 
    clean_names() %>% 
    mutate(across(duration, as.numeric)) %>% 
    replace_na(list(duration = 1))
  
}

tracks <- c("Luigi Raceway", "Moo Moo Farm", "Koopa Troopa Beach", "Kalimari Desert", "Toad's Turnpike", "Frappe Snowland", "Choco Mountain", "Mario Raceway", "Wario Stadium", "Sherbet Land", "Royal Raceway", "Bowser's Castle", "D.K.'s Jungle Parkway", "Yoshi Valley", "Banshee Boardwalk", "Rainbow Road")

scaffold <- tibble(base_url = "https://mkwrs.com/mk64/display.php?track=",
                   tracks = tracks,
                   track_slug = str_replace_all(tracks, "\\s", "+"),
                   url = glue("{base_url}{track_slug}"))

track_wr <- scaffold %>% 
  mutate(three_lap = map(url, scraper),
         fast_lap = map(glue("{url}&f=1"), scraper)) %>% 
  pivot_longer(three_lap:fast_lap, names_to = "type", values_to = "value") %>% 
  select(tracks, type, value) %>% 
  unnest(c(value)) %>% 
  mutate(date_parsed = parse_date(date))

bands <- tibble(tracks = tracks) %>% 
  mutate(type = list(c("three_lap", "fast_lap")),
         start = as.POSIXct(ymd("1997-01-01")),
         end = as.POSIXct(ymd("2021-05-31"))) %>% 
  unnest(c(type)) %>% 
  unite("tracks", tracks, type, remove = FALSE)

mr <- track_wr %>% 
  filter(player == "MR") %>% 
  mutate(start = date_parsed,
         end = date_parsed + days(duration)) %>% 
  arrange(tracks, type) %>% 
  unite("tracks", tracks, type) 

close <- tibble(x = as.POSIXct(ymd(c("2015-11-20", "2015-11-21", "2015-11-23"))))

images <- here("2021", "week22", "img") %>% 
  dir_ls() 
 

ggplot(mr) +
  geom_segment(data = bands, aes(x = start, xend = end, y = tracks, yend = tracks), size = 8, color = "grey80") +
  geom_segment(aes(x = start, xend = end, y = tracks, yend = tracks), size = 8, color = "#145C9E") +
  geom_vline(data = close, aes(xintercept = x), color = "#D11149", size = 1) +
  geom_text(data = bands, aes(x = as.POSIXct(ymd("1997-02-05")), y = tracks, label = str_to_title(str_replace(type, "_", " "))), hjust = 0, family = "Anton") +
  labs(x = NULL,
       y = "<span style = 'font-family:Anton;font-size:30pt'>MR's Quest for World Record Perfection</span><br><br>
      <span style = 'font-family:Oswald;font-size:18pt'>Since the game was released players have always tried to claim the title of the fastest.  With the rise of speedrunning world records were tracked and followed breathlessly by the community.
      <br><br>One of the most exciting times in Mario Kart 64 speedrunning was during Matthias Rustemeyer's (MR) drive to get all 32 world records in the non-shortcut category for Mario Kart 64.  The complete story is epically recounted in Summoning Salt's 'Quest for World Record Perfection' (youtu.be/D6cpa-TvKn8).
      <br><br>The abridged version was a battle between MR and the A1A, the Anti-1.00 Alliance, a group of speedrunners acting as a foil to MR and trying to keep him from holding all records.  Ultimately, MR only ended up holding 31 of 32 records over 3 separate days in November of 2015.  This was the closest anyone has come to holding all 32 records.
      <br><br>Shown on the right is a temporal bar chart showing the spans of time that <span style = 'font-family:Oswald;font-size:18pt;color:#145C9E'><b>MR held the record</b></span> for each track and lap count.  The <span style = 'font-family:Oswald;font-size:18pt;color:#D11149'><b>vertical line</b></span>when he reached 31 out of 32 records.  MR is still active and speedrunning Mario Kart 64 splitting the 32 records with  one of the last active members of the A1A, Daniel Burbank (Dan).</span>
      <br><br><br><br><span style = 'font-family:Lato;font-size:12pt'>**Data**:mkwrs.com | **Graphic**: @jakekaupp</span>") +
  scale_x_datetime(expand = c(0,0)) +
  scale_y_discrete(labels = c(rbind(rep_len("", length(tracks)), glue("<img src='{images}' width='60' />")))) +
  theme_jk() +
  theme(axis.text.y = element_markdown(vjust = 0.8),
        axis.title.y = element_textbox_simple(width = unit(3.5, 'in'), 
                                              vjust = 1,  
                                              padding = margin(4, 4, 4, 4),
                                              margin = margin(4, 0, 0, 0))) +
  ggsave(here("2021", "week22", "tw22_plot.png"), width = 16, height = 12)

alt_text <- "This is a graphic showing a horizontal temporal bar chart by track and lap count illustrating the spans of time that Matthias Rustemeyer (MR) has held the non-shortcut world records for Mario Kart 64.  MR's spans are shown in blue, with the others shown as grey.  There are 32 bars, 16 tracks and two lap counts of 3 lap and 1 lap.
 A vertical red line shows the time in November 2015 that he almost reached his goal and held 31 out of 32 records.  For a detailed history of this story, check out Summoning Salt's excellent video at https://youtu.be/D6cpa-TvKn8.  The data was from Mario Kart World Records at kwrs.com and the graphic is by Jake Kaupp"
