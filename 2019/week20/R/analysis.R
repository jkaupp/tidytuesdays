library(tidyverse)
library(here)
library(fs)
library(rcrossref)
library(ggbeeswarm)
library(jkmisc)


# Not re-downloading things, the citation count pulls take 2hrs.
if (length(dir_ls(here("2019", "week20", "data"))) <= 0) {
 
  nobel_winners <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winners.csv")
  nobel_winners_all_pubs <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winner_all_pubs.csv")
  
  saveRDS(nobel_winners, here("2019", "week20", "data", "nobel_winners.RDS"))
  
  saveRDS(nobel_winners_all_pubs, here("2019", "week20", "data", "nobel_winners_all_pubs.RDS"))

  
} else {
  
  nobel_winners <- dir_ls(here("2019", "week20", "data"), regexp = "nobel_winners.RDS") %>% 
    readRDS()
  
  nobel_winners_all_pubs <- dir_ls(here("2019", "week20", "data"), regexp = "nobel_winners_all_pubs.RDS") %>% 
    readRDS()
 
}


if (length(dir_ls(here("2019", "week20", "data"), regexp = "cite_count")) <= 0) {

  dois <- nobel_winners_all_pubs$doi

  list  <- split(dois, rep(1:ceiling(length(dois)/50), each=50)[1:length(dois)])

wait_cr_citation_count <- function(doi, index, list_len) {
  
  print(sprintf("%s complete", scales::percent(index/list_len)))
  
  Sys.sleep(1)
  
  cr_citation_count(doi)
  
}

cite_count <- imap_dfr(list, ~wait_cr_citation_count(.x, .y, length = length(list)))

saveRDS(cite_count, here("2019", "week20", "data", "cite_count.RDS")) 

} else {
  
cite_count <- readRDS(here("2019", "week20", "data", "cite_count.RDS"))
  
}

highlights <- c("einstein, a", "hill, av", "heeger, a")

plot_data <- nobel_winners_all_pubs %>%
  left_join(cite_count) %>%
  distinct(laureate_id, paper_id, .keep_all = TRUE) %>%
  select(pub_year, laureate_name, is_prize_winning_paper, count, category) %>% 
  replace_na(list(count = 0)) %>% 
  group_by(laureate_name, pub_year, category) %>%
  summarize(count = sum(count)) %>% 
  group_by(laureate_name) %>% 
  mutate(rolling_sum = cumsum(count)) %>% 
  mutate(color = if_else(laureate_name %in% highlights, "#F24534", "#21344F"),
         alpha = if_else(laureate_name %in% highlights, 1, 0.2))


everyone <- filter(plot_data, laureate_name %notin% highlights)

focus <- filter(plot_data, laureate_name %in% highlights) %>% 
  ungroup() %>% 
  mutate(laureate_name = case_when(laureate_name == "einstein, a" ~ "Einstein, A",
                                   laureate_name == "hill, av" ~ "Hill, AV",
                                   laureate_name == "heeger, a" ~ "Heeger, A")) %>% 
  group_by(laureate_name)


plot <- ggplot(plot_data, aes(x = pub_year, y = rolling_sum, group = laureate_name)) +
  geom_step(aes(color = color, alpha = alpha)) +
  geom_step(data = focus, aes(color = color, alpha = alpha)) +
  geom_text(data = filter(focus, pub_year == last(pub_year)), aes(color = color, alpha = alpha, label = laureate_name), x = 2018, family = "Oswald", hjust = 0) +
  scale_x_continuous(limits = c(1900, 2100), breaks = c(1900, 1925, 1950, 1975, 2000, 2018)) +
  scale_y_continuous(breaks = scales::pretty_breaks(), labels = scales::number) +
  scale_color_identity() +
  scale_alpha_identity() +
  facet_wrap(~category, labeller = as_labeller(str_to_title)) +
  labs(x = NULL,
       y = NULL,
       title = "Growth Patterns in How Often Nobel Prize Winning Researchers Are Cited",
       subtitle = str_wrap("Cummulative citation count by year (1900-2018).  Highlighted are A. Heeger (conductive polymers), A.V. Hill (heat and work in muscle) and A. Einstein (photoelecric effect). Each exhibit different citation patterns, likely attributed to the continued relevance and impact of their work.", 150),
       caption = "Data: Li, Jichao; Yin, Yian; Fortunato, Santo; Wang Dashun, 2018, 'A dataset of publication records for Nobel laureates', https://doi.org/10.7910/DVN/6NJ5RN, Harvard Dataverse. | Graphic: @jakekaupp") +
  theme_jk(grid = "Y") +
  theme(legend.position = "bottom") 

ggsave(here("2019", "week20", "tw20_plot.png"), plot, width = 12, height = 6)

