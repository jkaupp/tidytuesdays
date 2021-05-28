library(tidyverse)
library(fs)
library(here)
library(glue)
library(magick)

image_table <- tibble(path = dir_ls(here(), glob = "*.png", recurse = 2)) %>% 
  mutate(file = basename(path),
         year = as.numeric(str_extract(path, "[0-9]{4}(?=\\/)")),
         week = as.numeric(str_extract(path, "(?<=week)\\d{1,2}"))) %>% 
  arrange(year, week, file, path) %>% 
  group_by(year, week) %>% 
  mutate(idx = row_number()) %>% 
  ungroup() %>% 
  mutate(tag = glue("tt_{year}_{week}{ifelse(length(idx) > 1, glue('_{idx}'), '')}.png")) %>% 
  mutate(path = as.character(path)) 
