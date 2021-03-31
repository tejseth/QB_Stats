library(dplyr)
library(na.tools)
library(ggimage)
library(nflfastR)
library(ggrepel)
library(ggimage)
library(ggtext)
library(mgcv)
library(scales)
library(ggforce)
library(gt)
library(remotes)
library(tidyverse)

seasons <- 2006:2020
pbp <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
    )
  )
})

pbp_rp <- pbp %>% 
  filter(!is_na(epa), play_type=="no_play" | play_type=="pass" | play_type=="run")

pbp_rp <- pbp_rp %>%
  mutate(
    pass = if_else(str_detect(desc, "( pass)|(sacked)|(scramble)"), 1, 0),
    rush = if_else(str_detect(desc, "(left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)") & pass == 0, 1, 0),
    success = ifelse(epa>0, 1 , 0)
  ) 

pbp_rp <- pbp_rp %>% filter(pass==1 | rush==1)

pbp_rp <- pbp_rp %>%
  mutate(season = substr(old_game_id, 1, 4))

pbp_rp <- pbp_rp %>%
  mutate(
    posteam = case_when(
      posteam == 'OAK' ~ 'LV',
      posteam == 'SD' ~ 'LAC',
      posteam == 'STL' ~ 'LA',
      TRUE ~ posteam
    )
  )

pbp_rp <- pbp_rp %>%
  mutate(
    defteam = case_when(
      defteam == 'OAK' ~ 'LV',
      defteam == 'SD' ~ 'LAC',
      defteam == 'STL' ~ 'LA',
      TRUE ~ defteam
    )
  )

run_epa <- pbp_rp %>%
  filter(qb_dropback == 0) %>%
  filter(qb_scramble == 0) %>%
  filter(rush_attempt==1) %>%
  group_by(posteam, season) %>%
  summarize(rush_epa = mean(epa, na.rm=T))

def_epa <- pbp_rp %>%
  group_by(defteam, season) %>%
  summarize(def_epa = mean(epa, na.rm = T))

names(run_epa)[names(run_epa) == "posteam"] <- "team"
names(def_epa)[names(def_epa) == "defteam"] <- "team"

help_epa <- merge(run_epa, def_epa, by = c("team", "season"))

help_epa <- help_epa %>%
  mutate(total_help = rush_epa - def_epa) %>%
  arrange(total_help) %>% 
  filter(season < 2021)