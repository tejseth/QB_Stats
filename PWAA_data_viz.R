pwaa_no_qbs <- main_pwaa %>%
  select(season, team, total_wins, pred_wins, pwaa)

games <- read_csv("http://www.habitatring.com/games.csv")

games <- games %>%
  mutate(
    home_team = case_when(
      home_team == 'OAK' ~ 'LV',
      home_team == 'SD' ~ 'LAC',
      home_team == 'STL' ~ 'LA',
      TRUE ~ home_team
    )
  )
games <- games %>%
  mutate(
    away_team = case_when(
      away_team == 'OAK' ~ 'LV',
      away_team == 'SD' ~ 'LAC',
      away_team == 'STL' ~ 'LA',
      TRUE ~ away_team
    )
  )

qbs <- games %>%
  filter(game_type == "REG") %>%
  select(game_id, season, week, away_qb_id, home_qb_id, 
         away_qb_name, home_qb_name, home_team, away_team) %>%
  filter(season >= 2006) 

away_qbs <- qbs %>%
  group_by(away_qb_name, away_qb_id, season, away_team) %>%
  summarize(away_games = n())
home_qbs <- qbs %>%
  group_by(home_qb_name, home_qb_id, season, home_team) %>%
  summarize(home_games = n())
all_qbs <- home_qbs %>%
  left_join(away_qbs, by = c("home_qb_name" = "away_qb_name", "season" = "season")) 
all_qbs <- all_qbs %>%
  mutate(games = home_games + away_games)
all_qbs <- all_qbs %>%
  select(home_qb_name, home_qb_id, season, games, home_team)
all_qbs <- all_qbs %>%
  filter(!is.na(games)) %>%
  arrange(desc(games))
colnames(all_qbs)[which(names(all_qbs) == "home_qb_name")] <- "passer"
colnames(all_qbs)[which(names(all_qbs) == "home_qb_id")] <- "passer_id"
colnames(all_qbs)[which(names(all_qbs) == "home_team")] <- "team"

top_qbs <- all_qbs %>%
  group_by(team, season) %>% slice_max(order_by = passer, n = 1)

pwaa <- pwaa_no_qbs %>%
  left_join(top_qbs, by = c("team", "season"))

pwaa_stats <- pwaa %>%
  group_by(passer, passer_id) %>%
  summarize(seasons = n(),
            mean_pwaa = mean(pwaa)) %>%
  arrange(desc(mean_pwaa))



  

