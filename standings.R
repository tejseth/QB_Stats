standings <- read_csv("http://www.habitatring.com/standings.csv")

standings <- standings %>%
  filter(season > 2005)

standings <- standings %>%
  mutate(
    team = case_when(
      team == 'OAK' ~ 'LV',
      team == 'SD' ~ 'LAC',
      team == 'STL' ~ 'LA',
      TRUE ~ team
    )
  )

filtered_standings <- standings %>%
  select(season, team, wins, losses, ties, pct, sos)

filtered_standings <- filtered_standings %>%
  mutate(total_wins = ifelse(ties==1, wins + 0.5, wins)) %>%
  select(season, team, total_wins, sos)

help_epa$season <- as.integer(help_epa$season)
filtered_standings$season <- as.integer(filtered_standings$season)

standings_with_epa <- filtered_standings %>%
  left_join(help_epa, by = c("team", "season"))

pff_grades <- read.csv("~/Downloads/pff_grades.csv")
#these will be on my github

all_help <- standings_with_epa %>%
  left_join(pff_grades, by = c("team", "season"))

all_help_no_teams <- all_help %>%
  select(-team, -total_help)

all_help_no_teams %>% 
  cor(use="complete.obs") %>%
  round(2)






