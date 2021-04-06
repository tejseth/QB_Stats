lucky_brady <- read.csv("~/Downloads/lucky_brady.csv")

lucky_brady <- lucky_brady %>%
  left_join(teams_colors_logos, by = c("team" = "team_abbr"))

lucky_brady %>%
  ggplot() +
  geom_link(
    mapping = aes(x = season_avg, y = season, xend = super_bowl_score, yend = season, size = 2, color = team_color)
  ) +
  theme_bw() +
  scale_colour_identity() +
  geom_image(aes(x = season_avg, y = season, image = team_logo_espn), size = 0.04, asp = 16/9) +
  geom_image(aes(x = super_bowl_score, y = season, image = super_bowl_logo), size = 0.04, asp = 16/9) +
  labs(
    x = "Points",
    y = "Season",
    title = "There is No Defense Better Than a Tom Brady Super Bowl Defense...",
    subtitle = "Team's logo is their season average for points, Super Bowl logo is how many points they score against Brady",
    caption = "By Tej Seth | @mfbanalytics"
  ) +
  theme(
    plot.title = element_markdown(hjust = 0.5, size = 20, face = "bold"),
    plot.subtitle = element_markdown(hjust = 0.5, size = 12),
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.background = element_blank(),
    panel.border= element_blank()
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
ggsave('lucky_brady.png', dpi=300, height=9*.8, width=16*.8)


