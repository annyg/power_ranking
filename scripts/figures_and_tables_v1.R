# Figure Data prep ####
end_points_projected <- team_stats_by_round_w_strength %>%
  ungroup() %>%
  # dplyr:: filter(team_type != "bye") %>%
  group_by(team) %>%
  slice_max(order_by = match, n = 1) %>%
  ungroup() %>%
  arrange(-total_points, -strength_score_static, -total_gd, team)

end_points_played <- team_stats_by_round_w_strength %>%
  ungroup() %>%
  # dplyr:: filter(team_type != "bye") %>%
  dplyr:: filter(points_status == "Played") %>%
  group_by(team) %>%
  slice_max(order_by = match, n = 1) %>%
  ungroup() %>%
  arrange(-total_points, -strength_score_static, -total_gd, team)


end_points_projected_wl <- team_stats_by_round_w_strength %>%
  ungroup() %>%
  dplyr:: filter(team_type != "bye") %>%
  group_by(team) %>%
  summarise(win = sum(points == 3),
            loss = sum(points == 0),
            draw = sum(points == 1),.groups = 'drop') %>%
  ungroup()

end_points_played_wl <- team_stats_by_round_w_strength %>%
  ungroup() %>%
  dplyr:: filter(team_type != "bye") %>%
  dplyr:: filter(points_status == "Played") %>%
  group_by(team) %>%
  summarise(win = sum(points == 3),
            loss = sum(points == 0),
            draw = sum(points == 1),.groups = 'drop') %>%
  ungroup()
# Determine limits dynamically
date_minimum <- min(team_stats_by_round_w_strength$Dato, na.rm = TRUE) - weeks(1)
date_maximum <- max(team_stats_by_round_w_strength$Dato, na.rm = TRUE) + weeks(1)

# Calculate slopes and intercepts for each category
lines_data <- end_points_played %>%
  group_by(Turnering) %>%
  summarise(points_min = min(total_points),
            points_max = max(total_points),
            elo_min = min(team_elo),
            elo_max = max(team_elo)) %>%
  mutate(slope = (points_max - points_min) / (elo_max - elo_min),
         intercept = points_min - slope * elo_min)

team_stats_by_round_exp <- team_stats_by_round_w_strength %>%
  # select(team, games_played, total_points, gd, total_gd, strength_score_static, net_strength_difference) %>%
  arrange(team, -total_points, -total_gd, -games_played) %>%
  select(team) %>%
  distinct()

# Elo ranking plot ####
fig_elo <- ggplot(team_stats_by_round_w_strength, aes(x = Dato, y = team_elo, color = team)) +
  geom_line(linewidth = 0.7, alpha = 0.8) +
  geom_line(aes(linetype = points_status),linewidth = 1, alpha = 0.8, show.legend = FALSE) +
  geom_point(aes(shape = game_outcome), size = 3, alpha = 0.8) +
  geom_text(data = end_points_played,                      # Add labels to endpoints
            aes(label = team),
            hjust = -0.2,                           # Horizontal adjustment (push text slightly to the right)
            size = 4, angle = -10) +                             # Font size
  scale_y_continuous(breaks = seq(1000, 2000, by = 50), minor_breaks = NULL) +   # Set y-axis breaks
  scale_x_datetime(date_breaks = "1 month",  # Breaks at each week
                   date_labels = "%m-%d",  # Format for date labels
                   limits = c(date_minimum, date_maximum)) +
  labs(title = "ELO Ranking", x = "Date", y = "ELO Rank") +
  hrbrthemes::theme_ipsum_rc() +
  ggsci::scale_color_d3(palette = "category20", guide = 'none')  +
  theme(legend.position = "bottom") +                  # Optionally remove legend
  facet_wrap(~Turnering, ncol = 2)

# Point rankings and projections ####
fig_points <- ggplot(team_stats_by_round_w_strength, aes(x = Dato, y = total_points, color = team)) +
  geom_hline(yintercept = 0, linetype = "longdash") +                                 # Add horizontal lines
  geom_line(linetype = "dashed", linewidth = 1, alpha = 0.8) + # Base line without varying linetypes
  geom_line(aes(linetype = points_status),linewidth = 1, show.legend = TRUE) +
  geom_point(aes(shape = game_outcome), size = 3, alpha = 0.8) +
  geom_text(data = end_points_projected,                      # Add labels to endpoints
            aes(label = team),
            hjust = -0.1,                           # Horizontal adjustment (push text slightly to the right)
            size = 4, angle = -10) +                             # Font size
  scale_y_continuous(breaks = seq(0, 100, by = 1), limits = c(0,NA), minor_breaks = NULL) +   # Set y-axis breaks
  # scale_x_continuous(limits = c(0,6.5), breaks = seq(0, 7, by = 1), minor_breaks = NULL) +   # Set x-axis breaks
  scale_x_datetime(date_breaks = "1 month",  # Breaks at each week
                   date_labels = "%m-%d",  # Format for date labels
                   limits = c(date_minimum, date_maximum)) +
  labs(title = "Total Team Points Over Season", subtitle = "With projected outcomes based on latest ELO ranking", x = "Date", y = "Total Points") +
  hrbrthemes::theme_ipsum_rc() +
  ggsci::scale_color_d3(palette = "category20", guide = 'none')  +
  theme(legend.position = "bottom") +                   # Optionally remove legend
  facet_wrap(~Turnering, ncol = 2)

# Point rankings and projections ####
fig_ppg <- ggplot(team_stats_by_round_w_strength, aes(x = Dato, y = points_per_game, color = team)) +
  geom_hline(yintercept = 0, linetype = "longdash") +                                 # Add horizontal lines
  geom_line(linetype = "dashed", linewidth = 1, alpha = 0.8) + # Base line without varying linetypes
  geom_line(aes(linetype = points_status),linewidth = 1, show.legend = TRUE) +
  geom_point(aes(shape = game_outcome), size = 3, alpha = 0.8) +
  geom_text(data = end_points_projected,                      # Add labels to endpoints
            aes(label = team),
            hjust = -0.1,                           # Horizontal adjustment (push text slightly to the right)
            size = 4, angle = -10) +                             # Font size
  scale_y_continuous(breaks = seq(0, 100, by = 1), limits = c(0,NA), minor_breaks = NULL) +   # Set y-axis breaks
  # scale_x_continuous(limits = c(0,6.5), breaks = seq(0, 7, by = 1), minor_breaks = NULL) +   # Set x-axis breaks
  scale_x_datetime(date_breaks = "1 month",  # Breaks at each week
                   date_labels = "%m-%d",  # Format for date labels
                   limits = c(date_minimum, date_maximum)) +
  labs(title = "Points-Per-Game Over Season", subtitle = "With projected outcomes based on latest ELO ranking", x = "Date", y = "Points per game") +
  hrbrthemes::theme_ipsum_rc() +
  ggsci::scale_color_d3(palette = "category20", guide = 'none')  +
  theme(legend.position = "bottom") +                   # Optionally remove legend
  facet_wrap(~Turnering, ncol = 2)

# Add table with the final standings
tbl_standings_current <- end_points_played %>%
  select(team, games_played, total_points, total_gd, team_elo, points_per_game) %>%
  arrange(-total_points, -total_gd, -team_elo) %>%
  mutate(rank = row_number()) %>%
  select(rank, everything())  %>%
  left_join(end_points_played_wl, by = "team")

tbl_standings_current_gt <- gt::gt(tbl_standings_current) %>%
  gt::tab_header(
    title = "Current Standings",
    subtitle = tournament
  ) %>%
  gt::cols_label(
    team = "Team",
    games_played = "Games Played",
    total_points = "Total Points",
    total_gd = "Goal Difference",
    team_elo = "ELO Rank",
    points_per_game = "Points per Game"
  )

# Add table with the projected standings
tbl_standings_projected <- end_points_projected %>%
  select(team, games_played, total_points, total_gd, team_elo, points_per_game) %>%
  arrange(-total_points, -total_gd, -team_elo) %>%
  mutate(rank = row_number()) %>%
  select(rank, everything()) %>%
  left_join(end_points_projected_wl, by = "team")

tbl_standings_projected_gt <- gt::gt(tbl_standings_projected) %>%
  gt::tab_header(
    title = "Projected Standings",
    subtitle = tournament
  ) %>%
  gt::cols_label(
    team = "Team",
    games_played = "Games Played",
    total_points = "Total Points",
    total_gd = "Goal Difference",
    team_elo = "ELO Rank",
    points_per_game = "Points per Game"
  )

# Table with total points per team
tbl_points_per_team_played <- end_points_played %>%
  summarise(
    number_of_teams = n_distinct(team),
    total_points = sum(total_points, na.rm = TRUE),
    total_games = sum(games_played, na.rm = TRUE)/2,
    total_points_per_team = round(total_points / number_of_teams, 2),
    available_points_per_game = round(total_points / total_games, 2)
  ) %>%
  mutate(Status = "Played") %>%
  select(Status, everything())

tbl_points_per_team_projected <- end_points_projected %>%
  summarise(
    number_of_teams = n_distinct(team),
    total_points = sum(total_points, na.rm = TRUE),
    total_games = sum(games_played, na.rm = TRUE)/2,
    total_points_per_team = round(total_points / number_of_teams, 2),
    available_points_per_game = round(total_points / total_games, 2)
  ) %>%
  mutate(Status = "Projected") %>%
  select(Status, everything())

played_wl <- end_points_played_wl %>%
  summarise("Win/Loss games" = sum(win),
            "Draw games" = sum(draw)/2) %>%
  mutate(Status = "Played")

projected_wl <- end_points_projected_wl %>%
  summarise("Win/Loss games" = sum(win),
            "Draw games" = sum(draw)/2) %>%
  mutate(Status = "Projected")

tbl_points_per_team <- bind_rows(tbl_points_per_team_played, tbl_points_per_team_projected) %>%
  left_join(bind_rows(played_wl, projected_wl), by = "Status")# %>%
  # left_join(, by = "Status")

tbl_points_per_team_gt <- gt::gt(tbl_points_per_team) %>%
  gt::tab_header(
    title = "Total Points per Team",
    subtitle = tournament
  ) %>%
  gt::cols_label(
    number_of_teams = "Number of Teams",
    total_points = "Total Points",
    total_games = "Total Games",
    total_points_per_team = "Points per Team",
    available_points_per_game = "Points per Game"
  )

outcomes_played <- team_stats_by_round_w_strength %>%
  select(match, points, points_status) %>%
  distinct() %>%
  drop_na(match) %>%
  group_by(match) %>%
  slice_max(order_by = points, n = 1) %>%
  ungroup() %>%
  group_by(points_status) %>%
  summarise(win_loss = sum(points == 3 | points == 0),
            draw = sum(points == 1),.groups = 'drop')

# Heat plot ####
# Visualization using a heatmap
heatmap_plot <- team_stats_by_round_w_strength %>%
  drop_na(opponent) %>%
  mutate(team = factor(team, levels = end_points_played$team)) %>%
  mutate(opponent = factor(opponent, levels = end_points_played$team)) %>%
  ggplot(aes(x = opponent, y = team)) +
  geom_tile(aes(fill = prediction_gradient), color = "darkgrey", width = 0.9, height = 0.9, size = 0.3) +
  geom_text(aes(label = game_outcome), color = "black", size = 4, nudge_y = 0.15) +  # Overlay text
  geom_text(aes(label = round(team_prediction_elo, digits = 2)), color = "black", size = 4, nudge_y = -0.15) +  # Overlay text
  scale_fill_gradient2(low = "red",
                       # mid = "yellow",
                       high = "green", na.value = "lightgrey",
                       midpoint = 0.5, limits = c(0, 1),
                       breaks = c(0, 0.5, 1),
                       labels = c("Unpredicted Result", "Neutral", "Predicted Result"),
                       name = "Prediction Status") +
  labs(title = "Match Outcomes and Elo Predictions by Matchup",
       x = "Opponent",
       y = "Team") +
  hrbrthemes::theme_ipsum_rc() +
  facet_wrap(~Turnering, ncol = 2, scales = "free") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Heat plot ####
# Visualization using a heatmap
heatmap_plot_round <- team_stats_by_round_w_strength %>%
  drop_na(opponent) %>%
  mutate(
  #   Runde = factor(Runde, levels = sort(unique(as.numeric(Runde)), decreasing = TRUE)),
  #   opponent_short = str_trunc(opponent, width = 9, side = "right", ellipsis = "..."),
    team = factor(team, levels = end_points_played$team)
  ) %>%
  ggplot(aes(y = week_game, x = team)) +
  geom_tile(aes(fill = prediction_gradient), color = "darkgrey", width = 0.9, height = 0.9, size = 0.3) +
  geom_text(aes(label = opponent_short), color = "black", size = 3, nudge_y = 0.21) +  # Overlay text
  geom_text(aes(label = game_outcome), color = "black", size = 4, nudge_y = 0) +  # Overlay text
  geom_text(aes(label = round(team_prediction_elo, digits = 2)), color = "black", size = 4, nudge_y = -0.21) +  # Overlay text
  scale_fill_gradient2(low = "red",
                       # mid = "yellow",
                       high = "palegreen2", na.value = "lightgrey",
                       midpoint = 0.5, limits = c(0, 1),
                       breaks = c(0, 0.5, 1),
                       labels = c("Unpredicted Result", "Neutral", "Predicted Result"),
                       name = "Prediction Status") +
  labs(title = "Match Outcomes and Elo Predictions by Rounds",
       y = "Round",
       x = "Team") +
  hrbrthemes::theme_ipsum_rc() +
  facet_wrap(~Turnering, ncol = 2, scales = "free") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Heat plot ####
# Visualization using a heatmap
heatmap_plot_ppg <- team_stats_by_round_w_strength %>%
  drop_na(opponent) %>%
  # group_by(team, week) %>%
  # mutate(
  #   game_number = row_number(),) %>%
  # ungroup() %>%
  mutate(
  #   week = factor(week, levels = sort(unique(as.numeric(week)), decreasing = TRUE)),
  #   opponent_short = str_trunc(opponent, width = 9, side = "right", ellipsis = "..."),
    team = factor(team, levels = end_points_played$team),
  #   week_game = paste0("week ", week, " game ", game_number),
  ) %>%
  # mutate(
  #   week_game = factor(week_game, levels = sort(unique(week_game), decreasing = TRUE)),  # Ensure ascending order
  #   points_fill = case_when(points_status == "Played" ~ points_per_game,
  #                           points_status == "Projected" ~ NA_real_)
  # ) %>%
  ggplot(aes(y = week_game, x = team)) +
  geom_tile(aes(fill = points_fill), color = "darkgrey", width = 0.9, height = 0.9, size = 0.3) +
  geom_text(aes(label = opponent_short), color = "black", size = 3, nudge_y = 0.21) +  # Overlay text
  geom_text(aes(label = game_outcome), color = "black", size = 4, nudge_y = 0) +  # Overlay text
  geom_text(aes(label = points_per_game), color = "black", size = 4, nudge_y = -0.21) +  # Overlay text
  scale_fill_gradient(
    low = "white",
    # mid = "yellow",
    high = "dodgerblue3",
    na.value = "lightgrey",
    # midpoint = 0.5,
    limits = c(0, 3),
    breaks = c(0, 1, 2, 3),
    labels = c("0 points", "1 points", "2 points", "3 points"),
    name = "Points-Per-Game") +
  labs(title = "Points per games and match outcomes by week",
       x = "Team",
       y = "Week") +
  hrbrthemes::theme_ipsum_rc() +
  facet_wrap(~Turnering, ncol = 2, scales = "free") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Compare points and Elo rankings ####
fig_comp <- ggplot(end_points_played, aes(x = team_elo, y = total_points)) +
  geom_point(aes(color = team), size = 4, alpha = 0.8) +
  geom_text(aes(label = team), vjust = -1, hjust = 0.5) +  # Add team names near points
  geom_text(aes(label = "Overvalued", x = min(team_elo), y = max(total_points)), hjust = -0.5) +
  geom_text(aes(label = "Undervalued", x = max(team_elo), y = min(total_points)+2), hjust = 1) +
  geom_text(aes(label = team), vjust = -1, hjust = 0.5) +  # Add team names near points
  geom_abline(data = lines_data, aes(slope = slope, intercept = intercept), linetype = "dashed", color = "gray") +  # Line of equality
  scale_y_continuous(limits = c(min(end_points_played$total_points), max(end_points_played$total_points + 2)), breaks = seq(0, 50, by = 1), minor_breaks = NULL) +
  scale_x_continuous(breaks = seq(1000, 2000, by = 50), minor_breaks = NULL) +
  labs(title = "Comparison of Team Rankings by Points and ELO",
       y = "Points Rank",
       x = "ELO Rank") +
  hrbrthemes::theme_ipsum_rc() +
  ggsci::scale_color_d3(palette = "category20", guide = 'none')  +
  theme(legend.position = "none") +                  # Optionally remove legend
  facet_wrap(~Turnering, ncol = 2, scales = "free")
