# Data prep ####
## Step 1: Loading Data and Preprocessing Matches ####
library(tidyverse)

# Define function to update Elo ratings with goal difference consideration
update_elo <- function(rating1, rating2, score1 = NULL, score2 = NULL, k_factor = 20) {
  expected_score_a <- 1 / (1 + 10^((rating2 - rating1) / 400))
  expected_score_b <- 1 - expected_score_a

  # Check if scores are provided
  if(is.null(score1) || is.null(score2)) {
    return(list(expected_elo_score_a = expected_score_a, expected_elo_score_b = expected_score_b))
  }

  outcome_score1 <- ifelse(score1 > score2, 1, ifelse(score1 == score2, 0.5, 0))

  goal_diff <- abs(score1 - score2)
  G <- ifelse(goal_diff == 0, 1,
              ifelse(goal_diff == 1, 1,
                     ifelse(goal_diff == 2, 1.5,
                            (11 + goal_diff) / 8)))

  rating1_new <- rating1 + k_factor * G * (outcome_score1 - expected_score_a)
  rating2_new <- rating2 + k_factor * G * ((1 - outcome_score1) - (1 - expected_score_a))

  list(rating1_new = rating1_new, rating2_new = rating2_new, expected_elo_score_a = expected_score_a, expected_elo_score_b = expected_score_b)
}

# matches_serie <- readxl::read_excel("data/kamper_25-05-03.xlsx")

matches_serie <- readxl::read_excel("data/kamper_25-05-19.xlsx")
# matches_obos <- readxl::read_excel("data/kamper_obos_2025-05-10.xlsx")
# matches_vest <- readxl::read_excel("data/kamper_vest_25-05-19.xlsx")
# matches_j14 <- readxl::read_excel("data/kamper_j14_25-05-21.xlsx")


matches <- matches_serie %>%
  bind_rows(matches_vest) %>%  # Combine both datasets
  # matches <- matches_vest %>%
  # matches <- matches_j14 %>%
  # filter(Turnering == "Jenter 13 år 1. div. avd. 02 vår")# %>%  # Filter for relevant rounds))
  separate(Resultat, into = c("Hjemme", "Borte"), sep = " - ", convert = TRUE, remove = FALSE) %>%
  mutate(
    Hjemme = as.numeric(Hjemme),
    Borte = as.numeric(Borte),
    week = lubridate::isoweek(Dato),  # Extract week number
  ) %>%
  select(Dato,Runde, week, Hjemmelag, Bortelag, Hjemme, Borte, Turnering) %>%  # Select relevant columns
  rename(
    team_a = Hjemmelag,   # Renaming Hjemmelag to team_a
    team_b = Bortelag,    # Renaming Bortelag to team_b
    score_a = Hjemme,     # Renaming Hjemme (home team score) to score_a
    score_b = Borte       # Renaming Borte (away team score) to score_b
  ) %>%
  arrange(Dato) %>%  # Arrange by Dato
  mutate(
    match = row_number()  # Assign a unique match ID to each row
  ) #%>%
# drop_na()           # Remove any rows with missing values



## Step 2: Deriving Points, Goal Differential, and Reshaping to Long Format ####
# Add team-specific stats like points and goal differential
team_stats_by_round <- matches %>%
  mutate(
    points_a = case_when(
      score_a > score_b ~ 3,  # 3 points for Team A if they win
      score_a == score_b ~ 1, # 1 point for a draw
      TRUE ~ 0                # 0 points if Team A loses
    ),
    points_b = case_when(
      score_b > score_a ~ 3,  # 3 points for Team B if they win
      score_a == score_b ~ 1, # 1 point for a draw
      TRUE ~ 0                # 0 points if Team B loses
    ),
    gd_a = score_a - score_b, # Goal differential for Team A
    gd_b = score_b - score_a,  # Goal differential for Team B
    score_against_a = score_b * -1, # Goal differential for Team A
    score_against_b = score_b * -1, # Goal differential for Team B
    opponent_a = team_b,  # Opponent for Team A
    opponent_b = team_a,  # Opponent for Team b
  ) %>%
  pivot_longer(
    cols = c(team_a, team_b, opponent_a, opponent_b, score_a, score_b, score_against_a, score_against_b, gd_a, gd_b, points_a, points_b),
    names_to = c(".value", "team_type"),       # Unpack value and team type (a or b)
    names_pattern = "(.*)_(a|b)"               # Regex pattern to separate base names and suffixes
  ) %>%
  arrange(match) %>%  # Arrange by match
  group_by(team, Turnering) %>%  # Group by team_type and week
  complete(
    week = full_seq(13:max(week), 1),
    fill = list(
      team_type = "bye"#,
      # points = 0,
      # gd = 0#,
      # score = 0,
      # score_against = 0
    )
  ) %>% # Fill in missing weeks
  mutate(
    games_played = cumsum(if_else(team_type %in% c("a", "b"), 1, 0)),  # Only count when team_type is 'a' or 'b'
    total_points = cumsum(ifelse(is.na(points), 0, points)),
    total_gd = cumsum(ifelse(is.na(gd), 0, gd)),
    total_scored = cumsum(ifelse(is.na(score), 0, score)),
    total_scored_against = cumsum(ifelse(is.na(score_against), 0, score_against)),
    total_points_home = cumsum(if_else(team_type == "a", points, 0)),
    total_points_away = cumsum(if_else(team_type == "b", points, 0)),
    total_gd_home = cumsum(if_else(team_type == "a", gd, 0)),
    total_gd_away = cumsum(if_else(team_type == "b", gd, 0)),
  ) %>% # Calculate cumulative points
  ungroup() %>%
  mutate(
    # elo_rating = case_when(week == 13 ~ 1500),  # Initialize Elo rating
    strength_score_static = round(total_points + (total_gd / games_played), 2),  # Custom strength metric
    strength_score_home = round(total_points_home + (total_gd_home / games_played), 2),
    strength_score_away = round(total_points_away + (total_gd_away / games_played), 2),
    strength_score_home_away = round((strength_score_home + strength_score_away) / 2, 2) # Average home/away performance
    # strength_score_recent = sum(weighted_points, na.rm = TRUE) + sum(weighted_gd / games_played, na.rm = TRUE)
  ) %>%
  # mutate_all(~ replace(., is.nan(.) | is.na(.), 0)) %>%
  mutate(
    strength_score = strength_score_static  # Select strength score to use as the final metric
  )
# At this point, the table is in long format with one row per team per match.

## Step 4a: Adding Strength Scores to Matches ####
# Add strength scores for both teams (team_a and team_b)
matches_with_strength <- matches %>%
  left_join(
    team_stats_by_round %>% select(team, strength_score, match),
    by = c("team_a" = "team", "match" = "match")  # Join on team_a
  ) %>%
  rename(strength_a = strength_score) %>%   # Rename column for clarity
  left_join(
    team_stats_by_round %>% select(team, strength_score, match),
    by = c("team_b" = "team", "match" = "match")  # Join on team_b
  ) %>%
  rename(strength_b = strength_score) %>%       # Rename column for clarity
  mutate( # Add opponent's strength for each team
    opponent_strength_a = strength_b,  # Strength of Team B (opponent of Team A)
    opponent_strength_b = strength_a   # Strength of Team A (opponent of Team B)
  )

## Step 4b: Adding ELO Rankings to Matches ####
# Add ELO scores for both teams (team_a and team_b)
initial_elo <- matches_with_strength %>%
  select(team_a) %>%
  distinct() %>%
  rename(team = team_a) %>%
  mutate(
    elo_rating = 1500
  )

matches_with_strength <- matches_with_strength %>% arrange(match, Dato)

# Ensure elo_ratings is the same as initial Elo ratings
elo_ratings <- initial_elo

# Copy matches to a new data frame 'matches_with_elo' to store updated elo ratings
# matches_with_elo <- matches_with_strength

# Process matches to update Elo scores iteratively and store back into dataset
for (i in seq_len(nrow(matches_with_strength))) {

  # Retrieve current match and associated ratings
  current_match <- matches_with_strength[i, ]
  rating_a <- elo_ratings$elo_rating[match(current_match$team_a, elo_ratings$team)]
  rating_b <- elo_ratings$elo_rating[match(current_match$team_b, elo_ratings$team)]

  # Use last available Elo to predict outcomes for games not yet played
  if (is.na(current_match$score_a) || is.na(current_match$score_b)) {
    # Predict outcome using expected scores only if scores are not available
    predicted_outcomes <- update_elo(rating_a, rating_b)  # Use function without scores
  } else {
    # Calculate updated ratings for games that have scores
    updated_ratings <- update_elo(rating_a, rating_b, current_match$score_a, current_match$score_b, k_factor = 40)

    # Update ratings in the Elo table
    elo_ratings <- elo_ratings %>%
      mutate(
        elo_rating = case_when(
          team == current_match$team_a ~ updated_ratings$rating1_new,
          team == current_match$team_b ~ updated_ratings$rating2_new,
          TRUE ~ elo_rating
        )
      )

    # Store updated Elo ratings back into the matches dataset
    matches_with_strength[i, "elo_new_a"] <- updated_ratings$rating1_new
    matches_with_strength[i, "elo_new_b"] <- updated_ratings$rating2_new
  }

  # Store expected outcomes for all matches (including predicted ones)
  matches_with_strength[i, "elo_outcome_a"] <- ifelse(is.na(current_match$score_a),
                                                      predicted_outcomes$expected_elo_score_a,
                                                      updated_ratings$expected_elo_score_a)
  matches_with_strength[i, "elo_outcome_b"] <- ifelse(is.na(current_match$score_b),
                                                      predicted_outcomes$expected_elo_score_b,
                                                      updated_ratings$expected_elo_score_b)
}
# for (i in seq_len(nrow(matches_with_strength))) {
#
#   # Retrieve current match and associated ratings
#   current_match <- matches_with_strength[i, ]
#   rating_a <- elo_ratings$elo_rating[match(current_match$team_a, elo_ratings$team)]
#   rating_b <- elo_ratings$elo_rating[match(current_match$team_b, elo_ratings$team)]
#
#   # Get updated ratings
#   updated_ratings <- update_elo(rating_a, rating_b, current_match$score_a, current_match$score_b, k_factor = 40)
#
#   # Update ratings in the Elo table
#   elo_ratings <- elo_ratings %>%
#     mutate(
#       elo_rating = case_when(
#         team == current_match$team_a ~ updated_ratings$rating1_new,
#         team == current_match$team_b ~ updated_ratings$rating2_new,
#         TRUE ~ elo_rating
#       )
#     ) #%>%
#   # slice_match(match number)
#
#   # Store updated Elo ratings back into the matches dataset
#   matches_with_strength[i, "elo_new_a"] <- updated_ratings$rating1_new
#   matches_with_strength[i, "elo_new_b"] <- updated_ratings$rating2_new
#   matches_with_strength[i, "elo_outcome_a"] <- updated_ratings$expected_elo_score_a
#   matches_with_strength[i, "elo_outcome_b"] <- updated_ratings$expected_elo_score_b
# }


## Step 5: Adding Strength Metrics to Team-Level Stats ####
# Enrich team_stats with strength values for both the team and their opponent
team_stats_by_round_w_strength <- team_stats_by_round %>%
  left_join(
    matches_with_strength %>% select(Runde, match, strength_a, strength_b, elo_new_a, elo_new_b, elo_outcome_a, elo_outcome_b),  # Add match-level strength scores
    by = c("Runde", "match")
  ) %>%
  mutate(
    team_strength = case_when(
      team_type == "a" ~ strength_a,    # For Team A, use strength_a
      TRUE ~ strength_b                 # For Team B, use strength_b
    ),
    opponent_strength = case_when(
      team_type == "a" ~ strength_b,    # For Team A, use strength_b (their opponent's strength)
      TRUE ~ strength_a                 # For Team B, use strength_a (their opponent's strength)
    ),
    team_elo = case_when(
      team_type == "a" ~ elo_new_a,    # For Team A, use strength_a
      TRUE ~ elo_new_b                 # For Team B, use strength_b
    ),
    opponent_elo = case_when(
      team_type == "a" ~ elo_new_b,    # For Team A, use strength_b (their opponent's strength)
      TRUE ~ elo_new_a                 # For Team B, use strength_a (their opponent's strength)
    ),
    team_prediction_elo = case_when(
      team_type == "a" ~ elo_outcome_a,    # For Team A, use strength_b (their opponent's strength)
      TRUE ~ elo_outcome_b                 # For Team B, use strength_a (their opponent's strength)
    )
  ) %>%
  mutate(
    Dato = case_when(week == 13 ~ as.Date("2025-04-01"), TRUE ~ Dato),
    team_elo = case_when(week == 13 ~ 1500, TRUE ~ team_elo),  # Initialize Elo rating for week 13)
    net_strength_difference = team_strength - opponent_strength,  # Difference in strength between team and opponent
    # expected_performance_elo = 1 / (1 + 10^((opponent_strength - team_strength) / 400)),  # Expected performance based on strength
    expected_Performance = team_strength / (team_strength + opponent_strength),  # Between 0-1
    over_performance = net_strength_difference / opponent_strength   # Adjust performance by opponent strength
  ) %>%
  mutate(
    # Add projected points based on ELO predictions
    points_status = case_when(
      !is.na(score) | week == 13 ~ "Played",  # If points are available, mark as played
      TRUE ~ "Projected"  # Otherwise, mark as projected
    ),
    points = case_when(
      !is.na(score) ~ points,    # If points are available, use points
      TRUE ~ case_when(
        team_prediction_elo >= 0.55 ~ 3,  # If predicted outcome is a win, assign 3 points
        team_prediction_elo < 0.55 & team_prediction_elo >= 0.45 ~ 1, # If predicted outcome is a draw, assign 1 point
        TRUE ~ 0                       # Otherwise, assign 0 points
      )  # Otherwise, mark as projected
    )
  ) %>%
  group_by(team) %>%
  mutate(
    games_played = cumsum(if_else(team_type %in% c("a", "b"), 1, 0)),  # Only count when team_type is 'a' or 'b'
    total_points = cumsum(ifelse(is.na(points), 0, points)),
    total_gd = cumsum(ifelse(is.na(gd), 0, gd)),
    total_scored = cumsum(ifelse(is.na(score), 0, score)),
    total_scored_against = cumsum(ifelse(is.na(score_against), 0, score_against)),
    total_points_home = cumsum(if_else(team_type == "a", points, 0)),
    total_points_away = cumsum(if_else(team_type == "b", points, 0)),
    total_gd_home = cumsum(if_else(team_type == "a", gd, 0)),
    total_gd_away = cumsum(if_else(team_type == "b", gd, 0)),
    game_outcome = case_when(
      points == 3 ~ "Win",
      points == 1 ~ "Draw",
      points == 0 ~ "Loss",
      TRUE ~ "Bye"
    ),
  ) %>%
  ungroup()

## Step 6: Calculating Rankings with Adjusted Metrics ####
# Summarize aggregate stats for each team, including strength-based metrics
team_rankings <- team_stats_by_round_w_strength %>%
  group_by(team) %>%
  summarize(
    total_points = sum(points, na.rm = TRUE),  # Total points scored by the team
    last_elo = last(team_elo),  # Last Elo rating for the team
    max_elo = max(team_elo, na.rm = TRUE),  # Maximum Elo rating for the team
    games_played = n(),                        # Total number of games played
    avg_points_per_game = total_points / games_played,  # Points per game
    total_gd = sum(gd, na.rm = TRUE),          # Total goal differential
    avg_opponent_strength = mean(opponent_strength, na.rm = TRUE),  # Average opponent strength
    avg_strength_diff = mean(net_strength_difference, na.rm = TRUE) # Average net strength difference
  ) %>%
  mutate(
    ranking_score_1 = total_points * 2 + total_gd,  # Formula 1: Weighted heavily by points and goal difference
    ranking_score_2 = total_points * 2 + total_gd + avg_opponent_strength  # Formula 2: Includes opponent strength
  ) %>%
  arrange(desc(total_points))  # Rank teams based on total points (default ranking)

# View resulting rankings
head(team_rankings)


# Data plots ####
library(tidyverse)
library(lubridate)
library(ggplot2)

## Plot data prep ####
end_points_projected <- team_stats_by_round_w_strength %>%
  ungroup() %>%
  group_by(team) %>%
  slice_max(order_by = match, n = 1) %>%
  ungroup() %>%
  arrange(-total_points, -strength_score_static, -total_gd, team)

end_points_played <- team_stats_by_round_w_strength %>%
  ungroup() %>%
  dplyr:: filter(points_status == "Played") %>%
  group_by(team) %>%
  slice_max(order_by = match, n = 1) %>%
  ungroup() %>%
  arrange(-total_points, -strength_score_static, -total_gd, team)

# Determine limits dynamically
date_minimum <- min(team_stats_by_round_w_strength$Dato) - weeks(1)
date_maximum <- max(team_stats_by_round_w_strength$Dato) + weeks(1)

# Calculate slopes and intercepts for each category
lines_data <- end_points_played %>%
  group_by(Turnering) %>%
  summarise(points_min = min(total_points),
            points_max = max(total_points),
            elo_min = min(team_elo),
            elo_max = max(team_elo)) %>%
  mutate(slope = (elo_max - elo_min) / (points_max - points_min),
         intercept = elo_min - slope * points_min)

team_stats_by_round_exp <- team_stats_by_round_w_strength %>%
  # select(team, games_played, total_points, gd, total_gd, strength_score_static, net_strength_difference) %>%
  arrange(team, -total_points, -total_gd, -games_played) %>%
  select(team) %>%
  distinct()

## Point rankings and projections ####
fig_points <- ggplot(team_stats_by_round_w_strength, aes(x = Dato, y = total_points, color = team)) +
  geom_hline(yintercept = 0, linetype = "longdash") +                                 # Add horizontal lines
  geom_line(linetype = "dashed", linewidth = 1, alpha = 0.8) + # Base line without varying linetypes
  geom_line(aes(linetype = points_status),linewidth = 1, alpha = 0.8, show.legend = TRUE) +
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
fig_points

## Elo ranking plot ####
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
fig_elo


## Compare points and Elo rankings ####
fig_comp <- ggplot(end_points_played, aes(x = total_points, y = team_elo)) +
  geom_point(aes(color = team), size = 4, alpha = 0.8) +
  geom_text(aes(label = team), vjust = -1, hjust = 0.5) +  # Add team names near points
  geom_abline(data = lines_data, aes(slope = slope, intercept = intercept), linetype = "dashed", color = "gray") +  # Line of equality
  scale_x_continuous(breaks = seq(0, 50, by = 1), minor_breaks = NULL) +
  scale_y_continuous(breaks = seq(1000, 2000, by = 50), minor_breaks = NULL) +
  labs(title = "Comparison of Team Rankings by Points and ELO",
       x = "Points Rank",
       y = "ELO Rank") +
  hrbrthemes::theme_ipsum_rc() +
  ggsci::scale_color_d3(palette = "category20", guide = 'none')  +
  theme(legend.position = "none") +                  # Optionally remove legend
  facet_wrap(~Turnering, ncol = 2, scales = "free")
fig_comp


## Heat plot ####
# Visualization using a heatmap
heatmap_plot <- team_stats_by_round_w_strength %>%
  mutate(
    normalized_prediction = case_when(
      points == 3 ~ team_prediction_elo * 1.5,      # Win emphasizing
      points == 1 ~ (0.5 + 2 * team_prediction_elo) / 3,  # Skew towards tie clarity
      points == 0 ~ (team_prediction_elo + 0.5) * 1.5 / 3 # Loss focus
      # points == 3 ~ team_prediction_elo,
      # points == 1 ~ (0.5 + team_prediction_elo) / 2,  # Closer to tie
      # points == 0 ~ (team_prediction_elo + 0.5) / 2   # Closer to loss
    ),
    prediction_distance = abs(normalized_prediction * 3 - points),  # Scale factor
    prediction_gradient = 1 - prediction_distance / 3  # Adjust for bounded format

    # prediction_gradient = abs(normalized_prediction - points)
  ) %>%
  dplyr::filter(points_status == "Played") %>%
  drop_na(opponent) %>%
  mutate(team = factor(team, levels = end_points_played$team)) %>%
  mutate(opponent = factor(opponent, levels = end_points_played$team)) %>%
  ggplot(aes(x = opponent, y = team)) +
  geom_tile(aes(fill = prediction_gradient), color = "darkgrey", width = 0.9, height = 0.9, size = 0.3) +
  geom_text(aes(label = game_outcome), color = "black", size = 4) +  # Overlay text
  scale_fill_gradient2(low = "red", mid = "yellow", high = "green",
                       midpoint = 0.5, limits = c(0, 1),
                       breaks = c(0, 0.5, 1),
                       labels = c("Unpredicted Result", "Neutral", "Predicted Result"),
                       name = "Prediction Status") +
  labs(title = "Match Outcomes and Elo Predictions",
       x = "Opponent",
       y = "Team") +
  hrbrthemes::theme_ipsum_rc() +
  # ggsci::scale_color_d3(palette = "category20", guide = 'none')  +
  # theme(legend.position = "none") +                  # Optionally remove legend
  facet_wrap(~Turnering, ncol = 2, scales = "free") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
heatmap_plot
