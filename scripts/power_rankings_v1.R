library(tidyverse)

matches <- readxl::read_excel("data/kamper_25-05-03.xlsx") %>%
  select(Runde, Hjemmelag, Bortelag, Hjemme, Borte) %>%
  rename(
    team_a = Hjemmelag,
    team_b = Bortelag,
    score_a = Hjemme,
    score_b = Borte
  ) %>%
  drop_na() %>%
  mutate(
    match = row_number()
  )

# Create a long format table for team stats
team_stats <- matches %>%
  mutate(
    points_a = case_when(
      score_a > score_b ~ 3,  # Team A wins
      score_a == score_b ~ 1, # Draw
      TRUE ~ 0                # Team A loses
    ),
    points_b = case_when(
      score_b > score_a ~ 3,  # Team B wins
      score_a == score_b ~ 1, # Draw
      TRUE ~ 0                # Team B loses
    ),
    gd_a = score_a - score_b,  # Goal differential for Team A
    gd_b = score_b - score_a   # Goal differential for Team B
  ) %>%
  pivot_longer(
    cols = c(team_a, team_b, score_a, score_b, points_a, points_b, gd_a, gd_b),
    names_to = c(".value", "team_type"),       # Capture "team_type" (a or b)
    names_pattern = "(.*)_(a|b)"               # Match prefix and suffix
  )


#####
# Step 1: Calculate strength metrics for each team (you can choose any metric)
team_strength <- team_stats %>%
  group_by(team) %>%
  summarize(
    total_points = sum(points, na.rm = TRUE),  # Total points scored
    total_gd = sum(gd, na.rm = TRUE),          # Total goal differential
    games_played = n()                         # Total games played
  ) %>%
  mutate(
    strength_score = total_points + (total_gd / games_played)  # Custom strength metric
  )

# Step 2: Join the strength scores back with the matches dataset
# For both Team A and Team B, add their opponent's strength score
matches_with_strength <- matches %>%
  left_join(
    team_strength %>% select(team, strength_score), by = c("team_a" = "team")) %>%
  rename(strength_a = strength_score) %>%
  left_join(
    team_strength %>% select(team, strength_score), by = c("team_b" = "team")) %>%
  rename(strength_b = strength_score)

# Step 3: Add opponent's strength as new columns
matches_with_strength <- matches_with_strength %>%
  mutate(
    opponent_strength_a = strength_b,  # A's opponent strength is B's strength
    opponent_strength_b = strength_a   # B's opponent strength is A's strength
  )

# View the result
head(matches_with_strength)


team_stats_strength <- team_stats %>%
  # Join team_strength for the current team
  left_join(
    matches_with_strength %>% select(Runde, match, strength_a, strength_b),
    by = c("Runde", "match")
  ) %>%
  mutate(
    team_strength = case_when(
      team_type == "a" ~ strength_a,
      TRUE ~ strength_b
    ),
    opponent_strength = case_when(
      team_type == "a" ~ strength_b,
      TRUE ~ strength_a
    ),
  ) %>%
  mutate(
    net_strength_difference = team_strength - opponent_strength # Example metric
  )

# Adjust ranking formula to include opponent strength
# Summarize stats for each team across matches
team_rankings <- team_stats_strength %>%
  group_by(team) %>%
  summarize(
    total_points = sum(points, na.rm = TRUE),
    games_played = n(),
    avg_points_per_game = total_points / games_played,
    total_gd = sum(gd, na.rm = TRUE),
    avg_opponent_strength = mean(opponent_strength, na.rm = TRUE),  # Average opponent strength
    avg_strength_diff = mean(net_strength_difference, na.rm = TRUE)  # Average opponent strength
  ) %>%
  mutate(
    ranking_score_1 = total_points * 2 + total_gd,  # Weighting points heavily, with a slight boost for goal diff
    ranking_score_2 = total_points * 2 + total_gd + avg_opponent_strength  # Weighted score
  ) %>%
  arrange(desc(total_points))

# View the updated rankings
team_rankings

