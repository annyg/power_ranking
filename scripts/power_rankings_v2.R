# Step 1: Loading Data and Preprocessing Matches ####
library(tidyverse)

matches <- readxl::read_excel("data/kamper_25-05-03.xlsx") %>%
  select(Dato,Runde, Hjemmelag, Bortelag, Hjemme, Borte) %>%  # Select relevant columns
  rename(
    team_a = Hjemmelag,   # Renaming Hjemmelag to team_a
    team_b = Bortelag,    # Renaming Bortelag to team_b
    score_a = Hjemme,     # Renaming Hjemme (home team score) to score_a
    score_b = Borte       # Renaming Borte (away team score) to score_b
  ) %>%
  arrange(Dato) %>%  # Arrange by Dato
  mutate(
    match = row_number()  # Assign a unique match ID to each row
  ) %>%
  drop_na()           # Remove any rows with missing values

# Step 2: Deriving Points, Goal Differential, and Reshaping to Long Format ####
# Add team-specific stats like points and goal differential
team_stats <- matches %>%
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
    gd_b = score_b - score_a  # Goal differential for Team B
  ) %>%
  pivot_longer(
    cols = c(team_a, team_b, score_a, score_b, points_a, points_b, gd_a, gd_b),
    names_to = c(".value", "team_type"),       # Unpack value and team type (a or b)
    names_pattern = "(.*)_(a|b)"               # Regex pattern to separate base names and suffixes
  )
# At this point, the table is in long format with one row per team per match.

# Step 3: Calculating Team Strength Metrics ####
# Group by team and calculate aggregated metrics such as total points, goal differential, and games played
team_strength <- team_stats %>%
  arrange(match) %>%
  group_by(team) %>%
  mutate(
    time_weight = exp(as.numeric(match - max(match)) / 30),  # Weigh recent matches more
    weighted_points = points * time_weight,
    weighted_gd = gd * time_weight
  ) %>%
  summarize(
    total_points = sum(points, na.rm = TRUE),  # Total points scored by the team
    total_gd = sum(gd, na.rm = TRUE),          # Total goal differential (goals scored - goals conceded)
    games_played = n(),                        # Total number of matches played
    total_points_home = sum(points[team_type == "a"], na.rm = TRUE),
    total_points_away = sum(points[team_type == "b"], na.rm = TRUE),
    total_gd_home = sum(gd[team_type == "a"], na.rm = TRUE),
    total_gd_away = sum(gd[team_type == "b"], na.rm = TRUE),
    strength_score_recent = sum(weighted_points, na.rm = TRUE) + sum(weighted_gd / games_played, na.rm = TRUE)
  ) %>%
  mutate(
    strength_score_static = total_points + (total_gd / games_played),  # Custom strength metric
    strength_score_home = total_points_home + (total_gd_home / games_played),
    strength_score_away = total_points_away + (total_gd_away / games_played),
    strength_score_home_away = (strength_score_home + strength_score_away) / 2  # Average home/away performance
  ) %>%
  mutate(
    strength_score = strength_score_recent  # Select strength score to use as the final metric
  )

# Step 4: Adding Strength Scores to Matches ####
# Add strength scores for both teams (team_a and team_b)
matches_with_strength <- matches %>%
  left_join(
    team_strength %>% select(team, strength_score),
    by = c("team_a" = "team")  # Join on team_a
  ) %>%
  rename(strength_a = strength_score) %>%   # Rename column for clarity
  left_join(
    team_strength %>% select(team, strength_score),
    by = c("team_b" = "team")  # Join on team_b
  ) %>%
  rename(strength_b = strength_score)       # Rename column for clarity

# Add opponent's strength for each team
matches_with_strength <- matches_with_strength %>%
  mutate(
    opponent_strength_a = strength_b,  # Strength of Team B (opponent of Team A)
    opponent_strength_b = strength_a   # Strength of Team A (opponent of Team B)
  )

# Step 5: Adding Strength Metrics to Team-Level Stats ####
# Enrich team_stats with strength values for both the team and their opponent
team_stats_strength <- team_stats %>%
  left_join(
    matches_with_strength %>% select(Runde, match, strength_a, strength_b),  # Add match-level strength scores
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
    )
  ) %>%
  mutate(
    net_strength_difference = team_strength - opponent_strength  # Difference in strength between team and opponent
  )

# Step 6: Calculating Rankings with Adjusted Metrics ####
# Summarize aggregate stats for each team, including strength-based metrics
team_rankings <- team_stats_strength %>%
  group_by(team) %>%
  summarize(
    total_points = sum(points, na.rm = TRUE),  # Total points scored by the team
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


# Step 7a: Dynamic Ratings Framework (Inspired by Elo or Glicko Models) ####
# Initialize ratings
team_ratings <- team_strength %>%
  mutate(initial_rating = 1500)

# Define Elo update function
update_elo <- function(rating_team, rating_opp, actual_result, k = 40) {
  expected_result <- 1 / (1 + 10^((rating_opp - rating_team) / 400))
  rating_team + k * (actual_result - expected_result)
}

# Dynamically update ratings for each match
elo_results <- matches %>%
  arrange(Runde) %>%  # Order matches chronologically
  rowwise() %>%
  mutate(
    rating_a_new = update_elo(team_a_rating, team_b_rating, ifelse(score_a > score_b, 1, ifelse(score_a == score_b, 0.5, 0))),
    rating_b_new = update_elo(team_b_rating, team_a_rating, ifelse(score_b > score_a, 1, ifelse(score_a == score_b, 0.5, 0)))
  )

# Replace old ratings with updated ratings for next matches


# Step 7b: Bayesian Hierarchical Modeling ####
library(brms)

# Create a dataframe for modeling
data_for_model <- matches_with_strength %>%
  pivot_longer(cols = c(team_a, team_b),
               names_to = "team_role",
               values_to = "team") %>%
  mutate(
    home_advantage = ifelse(team_role == "team_a", 1, 0),  # Home teams have an advantage
    goal_diff = ifelse(team_role == "team_a", score_a - score_b, score_b - score_a),  # Adjust goal_diff for away teams
    strength_team = ifelse(team_role == "team_a", strength_a, strength_b),
    strength_opponent = ifelse(team_role == "team_a", strength_b, strength_a)
  )

# Fit a Bayesian model
model <- brm(
  formula = goal_diff ~ strength_team + strength_opponent + home_advantage + (1 | team),  # Hierarchical model
  data = data_for_model,
  family = gaussian(),                     # Assuming goal difference follows a Gaussian distribution
  prior = c(
    prior(normal(0, 1), class = "b"),      # Prior for slope coefficients
    prior(normal(0, 10), class = "Intercept")  # Prior for intercept
  ),
  chains = 4, iter = 2000, warmup = 1000   # MCMC configuration
)


# Summarize posterior distributions
summary(model)

# Predict team strengths
team_strengths <- posterior_samples(model)


# Step 7c: Predictive Models for Match Outcomes ####
library(randomForest)

# Prepare training data for predictive modeling
model_data <- matches_with_strength %>%
  mutate(
    result = case_when(
      score_a > score_b ~ "Win",
      score_a < score_b ~ "Loss",
      TRUE ~ "Draw"
    ),
    goal_diff = score_a - score_b,
    avg_strength_diff = strength_a - strength_b
  ) %>%
  mutate(result = as.factor(result)) %>%
  select(result, avg_strength_diff, opponent_strength_a, opponent_strength_b)

# Create an 80/20 split for training and testing
set.seed(123)  # Set seed for reproducibility
library(caret)
# train_index <- createDataPartition(model_data$result, p = 0.8, list = FALSE)
# train_data <- model_data[train_index, ]  # 80% for training
# test_data <- model_data[-train_index, ]  # 20% for testing
train_data <- model_data

# Train a random forest model
rf_model <- randomForest(result ~ .,  # Predicting 'result'
                         data = train_data,  # Training dataset
                         ntree = 500,  # Number of trees
                         importance = TRUE)  # Measure feature importance

# View feature importance
importance(rf_model)

# View the trained random forest model
print(rf_model)

# Variable importance (e.g., which features contributed most to the predictions)
varImpPlot(rf_model)

# Predict outcomes for future matches
predictions <- predict(rf_model, newdata = test_data)


# 5-Fold Cross-Validation Example
library(caret)
train_control <- trainControl(method = "cv", number = 5)
rf_cv_model <- train(result ~ ., data = train_data, method = "rf", trControl = train_control)

# View cross-validation performance
print(rf_cv_model)

