library(tidyverse)

# Sample dataset with the specified columns
# data <- team_stats_by_round_w_strength %>%
#   select(1:11, elo_rating) %>%
#   mutate(
#     elo_rating = if_else(is.na(elo_rating), 1500, elo_rating)
#   ) %>%
#   mutate(elo_curr = lag(elo_rating)) %>%
#   mutate(opponent_elo = data$elo_rating[match(opponent, data$team)],)

# # Define the ELO update function taking into consideration goals
# update_elo <- function(rating1 = elo_rating, rating2 = opponent_elo_rating, score1 = score, score2 = score_against, k_factor = 20) {
#   expected_score1 <- 1 / (1 + 10^((rating2 - rating1) / 400))
#   outcome_score1 <- ifelse(score1 > score2, 1, ifelse(score1 == score2, 0.5, 0))
#
#   goal_diff <- abs(score1 - score2)
#   G <- ifelse(goal_diff == 0, 1,
#               ifelse(goal_diff == 1, 1,
#                      ifelse(goal_diff == 2, 1.5,
#                             (11 + goal_diff) / 8)))
#
#   rating1_new <- rating1 + k_factor * G * (outcome_score1 - expected_score1)
#   rating2_new <- rating2 + k_factor * G * ((1 - outcome_score1) - (1 - expected_score1))
#
#   list(rating1_new = rating1_new, rating2_new = rating2_new)
# }
#
# update_elo(rating1 = 1490, rating2 = 1510, score1 = 10, score2 = 2)

# # Function to process each week's matches
# process_week <- function(week_data, elo_ratings) {
#   week_data %>%
#     filter(team_type != "bye") %>%
#     mutate(
#       rating1 = elo_ratings$elo_rating[match(team, elo_ratings$team)],
#       rating2 = elo_ratings$elo_rating[match(opponent, elo_ratings$team)],
#       new_ratings = pmap(list(rating1, rating2, score, goals_against), update_elo)
#     ) %>%
#     unnest_wider(new_ratings) %>%
#     mutate(
#       elo_rating_new = if_else(team_type == "a", new_ratings_rating1_new, new_ratings_rating2_new)
#     ) %>%
#     select(team, elo_rating_new)
# }

# # Apply the weekly ELO update
# results <- data %>%
#   arrange(week) %>%
#   group_by(team) %>%
#   nest() %>%
#   mutate(
#     updated_weekly = map(data, function(week_data) {
#       week_results <- process_week(week_data, data)
#       data <<- data %>%
#         mutate(
#           elo_rating = if_else(team %in% week_results$team, week_results$elo_rating_new, elo_rating)
#         )
#       data
#     })
#   ) %>%
#   unnest(updated_weekly)


# # Process ELO ratings for all weeks
# results <- data %>%
#   group_by(week) %>%
#   nest() %>%
#   mutate(
#     updated_weekly = map(data, ~{
#       week_data <- .x %>% filter(!is.na(match))  # Ensure you're filtering correctly to exclude byes
#       week_data %>%
#         mutate(
#           rating1 = data$elo_rating[match(team, data$team)],
#           rating2 = data$elo_rating[match(opponent, data$team)]
#         ) %>%
#         rowwise() %>%
#         mutate(new_ratings = list(update_elo(rating1, rating2, score, goal_difference))) %>%
#         unnest_wider(new_ratings) %>%
#         mutate(
#           elo_rating = if_else(team_type == "a", new_ratings_rating1_new, new_ratings_rating2_new)
#         ) %>%
#         select(team, elo_rating) %>%
#         ungroup()
#     })
#   ) %>%
#   unnest(updated_weekly) %>%
#   arrange(desc(elo_rating))
#


#_______
initial_elo <- matches %>%
  select(team_a) %>%
  distinct() %>%
  rename(team = team_a) %>%
  mutate(
    elo_rating = 1500
  )

matches <- matches %>% arrange(match, Dato)

# Define function to update Elo ratings with goal difference consideration
update_elo <- function(rating1, rating2, score1, score2, k_factor = 20) {
  expected_score_a <- 1 / (1 + 10^((rating2 - rating1) / 400))
  expected_score_b <- 1 - expected_score_a
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



# # Process matches to update Elo scores iteratively
# elo_ratings <- initial_elo
#
# # Sequentially update Elo ratings for matches
# for (i in seq_len(nrow(matches))) {
#
#   # Retrieve current match and associated ratings
#   current_match <- matches[i, ]
#   rating_a <- elo_ratings$elo_rating[match(current_match$team_a, elo_ratings$team)]
#   rating_b <- elo_ratings$elo_rating[match(current_match$team_b, elo_ratings$team)]
#
#   # Get updated ratings
#   updated_ratings <- update_elo(rating_a, rating_b, current_match$score_a, current_match$score_b)
#
#   # Update ratings in the current Elo table
#   elo_ratings <- elo_ratings %>%
#     mutate(
#       elo_rating = case_when(
#         team == current_match$team_a ~ updated_ratings$rating1_new,
#         team == current_match$team_b ~ updated_ratings$rating2_new,
#         TRUE ~ elo_rating
#       )
#     )
# }

# Ensure elo_ratings is the same as initial Elo ratings
elo_ratings <- initial_elo

# Copy matches to a new data frame 'matches_with_elo' to store updated elo ratings
matches_with_elo <- matches

# Process matches to update Elo scores iteratively and store back into dataset
for (i in seq_len(nrow(matches))) {

  # Retrieve current match and associated ratings
  current_match <- matches[i, ]
  rating_a <- elo_ratings$elo_rating[match(current_match$team_a, elo_ratings$team)]
  rating_b <- elo_ratings$elo_rating[match(current_match$team_b, elo_ratings$team)]

  # Get updated ratings
  updated_ratings <- update_elo(rating_a, rating_b, current_match$score_a, current_match$score_b, k_factor = 60)

  # Update ratings in the Elo table
  elo_ratings <- elo_ratings %>%
    mutate(
      elo_rating = case_when(
        team == current_match$team_a ~ updated_ratings$rating1_new,
        team == current_match$team_b ~ updated_ratings$rating2_new,
        TRUE ~ elo_rating
      )
    ) #%>%
    # slice_match(match number)

  # Store updated Elo ratings back into the matches dataset
  matches_with_elo[i, "elo_new_a"] <- updated_ratings$rating1_new
  matches_with_elo[i, "elo_new_b"] <- updated_ratings$rating2_new
  matches_with_elo[i, "elo_outcome_a"] <- updated_ratings$expected_elo_score_a
  matches_with_elo[i, "elo_outcome_b"] <- updated_ratings$expected_elo_score_b
}


# View the final Elo rankings
elo_ratings %>%
  arrange(desc(elo_rating))
