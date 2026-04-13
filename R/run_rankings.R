# R/run_rankings.R
# Wraps the full v7 Elo + strength ranking pipeline into a single function.
# Input:  matches (normalized data frame from normalize_matches())
#         k_factor (numeric, controls Elo rating sensitivity)
# Output: named list of all computed objects needed by make_figures() and make_tables()

library(dplyr)
library(tidyr)
library(lubridate)

# ---------------------------------------------------------------------------
# Elo update function
# ---------------------------------------------------------------------------

#' Calculate updated Elo ratings for one match.
#'
#' @param rating1 Current Elo rating of the home team.
#' @param rating2 Current Elo rating of the away team.
#' @param score1  Goals scored by home team (NULL if match not yet played).
#' @param score2  Goals scored by away team (NULL if match not yet played).
#' @param k_factor Elo K-factor.
#' @return Named list with rating1_new, rating2_new, expected_elo_score_a/b.
update_elo <- function(rating1, rating2, score1 = NULL, score2 = NULL, k_factor = 60) {
  expected_score_a <- 1 / (1 + 10^((rating2 - rating1) / 400))
  expected_score_b <- 1 - expected_score_a

  if (is.null(score1) || is.null(score2)) {
    return(list(
      expected_elo_score_a = expected_score_a,
      expected_elo_score_b = expected_score_b
    ))
  }

  outcome_score1 <- ifelse(score1 > score2, 1, ifelse(score1 == score2, 0.5, 0))
  goal_diff <- abs(score1 - score2)

  G <- dplyr::case_when(
    goal_diff == 0        ~ 1.0,
    goal_diff == 1        ~ 1.1,
    goal_diff %in% 2:3    ~ 1.5,
    goal_diff %in% 4:5    ~ 2.0,
    goal_diff > 5         ~ 2.5
  )

  rating1_new <- rating1 + k_factor * G * (outcome_score1 - expected_score_a)
  rating2_new <- rating2 + k_factor * G * ((1 - outcome_score1) - expected_score_b)

  list(
    rating1_new          = rating1_new,
    rating2_new          = rating2_new,
    expected_elo_score_a = expected_score_a,
    expected_elo_score_b = expected_score_b
  )
}

# ---------------------------------------------------------------------------
# Main pipeline function
# ---------------------------------------------------------------------------

#' Run the full Elo + strength ranking pipeline.
#'
#' @param matches Normalized data frame from normalize_matches(). Must contain:
#'   Dato, Runde, Hjemmelag, Bortelag, Resultat, Turnering.
#' @param k_factor Numeric. Elo K-factor (default 60). Higher = more volatile.
#' @return Named list of all computed objects for downstream use.
run_rankings <- function(matches, k_factor = 60) {

  # ------------------------------------------------------------------
  # Step 1: Preprocess matches
  # ------------------------------------------------------------------
  matches <- matches %>%
    tidyr::separate(
      Resultat,
      into    = c("Hjemme", "Borte"),
      sep     = " - ",
      convert = TRUE,
      remove  = FALSE
    ) %>%
    dplyr::mutate(
      Hjemme = as.numeric(Hjemme),
      Borte  = as.numeric(Borte),
      week   = lubridate::isoweek(Dato)
    ) %>%
    dplyr::select(Dato, Runde, week, Hjemmelag, Bortelag, Hjemme, Borte, Turnering) %>%
    dplyr::rename(
      team_a  = Hjemmelag,
      team_b  = Bortelag,
      score_a = Hjemme,
      score_b = Borte
    ) %>%
    dplyr::arrange(Dato) %>%
    dplyr::mutate(match = dplyr::row_number())

  date_season_start <- min(matches$Dato, na.rm = TRUE) - lubridate::weeks(1)

  # ------------------------------------------------------------------
  # Step 2: Long-format team stats by round
  # ------------------------------------------------------------------
  team_stats_by_round <- matches %>%
    dplyr::mutate(
      points_a       = dplyr::case_when(score_a > score_b ~ 3, score_a == score_b ~ 1, TRUE ~ 0),
      points_b       = dplyr::case_when(score_b > score_a ~ 3, score_a == score_b ~ 1, TRUE ~ 0),
      gd_a           = score_a - score_b,
      gd_b           = score_b - score_a,
      score_against_a = score_b * -1,
      score_against_b = score_b * -1,
      opponent_a     = team_b,
      opponent_b     = team_a
    ) %>%
    tidyr::pivot_longer(
      cols         = c(team_a, team_b, opponent_a, opponent_b,
                       score_a, score_b, score_against_a, score_against_b,
                       gd_a, gd_b, points_a, points_b),
      names_to     = c(".value", "team_type"),
      names_pattern = "(.*)_(a|b)"
    ) %>%
    dplyr::arrange(match) %>%
    dplyr::group_by(team, Turnering) %>%
    tidyr::complete(
      week = tidyr::full_seq(
        lubridate::isoweek(date_season_start):max(week), 1
      ),
      fill = list(team_type = "bye")
    ) %>%
    dplyr::mutate(
      games_played         = cumsum(dplyr::if_else(team_type %in% c("a", "b"), 1, 0)),
      total_points         = cumsum(ifelse(is.na(points), 0, points)),
      total_gd             = cumsum(ifelse(is.na(gd), 0, gd)),
      total_scored         = cumsum(ifelse(is.na(score), 0, score)),
      total_scored_against = cumsum(ifelse(is.na(score_against), 0, score_against)),
      total_points_home    = cumsum(dplyr::if_else(team_type == "a", points, 0L)),
      total_points_away    = cumsum(dplyr::if_else(team_type == "b", points, 0L)),
      total_gd_home        = cumsum(dplyr::if_else(team_type == "a", gd, 0L)),
      total_gd_away        = cumsum(dplyr::if_else(team_type == "b", gd, 0L))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      strength_score_static   = round(total_points + (total_gd / games_played), 2),
      strength_score_home     = round(total_points_home + (total_gd_home / games_played), 2),
      strength_score_away     = round(total_points_away + (total_gd_away / games_played), 2),
      strength_score_home_away = round((strength_score_home + strength_score_away) / 2, 2),
      strength_score          = strength_score_static
    )

  # ------------------------------------------------------------------
  # Step 3: Add strength scores to matches
  # ------------------------------------------------------------------
  matches_with_strength <- matches %>%
    dplyr::left_join(
      team_stats_by_round %>% dplyr::select(team, strength_score, match),
      by = c("team_a" = "team", "match" = "match")
    ) %>%
    dplyr::rename(strength_a = strength_score) %>%
    dplyr::left_join(
      team_stats_by_round %>% dplyr::select(team, strength_score, match),
      by = c("team_b" = "team", "match" = "match")
    ) %>%
    dplyr::rename(strength_b = strength_score) %>%
    dplyr::mutate(
      opponent_strength_a = strength_b,
      opponent_strength_b = strength_a
    )

  # Preserve multi-tournament season ordering (earlier seasons processed first)
  season_order <- matches_with_strength %>%
    dplyr::group_by(Turnering) %>%
    dplyr::summarise(season_start = min(Dato, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(season_start) %>%
    dplyr::pull(Turnering)

  matches_with_strength <- matches_with_strength %>%
    dplyr::mutate(Turnering = factor(Turnering, levels = season_order)) %>%
    dplyr::arrange(Turnering, Dato, match)

  # ------------------------------------------------------------------
  # Step 4: Iterative Elo update loop
  # ------------------------------------------------------------------
  initial_elo <- matches_with_strength %>%
    dplyr::select(team_a) %>%
    dplyr::distinct() %>%
    dplyr::rename(team = team_a) %>%
    dplyr::mutate(elo_rating = 1500)

  elo_ratings <- initial_elo
  predicted_outcomes <- NULL
  updated_ratings    <- NULL

  for (i in seq_len(nrow(matches_with_strength))) {
    current_match <- matches_with_strength[i, ]
    rating_a <- elo_ratings$elo_rating[match(current_match$team_a, elo_ratings$team)]
    rating_b <- elo_ratings$elo_rating[match(current_match$team_b, elo_ratings$team)]

    if (is.na(current_match$score_a) || is.na(current_match$score_b)) {
      predicted_outcomes <- update_elo(rating_a, rating_b, k_factor = k_factor)
    } else {
      updated_ratings <- update_elo(
        rating_a, rating_b,
        current_match$score_a, current_match$score_b,
        k_factor = k_factor
      )
      elo_ratings <- elo_ratings %>%
        dplyr::mutate(
          elo_rating = dplyr::case_when(
            team == current_match$team_a ~ updated_ratings$rating1_new,
            team == current_match$team_b ~ updated_ratings$rating2_new,
            TRUE ~ elo_rating
          )
        )
      matches_with_strength[i, "elo_new_a"] <- updated_ratings$rating1_new
      matches_with_strength[i, "elo_new_b"] <- updated_ratings$rating2_new
    }

    matches_with_strength[i, "elo_outcome_a"] <- ifelse(
      is.na(current_match$score_a),
      predicted_outcomes$expected_elo_score_a,
      updated_ratings$expected_elo_score_a
    )
    matches_with_strength[i, "elo_outcome_b"] <- ifelse(
      is.na(current_match$score_b),
      predicted_outcomes$expected_elo_score_b,
      updated_ratings$expected_elo_score_b
    )
  }

  # ------------------------------------------------------------------
  # Step 5: Enrich team stats with Elo data + projections
  # ------------------------------------------------------------------
  team_stats_by_round_w_strength <- team_stats_by_round %>%
    dplyr::left_join(
      matches_with_strength %>%
        dplyr::select(Runde, match, strength_a, strength_b,
                      elo_new_a, elo_new_b, elo_outcome_a, elo_outcome_b),
      by = c("Runde", "match")
    ) %>%
    dplyr::mutate(
      team_strength      = dplyr::case_when(team_type == "a" ~ strength_a, TRUE ~ strength_b),
      opponent_strength  = dplyr::case_when(team_type == "a" ~ strength_b, TRUE ~ strength_a),
      team_elo           = dplyr::case_when(team_type == "a" ~ elo_new_a,  TRUE ~ elo_new_b),
      opponent_elo       = dplyr::case_when(team_type == "a" ~ elo_new_b,  TRUE ~ elo_new_a),
      team_prediction_elo = dplyr::case_when(team_type == "a" ~ elo_outcome_a, TRUE ~ elo_outcome_b),
      Dato = dplyr::case_when(
        week == lubridate::isoweek(date_season_start) ~ date_season_start,
        TRUE ~ Dato
      ),
      net_strength_difference = team_strength - opponent_strength,
      expected_Performance    = team_strength / (team_strength + opponent_strength),
      over_performance        = net_strength_difference / opponent_strength,
      points_status = dplyr::case_when(
        is.na(score) & week == lubridate::isoweek(date_season_start) ~ "Played",
        !is.na(score) | week == lubridate::isoweek(date_season_start) ~ "Played",
        is.na(score) & team_type != "bye" ~ "Projected"
      ),
      points = dplyr::case_when(
        !is.na(score) ~ points,
        TRUE ~ dplyr::case_when(
          team_prediction_elo >= 0.55 ~ 3L,
          team_prediction_elo >= 0.45 ~ 1L,
          TRUE ~ 0L
        )
      )
    ) %>%
    dplyr::group_by(team, Turnering) %>%
    dplyr::mutate(
      games_played         = cumsum(dplyr::if_else(team_type %in% c("a", "b"), 1, 0)),
      total_points         = cumsum(ifelse(is.na(points), 0, points)),
      points_per_game      = round(total_points / games_played, 2),
      points_per_game      = dplyr::case_when(
        week == lubridate::isoweek(date_season_start) ~ 0,
        TRUE ~ points_per_game
      ),
      total_gd             = cumsum(ifelse(is.na(gd), 0, gd)),
      total_scored         = cumsum(ifelse(is.na(score), 0, score)),
      total_scored_against = cumsum(ifelse(is.na(score_against), 0, score_against)),
      total_points_home    = cumsum(dplyr::if_else(team_type == "a", points, 0L)),
      total_points_away    = cumsum(dplyr::if_else(team_type == "b", points, 0L)),
      total_gd_home        = cumsum(dplyr::if_else(team_type == "a", gd, 0L)),
      total_gd_away        = cumsum(dplyr::if_else(team_type == "b", gd, 0L)),
      game_outcome = dplyr::case_when(
        points == 3 & team_type != "bye" ~ "Win",
        points == 1 & team_type != "bye" ~ "Draw",
        points == 0 & team_type != "bye" ~ "Loss",
        team_type == "bye"               ~ "Bye"
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      normalized_prediction = dplyr::case_when(
        points == 3 ~ team_prediction_elo * 3,
        points == 1 ~ 1.5,
        points == 0 ~ team_prediction_elo * 3
      ),
      prediction_distance = dplyr::case_when(
        points == 1 ~ abs(team_prediction_elo - 0.5),
        TRUE        ~ abs(normalized_prediction - points)
      ),
      prediction_gradient = dplyr::case_when(
        points == 1 & team_prediction_elo == 0.5 ~ 1,
        points == 1 ~ 0.5 - (prediction_distance * 3 / 3),
        TRUE        ~ 1 - prediction_distance / 3
      ),
      prediction_gradient = dplyr::case_when(
        points_status == "Played" ~ prediction_gradient,
        TRUE ~ NA_real_
      )
    ) %>%
    dplyr::group_by(team, week) %>%
    dplyr::mutate(game_number = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      week         = factor(week, levels = sort(unique(as.numeric(week)), decreasing = TRUE)),
      opponent_short = stringr::str_trunc(opponent, width = 9, side = "right", ellipsis = "..."),
      week_game    = paste0("week ", week, " game ", game_number),
      week_game    = factor(week_game, levels = sort(unique(week_game), decreasing = TRUE)),
      points_fill  = dplyr::case_when(
        points_status == "Played"    ~ points_per_game,
        points_status == "Projected" ~ NA_real_
      )
    )

  # ------------------------------------------------------------------
  # Step 6: Aggregate rankings
  # ------------------------------------------------------------------
  team_rankings <- team_stats_by_round_w_strength %>%
    dplyr::group_by(team) %>%
    dplyr::summarize(
      total_points          = sum(points, na.rm = TRUE),
      last_elo              = dplyr::last(team_elo),
      max_elo               = max(team_elo, na.rm = TRUE),
      games_played          = dplyr::n(),
      avg_points_per_game   = total_points / games_played,
      total_gd              = sum(gd, na.rm = TRUE),
      avg_opponent_strength = mean(opponent_strength, na.rm = TRUE),
      avg_strength_diff     = mean(net_strength_difference, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      ranking_score_1 = total_points * 2 + total_gd,
      ranking_score_2 = total_points * 2 + total_gd + avg_opponent_strength
    ) %>%
    dplyr::arrange(desc(total_points))

  # ------------------------------------------------------------------
  # Step 7: Endpoint snapshots (used by figures & tables)
  # ------------------------------------------------------------------
  end_points_projected <- team_stats_by_round_w_strength %>%
    dplyr::group_by(team) %>%
    dplyr::slice_max(order_by = match, n = 1) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(-total_points, -strength_score_static, -total_gd, team)

  end_points_played <- team_stats_by_round_w_strength %>%
    dplyr::filter(points_status == "Played") %>%
    dplyr::group_by(team) %>%
    dplyr::slice_max(order_by = match, n = 1) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(-total_points, -strength_score_static, -total_gd, team)

  end_points_projected_wl <- team_stats_by_round_w_strength %>%
    dplyr::filter(team_type != "bye") %>%
    dplyr::group_by(team) %>%
    dplyr::summarise(
      win  = sum(points == 3),
      draw = sum(points == 1),
      loss = sum(points == 0),
      .groups = "drop"
    )

  end_points_played_wl <- team_stats_by_round_w_strength %>%
    dplyr::filter(team_type != "bye", points_status == "Played") %>%
    dplyr::group_by(team) %>%
    dplyr::summarise(
      win  = sum(points == 3),
      draw = sum(points == 1),
      loss = sum(points == 0),
      .groups = "drop"
    )

  date_minimum <- min(team_stats_by_round_w_strength$Dato, na.rm = TRUE) - lubridate::weeks(1)
  date_maximum <- max(team_stats_by_round_w_strength$Dato, na.rm = TRUE) + lubridate::weeks(1)

  lines_data <- end_points_played %>%
    dplyr::group_by(Turnering) %>%
    dplyr::summarise(
      points_min = min(total_points),
      points_max = max(total_points),
      elo_min    = min(team_elo),
      elo_max    = max(team_elo),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      slope     = (points_max - points_min) / (elo_max - elo_min),
      intercept = points_min - slope * elo_min
    )

  # ------------------------------------------------------------------
  # Return everything downstream layers need
  # ------------------------------------------------------------------
  list(
    team_stats            = team_stats_by_round_w_strength,
    matches               = matches_with_strength,
    team_rankings         = team_rankings,
    end_points_played     = end_points_played,
    end_points_projected  = end_points_projected,
    end_points_played_wl  = end_points_played_wl,
    end_points_projected_wl = end_points_projected_wl,
    lines_data            = lines_data,
    date_minimum          = date_minimum,
    date_maximum          = date_maximum,
    tournament            = paste(unique(as.character(matches$Turnering)), collapse = " + "),
    date_season_start     = date_season_start,
    k_factor              = k_factor
  )
}
