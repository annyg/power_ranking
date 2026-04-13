# R/make_figures.R
# Generates all ggplot2 figures from the output of run_rankings().
# Returns a named list of ggplot objects.
#
# ANYthings brand applied:
#   Colors: #1A1A1A (black), #333333 (body), #767676 (grey), #D9D9D9 (grid),
#           #E03C31 (signal red accent), #0B3D91 (system blue)
#   Typography: Inter (web), Arial (fallback)

library(ggplot2)
library(dplyr)
library(stringr)
library(ggrepel)

# ---------------------------------------------------------------------------
# ANYthings ggplot2 theme (inline fallback — use library(anythings) if available)
# ---------------------------------------------------------------------------

theme_anythings_football <- function(base_size = 11) {
  # Prefer Inter, then Arial (if registered), then "sans" (Helvetica/PostScript native)
  base_family <- tryCatch({
    fonts <- extrafont::fonttable()
    if (any(fonts$FamilyName == "Inter"))  "Inter"
    else if (any(fonts$FamilyName == "Arial")) "Arial"
    else "sans"
  }, error = function(e) "sans")
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      text             = ggplot2::element_text(colour = "#333333"),
      plot.title       = ggplot2::element_text(colour = "#1A1A1A", face = "bold",
                                               size = base_size + 3, margin = ggplot2::margin(b = 6)),
      plot.subtitle    = ggplot2::element_text(colour = "#767676", size = base_size,
                                               margin = ggplot2::margin(b = 10)),
      plot.caption     = ggplot2::element_text(colour = "#767676", size = base_size - 2,
                                               hjust = 0, margin = ggplot2::margin(t = 8)),
      axis.text        = ggplot2::element_text(colour = "#767676", size = base_size - 1),
      axis.title       = ggplot2::element_text(colour = "#333333", size = base_size),
      panel.grid.major = ggplot2::element_line(colour = "#D9D9D9", linewidth = 0.4),
      panel.grid.minor = ggplot2::element_blank(),
      legend.text      = ggplot2::element_text(colour = "#333333", size = base_size - 1),
      legend.title     = ggplot2::element_text(colour = "#333333", face = "bold",
                                               size = base_size - 1),
      legend.position  = "bottom",
      legend.key.size  = ggplot2::unit(0.4, "cm"),
      strip.text       = ggplot2::element_text(colour = "#1A1A1A", face = "bold",
                                               size = base_size),
      plot.background  = ggplot2::element_rect(fill = "#FFFFFF", colour = NA),
      panel.background = ggplot2::element_rect(fill = "#FFFFFF", colour = NA),
      plot.margin      = ggplot2::margin(12, 16, 8, 12)
    )
}

# Multi-team color scale: functional differentiation palette (up to 20 teams)
# Preserves ANYthings' NYCTA-inspired data visualization intent
scale_colour_football <- function(...) {
  ggsci::scale_colour_d3(palette = "category20", ...)
}
scale_fill_football <- function(...) {
  ggsci::scale_fill_d3(palette = "category20", ...)
}

# Prediction accuracy gradient: red (unpredicted) → white (neutral) → green (predicted)
prediction_fill_scale <- function() {
  ggplot2::scale_fill_gradient2(
    low      = "#E03C31",  # ANYthings Signal Red
    mid      = "#F5F5F5",  # near-white neutral
    high     = "#2E7D32",  # dark green for confirmed predictions
    midpoint = 0.5,
    limits   = c(0, 1),
    breaks   = c(0, 0.5, 1),
    labels   = c("Unpredicted", "Neutral", "Predicted"),
    name     = "Prediction",
    na.value = "#D9D9D9"
  )
}

# ---------------------------------------------------------------------------
# Main function
# ---------------------------------------------------------------------------

#' Build all dashboard figures from ranked data.
#'
#' @param data Named list returned by run_rankings().
#' @return Named list of ggplot2 objects.
make_figures <- function(data) {

  ts        <- data$team_stats          # team_stats_by_round_w_strength
  ep_played <- data$end_points_played
  ep_proj   <- data$end_points_projected
  lines_d   <- data$lines_data
  d_min     <- data$date_minimum
  d_max     <- data$date_maximum

  # Team / round counts for dynamic sizing
  n_teams  <- dplyr::n_distinct(ep_played$team)
  n_rounds <- dplyr::n_distinct(ts$week_game[!is.na(ts$opponent)])

  # Heatmap text sizes: scale down as more teams/rounds are added
  #   heat_main: outcome letter (W/D/L), tile centre
  #   heat_sub:  opponent short name + ELO probability / PPG, nudged above/below
  heat_main_size <- pmax(1.8, pmin(3.5, 21 / n_teams))
  heat_sub_size  <- pmax(1.2, pmin(2.5, 15 / n_teams))

  # Extra right-side margin so labels don't get clipped — scales with longest name
  right_margin_pt <- max(nchar(as.character(unique(ts$team)))) * 5 + 20

  x_scale <- ggplot2::scale_x_date(
    date_breaks = "1 week",
    date_labels = "%b %d",
    limits      = c(d_min, d_max),
    expand      = ggplot2::expansion(mult = c(0.01, 0.18))
  )

  # Shared outcome shape + status linetype scales (reused across line charts)
  outcome_shapes <- c(Win = 17, Draw = 16, Loss = 4, Bye = NA_integer_)
  scale_outcome  <- ggplot2::scale_shape_manual(
    values        = outcome_shapes,
    na.translate  = FALSE,
    name          = "Outcome"
  )
  scale_status   <- ggplot2::scale_linetype_manual(
    values        = c(Played = "solid", Projected = "dashed"),
    na.translate  = FALSE,
    name          = "Status"
  )
  rotated_x <- ggplot2::theme(
    axis.text.x  = ggplot2::element_text(angle = 45, hjust = 1),
    plot.margin  = ggplot2::margin(12, right_margin_pt, 8, 12)
  )

  # ELO with 1500 anchor: fill initial NA per team with 1500, then carry forward
  # so bye-week gaps also show the last known ELO rather than a gap in the line
  ts_elo <- ts %>%
    dplyr::group_by(team) %>%
    dplyr::arrange(Dato) %>%
    dplyr::mutate(
      team_elo = dplyr::if_else(
        is.na(team_elo) & dplyr::row_number() == 1L, 1500, team_elo
      )
    ) %>%
    tidyr::fill(team_elo, .direction = "down") %>%
    dplyr::ungroup()

  # ---- 1. ELO ranking over time -------------------------------------------
  fig_elo <- ggplot2::ggplot(ts_elo, ggplot2::aes(x = Dato, y = team_elo, colour = team)) +
    ggplot2::geom_line(linewidth = 0.6, alpha = 0.7) +
    ggplot2::geom_line(ggplot2::aes(linetype = points_status), linewidth = 1,
                       alpha = 0.9, show.legend = TRUE) +
    ggplot2::geom_point(ggplot2::aes(shape = game_outcome), size = 2.5, alpha = 0.8) +
    ggrepel::geom_label_repel(
      data           = ep_played,
      ggplot2::aes(label = team),
      direction      = "y",
      hjust          = "left",
      nudge_x        = 7,
      fontface       = "bold",
      size           = 2.8,
      fill           = "#F5F5F5",
      alpha          = 0.9,
      label.size     = 0.3,
      label.padding  = ggplot2::unit(0.18, "lines"),
      segment.colour = "#D9D9D9",
      segment.size   = 0.3,
      show.legend    = FALSE
    ) +
    ggplot2::scale_y_continuous(
      breaks       = seq(1000, 2000, by = 50),
      minor_breaks = NULL
    ) +
    x_scale +
    scale_outcome +
    scale_status +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(
      title    = "ELO Rating Over Season",
      subtitle = "Solid = played, dashed = projected  ·  all teams start at 1500",
      x = NULL, y = "ELO Rating"
    ) +
    scale_colour_football(guide = "none") +
    theme_anythings_football() +
    rotated_x

  # ---- 2. Cumulative points over time --------------------------------------
  fig_points <- ggplot2::ggplot(ts, ggplot2::aes(x = Dato, y = total_points, colour = team)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "longdash", colour = "#D9D9D9") +
    ggplot2::geom_line(linetype = "dashed", linewidth = 0.8, alpha = 0.3) +
    ggplot2::geom_line(ggplot2::aes(linetype = points_status), linewidth = 1,
                       show.legend = TRUE) +
    ggplot2::geom_point(ggplot2::aes(shape = game_outcome), size = 2.5, alpha = 0.8) +
    ggrepel::geom_label_repel(
      data           = ep_proj,
      ggplot2::aes(label = team),
      direction      = "y",
      hjust          = "left",
      nudge_x        = 7,
      fontface       = "bold",
      size           = 2.8,
      fill           = "#F5F5F5",
      alpha          = 0.9,
      label.size     = 0.3,
      label.padding  = ggplot2::unit(0.18, "lines"),
      segment.colour = "#D9D9D9",
      segment.size   = 0.3,
      show.legend    = FALSE
    ) +
    ggplot2::scale_y_continuous(
      breaks       = seq(0, 100, by = 3),
      limits       = c(0, NA),
      minor_breaks = NULL
    ) +
    x_scale +
    scale_outcome +
    scale_status +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(
      title    = "Cumulative Points",
      subtitle = "Dashed = projected final standings",
      x = NULL, y = "Total Points"
    ) +
    scale_colour_football(guide = "none") +
    theme_anythings_football() +
    rotated_x

  # ---- 3. Points-per-game over time ----------------------------------------
  fig_ppg <- ggplot2::ggplot(ts, ggplot2::aes(x = Dato, y = points_per_game, colour = team)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "longdash", colour = "#D9D9D9") +
    ggplot2::geom_line(linetype = "dashed", linewidth = 0.8, alpha = 0.3) +
    ggplot2::geom_line(ggplot2::aes(linetype = points_status), linewidth = 1,
                       show.legend = TRUE) +
    ggplot2::geom_point(ggplot2::aes(shape = game_outcome), size = 2.5, alpha = 0.8) +
    ggrepel::geom_label_repel(
      data           = ep_proj,
      ggplot2::aes(label = team),
      direction      = "y",
      hjust          = "left",
      nudge_x        = 7,
      fontface       = "bold",
      size           = 2.8,
      fill           = "#F5F5F5",
      alpha          = 0.9,
      label.size     = 0.3,
      label.padding  = ggplot2::unit(0.18, "lines"),
      segment.colour = "#D9D9D9",
      segment.size   = 0.3,
      show.legend    = FALSE
    ) +
    ggplot2::scale_y_continuous(
      breaks       = seq(0, 3, by = 0.5),
      limits       = c(0, 3.2),
      minor_breaks = NULL
    ) +
    x_scale +
    scale_outcome +
    scale_status +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(
      title    = "Points Per Game",
      subtitle = "Normalised for games played — dashed = projected",
      x = NULL, y = "PPG"
    ) +
    scale_colour_football(guide = "none") +
    theme_anythings_football() +
    rotated_x

  # ---- 4. ELO vs Points scatter --------------------------------------------
  fig_comp <- ggplot2::ggplot(ep_played, ggplot2::aes(x = team_elo, y = total_points)) +
    ggplot2::geom_abline(
      data      = lines_d,
      ggplot2::aes(slope = slope, intercept = intercept),
      linetype  = "dashed",
      colour    = "#767676",
      linewidth = 0.5
    ) +
    ggplot2::geom_point(ggplot2::aes(colour = team), size = 4, alpha = 0.85) +
    ggrepel::geom_label_repel(
      ggplot2::aes(label = team, colour = team),
      vjust       = 1.8,
      hjust       = 0.5,
      fontface    = "bold",
      size        = 3,
      show.legend = FALSE
    ) +
    ggplot2::annotate("text", x = -Inf, y = Inf,
                      label = "Overvalued", hjust = -0.3, vjust = 1.4,
                      size = 3, colour = "#767676") +
    ggplot2::annotate("text", x = Inf, y = -Inf,
                      label = "Undervalued", hjust = 1.2, vjust = -0.8,
                      size = 3, colour = "#767676") +
    ggplot2::scale_x_continuous(breaks = seq(1000, 2000, by = 50), minor_breaks = NULL) +
    ggplot2::scale_y_continuous(breaks = seq(0, 60, by = 1), minor_breaks = NULL) +
    ggplot2::labs(
      title    = "ELO vs Points",
      subtitle = "Teams above the line are over-performing their ELO; below = under-performing",
      x = "ELO Rating", y = "Total Points"
    ) +
    scale_colour_football(guide = "none") +
    theme_anythings_football()

  # ---- 5. Heatmap: per-round outcomes + ELO predictions -------------------
  fig_heatmap_round <- ts %>%
    dplyr::filter(!is.na(opponent)) %>%
    dplyr::mutate(team = factor(team, levels = ep_played$team)) %>%
    ggplot2::ggplot(ggplot2::aes(y = week_game, x = team)) +
    ggplot2::geom_tile(
      ggplot2::aes(fill = prediction_gradient),
      colour = "#D9D9D9", width = 0.9, height = 0.9, linewidth = 0.3
    ) +
    ggplot2::geom_text(ggplot2::aes(label = opponent_short),
                       colour = "#1A1A1A", size = heat_sub_size, nudge_y = 0.22) +
    ggplot2::geom_text(ggplot2::aes(label = game_outcome),
                       colour = "#1A1A1A", size = heat_main_size) +
    ggplot2::geom_text(ggplot2::aes(label = round(team_prediction_elo, 2)),
                       colour = "#1A1A1A", size = heat_sub_size, nudge_y = -0.22) +
    prediction_fill_scale() +
    ggplot2::labs(
      title = "Match Outcomes by Round",
      subtitle = "Opponent shown above result, ELO win probability below",
      y = "Round", x = NULL
    ) +
    theme_anythings_football() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  # ---- 6. Heatmap: team vs opponent matchup matrix ------------------------
  fig_heatmap_matchup <- ts %>%
    dplyr::filter(!is.na(opponent)) %>%
    dplyr::mutate(
      team     = factor(team,     levels = ep_played$team),
      opponent = factor(opponent, levels = ep_played$team)
    ) %>%
    ggplot2::ggplot(ggplot2::aes(x = opponent, y = team)) +
    ggplot2::geom_tile(
      ggplot2::aes(fill = prediction_gradient),
      colour = "#D9D9D9", width = 0.9, height = 0.9, linewidth = 0.3
    ) +
    ggplot2::geom_text(ggplot2::aes(label = game_outcome),
                       colour = "#1A1A1A", size = heat_main_size, nudge_y = 0.15) +
    ggplot2::geom_text(ggplot2::aes(label = round(team_prediction_elo, 2)),
                       colour = "#1A1A1A", size = heat_sub_size, nudge_y = -0.15) +
    prediction_fill_scale() +
    ggplot2::labs(
      title    = "Head-to-Head Results vs ELO Prediction",
      subtitle = "Green = result matched prediction, Red = upset",
      x = "Opponent", y = "Team"
    ) +
    theme_anythings_football() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  # ---- 7. Heatmap: PPG by round -------------------------------------------
  fig_heatmap_ppg <- ts %>%
    dplyr::filter(!is.na(opponent)) %>%
    dplyr::mutate(team = factor(team, levels = ep_played$team)) %>%
    ggplot2::ggplot(ggplot2::aes(y = week_game, x = team)) +
    ggplot2::geom_tile(
      ggplot2::aes(fill = points_fill),
      colour = "#D9D9D9", width = 0.9, height = 0.9, linewidth = 0.3
    ) +
    ggplot2::geom_text(ggplot2::aes(label = opponent_short),
                       colour = "#1A1A1A", size = heat_sub_size, nudge_y = 0.22) +
    ggplot2::geom_text(ggplot2::aes(label = game_outcome),
                       colour = "#1A1A1A", size = heat_main_size) +
    ggplot2::geom_text(ggplot2::aes(label = points_per_game),
                       colour = "#1A1A1A", size = heat_sub_size, nudge_y = -0.22) +
    ggplot2::scale_fill_gradient(
      low = "#F5F5F5", high = "#0B3D91",  # ANYthings System Blue
      na.value = "#D9D9D9",
      limits = c(0, 3),
      breaks = c(0, 1, 2, 3),
      labels = c("0", "1", "2", "3 pts"),
      name   = "PPG"
    ) +
    ggplot2::labs(
      title = "Points Per Game by Round",
      x = NULL, y = "Round"
    ) +
    theme_anythings_football() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  # ---- 8. NEW: Attack vs Defense profile ----------------------------------
  # Goals conceded/game (x) vs goals scored/game (y) — quadrant analysis
  attack_defense_data <- ep_played %>%
    dplyr::mutate(
      goals_scored_pg   = round(total_scored              / games_played, 2),
      goals_conceded_pg = round(abs(total_scored_against) / games_played, 2)
    )

  midpoint_x <- median(attack_defense_data$goals_conceded_pg, na.rm = TRUE)
  midpoint_y <- median(attack_defense_data$goals_scored_pg,   na.rm = TRUE)
  axis_max   <- max(
    max(attack_defense_data$goals_scored_pg,   na.rm = TRUE),
    max(attack_defense_data$goals_conceded_pg, na.rm = TRUE)
  ) * 1.05  # 5% headroom so top labels aren't clipped

  fig_attack_defense <- ggplot2::ggplot(
    attack_defense_data,
    ggplot2::aes(x = goals_conceded_pg, y = goals_scored_pg, colour = team)
  ) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "#767676", linewidth = 0.5) +
    ggplot2::geom_hline(yintercept = midpoint_y, linetype = "dashed", colour = "#D9D9D9") +
    ggplot2::geom_vline(xintercept = midpoint_x, linetype = "dashed", colour = "#D9D9D9") +
    ggplot2::annotate("text", x = -Inf, y = -Inf,
                      label = "Few conceded\nFew scored", hjust = -0.1, vjust = -0.3,
                      size = 3, colour = "#767676") +
    ggplot2::annotate("text", x = Inf, y = -Inf,
                      label = "Many conceded\nFew scored", hjust = 1.1, vjust = -0.3,
                      size = 3, colour = "#E03C31", fontface = "bold") +
    ggplot2::annotate("text", x = -Inf, y = Inf,
                      label = "Few conceded\nMany scored", hjust = -0.1, vjust = 1.4,
                      size = 3, colour = "#2E7D32", fontface = "bold") +
    ggplot2::annotate("text", x = Inf, y = Inf,
                      label = "Many conceded\nMany scored", hjust = 1.1, vjust = 1.4,
                      size = 3, colour = "#767676") +
    ggplot2::geom_point(size = 5, alpha = 0.85) +
    ggplot2::geom_text(
      ggplot2::aes(label = team),
      vjust    = 1.8,
      hjust    = 0.5,
      fontface = "bold",
      size     = 3,
      colour   = "#333333"
    ) +
    ggplot2::scale_x_continuous(
      breaks       = seq(0, 20, by = 0.5),
      minor_breaks = NULL,
      limits       = c(0, axis_max)
    ) +
    ggplot2::scale_y_continuous(
      breaks       = seq(0, 20, by = 0.5),
      minor_breaks = NULL,
      limits       = c(0, axis_max)
    ) +
    ggplot2::labs(
      title    = "Attack vs Defence Profile",
      subtitle = "Goals conceded per game (x) vs goals scored per game (y)  ·  above diagonal = positive goal difference",
      x = "Goals conceded / game",
      y = "Goals scored / game"
    ) +
    scale_colour_football(guide = "none") +
    theme_anythings_football()

  # ---- 9. NEW: Home vs Away PPG bar chart ---------------------------------
  home_away_data <- ep_played %>%
    dplyr::mutate(
      games_home  = pmax(total_points_home / 3, 1),   # rough game count
      games_away  = pmax(total_points_away / 3, 1),
      ppg_home    = round(total_points_home / games_home, 2),
      ppg_away    = round(total_points_away / games_away, 2)
    ) %>%
    dplyr::select(team, ppg_home, ppg_away) %>%
    tidyr::pivot_longer(
      cols      = c(ppg_home, ppg_away),
      names_to  = "venue",
      values_to = "ppg"
    ) %>%
    dplyr::mutate(
      venue = dplyr::recode(venue, ppg_home = "Home", ppg_away = "Away"),
      team  = factor(team, levels = rev(ep_played$team))
    )

  fig_home_away <- ggplot2::ggplot(
    home_away_data,
    ggplot2::aes(y = team, x = ppg, fill = venue)
  ) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.7),
                      width = 0.65, alpha = 0.9) +
    ggplot2::scale_fill_manual(
      values = c(Home = "#0B3D91", Away = "#767676"),  # blue for home, grey for away
      name   = NULL
    ) +
    ggplot2::scale_x_continuous(
      limits       = c(0, 3.2),
      breaks       = seq(0, 3, by = 0.5),
      minor_breaks = NULL
    ) +
    ggplot2::labs(
      title    = "Home vs Away Performance",
      subtitle = "Points per game split by venue",
      x = "Points per game", y = NULL
    ) +
    theme_anythings_football() +
    ggplot2::theme(legend.position = "top")

  # ---- 10. NEW: ELO momentum (most recent round change) ------------------
  elo_momentum <- ts %>%
    dplyr::filter(team_type %in% c("a", "b"), !is.na(team_elo)) %>%
    dplyr::arrange(team, match) %>%
    dplyr::group_by(team) %>%
    dplyr::mutate(
      elo_prev  = dplyr::lag(team_elo),
      elo_delta = team_elo - elo_prev
    ) %>%
    dplyr::filter(!is.na(elo_delta)) %>%
    dplyr::slice_max(order_by = match, n = 1) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(elo_delta) %>%
    dplyr::mutate(
      team      = factor(team, levels = team),
      direction = ifelse(elo_delta >= 0, "Gained", "Lost")
    )

  fig_elo_momentum <- ggplot2::ggplot(
    elo_momentum,
    ggplot2::aes(y = team, x = elo_delta, fill = direction)
  ) +
    ggplot2::geom_col(alpha = 0.9) +
    ggplot2::geom_vline(xintercept = 0, colour = "#1A1A1A", linewidth = 0.5) +
    ggplot2::scale_fill_manual(
      values = c(Gained = "#2E7D32", Lost = "#E03C31"),
      guide  = "none"
    ) +
    ggplot2::scale_x_continuous(minor_breaks = NULL) +
    ggplot2::labs(
      title    = "ELO Momentum",
      subtitle = "Rating change in the most recent round played",
      x = "ELO change", y = NULL
    ) +
    theme_anythings_football()

  # Return all figures as a named list
  list(
    fig_elo            = fig_elo,
    fig_points         = fig_points,
    fig_ppg            = fig_ppg,
    fig_comp           = fig_comp,
    fig_heatmap_round  = fig_heatmap_round,
    fig_heatmap_matchup = fig_heatmap_matchup,
    fig_heatmap_ppg    = fig_heatmap_ppg,
    fig_attack_defense = fig_attack_defense,
    fig_home_away      = fig_home_away,
    fig_elo_momentum   = fig_elo_momentum
  )
}
