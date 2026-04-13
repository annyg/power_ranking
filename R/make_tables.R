# R/make_tables.R
# Generates all GT tables from the output of run_rankings().
# Returns a named list of gt table objects.
#
# ANYthings brand applied via gt styling:
#   Header bg: #1A1A1A, Header text: white
#   Borders: #D9D9D9, Alternate rows: #F5F5F5
#   Accent: #E03C31 (Signal Red) for top/bottom positions
#   Font: Inter / Arial

library(dplyr)
library(gt)

# ---------------------------------------------------------------------------
# Shared GT styling helper
# ---------------------------------------------------------------------------

style_gt_anythings <- function(tbl, title = NULL, subtitle = NULL) {
  tbl <- tbl %>%
    gt::tab_options(
      table.font.names          = c("Inter", "Arial", "sans-serif"),
      table.font.size           = gt::px(12),
      table.background.color    = "#FFFFFF",
      table.border.top.color    = "#1A1A1A",
      table.border.top.width    = gt::px(2),
      table.border.bottom.color = "#D9D9D9",
      column_labels.font.weight = "bold",
      column_labels.background.color = "#1A1A1A",
      column_labels.border.bottom.color = "#D9D9D9",
      row.striping.background_color = "#F5F5F5",
      row.striping.include_stub  = TRUE,
      row_group.background.color = "#F5F5F5",
      row_group.font.weight      = "bold",
      data_row.padding           = gt::px(6),
      source_notes.font.size     = gt::px(10)
    ) %>%
    gt::opt_row_striping() %>%
    gt::tab_style(
      style     = gt::cell_text(color = "#FFFFFF"),
      locations = gt::cells_column_labels()
    ) %>%
    gt::tab_style(
      style     = gt::cell_text(color = "#767676"),
      locations = gt::cells_source_notes()
    )

  if (!is.null(title)) {
    tbl <- tbl %>%
      gt::tab_header(title = title, subtitle = subtitle) %>%
      gt::tab_style(
        style = gt::cell_text(color = "#1A1A1A", weight = "bold", size = gt::px(14)),
        locations = gt::cells_title(groups = "title")
      ) %>%
      gt::tab_style(
        style = gt::cell_text(color = "#767676", size = gt::px(11)),
        locations = gt::cells_title(groups = "subtitle")
      )
  }

  tbl
}

# ---------------------------------------------------------------------------
# Main function
# ---------------------------------------------------------------------------

#' Build all dashboard tables from ranked data.
#'
#' @param data Named list returned by run_rankings().
#' @return Named list of gt table objects.
make_tables <- function(data) {

  ts         <- data$team_stats
  ep_played  <- data$end_points_played
  ep_proj    <- data$end_points_projected
  wl_played  <- data$end_points_played_wl
  wl_proj    <- data$end_points_projected_wl
  tournament <- data$tournament

  # ------------------------------------------------------------------
  # 1. Current standings (played games only)
  # ------------------------------------------------------------------
  tbl_current_df <- ep_played %>%
    dplyr::left_join(wl_played, by = "team") %>%
    dplyr::select(team, games_played, win, draw, loss,
                  total_gd, total_points, points_per_game, team_elo) %>%
    dplyr::arrange(-total_points, -total_gd, -team_elo) %>%
    dplyr::mutate(rank = dplyr::row_number(), .before = 1) %>%
    dplyr::rename(
      Rank   = rank,
      Team   = team,
      GP     = games_played,
      W      = win,
      D      = draw,
      L      = loss,
      GD     = total_gd,
      Pts    = total_points,
      PPG    = points_per_game,
      ELO    = team_elo
    )

  n_teams <- nrow(tbl_current_df)

  tbl_current <- gt::gt(tbl_current_df) %>%
    style_gt_anythings(
      title    = "Current Standings",
      subtitle = tournament
    ) %>%
    gt::fmt_number(columns = c(ELO), decimals = 0) %>%
    gt::fmt_number(columns = c(PPG),  decimals = 2) %>%
    gt::cols_align(align = "right",  columns = c(GP, W, D, L, GD, Pts, PPG, ELO)) %>%
    gt::cols_align(align = "left",   columns = Team) %>%
    gt::cols_width(Team ~ gt::px(140), Rank ~ gt::px(40),
                   GP ~ gt::px(40), W ~ gt::px(35), D ~ gt::px(35), L ~ gt::px(35)) %>%
    # Highlight top 2 (promotion zone) in system blue
    gt::tab_style(
      style     = gt::cell_text(color = "#0B3D91", weight = "bold"),
      locations = gt::cells_body(rows = Rank <= 2)
    ) %>%
    # Highlight bottom 2 (relegation zone) in signal red
    gt::tab_style(
      style     = gt::cell_text(color = "#E03C31", weight = "bold"),
      locations = gt::cells_body(rows = Rank >= (n_teams - 1))
    ) %>%
    gt::tab_spanner(label = "Record", columns = c(W, D, L)) %>%
    gt::tab_source_note(
      gt::md(paste0("ELO starting rating: 1,500 | K-factor: ", data$k_factor))
    )

  # ------------------------------------------------------------------
  # 2. Projected standings
  # ------------------------------------------------------------------
  tbl_projected_df <- ep_proj %>%
    dplyr::left_join(wl_proj, by = "team") %>%
    dplyr::select(team, games_played, win, draw, loss,
                  total_gd, total_points, points_per_game, team_elo) %>%
    dplyr::arrange(-total_points, -total_gd, -team_elo) %>%
    dplyr::mutate(rank = dplyr::row_number(), .before = 1) %>%
    dplyr::rename(
      Rank = rank, Team = team, GP = games_played,
      W = win, D = draw, L = loss,
      GD = total_gd, Pts = total_points, PPG = points_per_game, ELO = team_elo
    )

  tbl_projected <- gt::gt(tbl_projected_df) %>%
    style_gt_anythings(
      title    = "Projected Final Standings",
      subtitle = paste0(tournament, " — unplayed matches estimated from current ELO")
    ) %>%
    gt::fmt_number(columns = c(ELO), decimals = 0) %>%
    gt::fmt_number(columns = c(PPG), decimals = 2) %>%
    gt::cols_align(align = "right", columns = c(GP, W, D, L, GD, Pts, PPG, ELO)) %>%
    gt::cols_align(align = "left",  columns = Team) %>%
    gt::tab_style(
      style     = gt::cell_text(color = "#0B3D91", weight = "bold"),
      locations = gt::cells_body(rows = Rank <= 2)
    ) %>%
    gt::tab_style(
      style     = gt::cell_text(color = "#E03C31", weight = "bold"),
      locations = gt::cells_body(rows = Rank >= (n_teams - 1))
    ) %>%
    gt::tab_spanner(label = "Record", columns = c(W, D, L)) %>%
    gt::tab_source_note(gt::md("*Projected* rows estimated via ELO win probability threshold (≥0.55 = win, 0.45–0.55 = draw)"))

  # ------------------------------------------------------------------
  # 3. League summary
  # ------------------------------------------------------------------
  outcomes_played <- ts %>%
    dplyr::select(match, points, points_status) %>%
    dplyr::distinct() %>%
    tidyr::drop_na(match) %>%
    dplyr::group_by(match) %>%
    dplyr::slice_max(order_by = points, n = 1) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(points_status) %>%
    dplyr::summarise(
      win_loss = sum(points == 3 | points == 0),
      draw     = sum(points == 1),
      .groups  = "drop"
    )

  tbl_league_df <- ep_played %>%
    dplyr::summarise(
      n_teams             = dplyr::n_distinct(team),
      total_points        = sum(total_points, na.rm = TRUE),
      total_games         = sum(games_played, na.rm = TRUE) / 2,
      pts_per_team        = round(total_points / n_teams, 1),
      pts_per_game        = round(total_points / total_games, 2)
    ) %>%
    dplyr::mutate(Status = "Played") %>%
    dplyr::bind_rows(
      ep_proj %>%
        dplyr::summarise(
          n_teams      = dplyr::n_distinct(team),
          total_points = sum(total_points, na.rm = TRUE),
          total_games  = sum(games_played, na.rm = TRUE) / 2,
          pts_per_team = round(total_points / n_teams, 1),
          pts_per_game = round(total_points / total_games, 2)
        ) %>%
        dplyr::mutate(Status = "Projected")
    ) %>%
    dplyr::select(Status, n_teams, total_games, total_points, pts_per_team, pts_per_game)

  tbl_league <- gt::gt(tbl_league_df) %>%
    style_gt_anythings(
      title    = "League Summary",
      subtitle = tournament
    ) %>%
    gt::cols_label(
      Status       = "",
      n_teams      = "Teams",
      total_games  = "Matches",
      total_points = "Total Pts",
      pts_per_team = "Pts / Team",
      pts_per_game = "Pts / Match"
    ) %>%
    gt::cols_align(align = "right", columns = c(n_teams, total_games, total_points, pts_per_team, pts_per_game)) %>%
    gt::cols_align(align = "left",  columns = Status)

  # ------------------------------------------------------------------
  # 4. Form table — last 5 results per team
  # ------------------------------------------------------------------
  form_raw <- ts %>%
    dplyr::filter(team_type %in% c("a", "b"), points_status == "Played") %>%
    dplyr::arrange(team, match) %>%
    dplyr::group_by(team) %>%
    dplyr::slice_tail(n = 5) %>%
    dplyr::mutate(game_seq = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      result_char = dplyr::case_when(
        game_outcome == "Win"  ~ "W",
        game_outcome == "Draw" ~ "D",
        game_outcome == "Loss" ~ "L",
        TRUE ~ "-"
      )
    ) %>%
    dplyr::select(team, game_seq, result_char)

  form_wide <- form_raw %>%
    tidyr::pivot_wider(
      names_from  = game_seq,
      values_from = result_char,
      names_prefix = "g"
    ) %>%
    dplyr::left_join(
      ep_played %>% dplyr::select(team, total_points, points_per_game),
      by = "team"
    ) %>%
    dplyr::arrange(-total_points) %>%
    dplyr::rename(Team = team, Pts = total_points, PPG = points_per_game)

  # Rename game columns dynamically
  game_cols <- intersect(paste0("g", 1:5), names(form_wide))
  if (length(game_cols) > 0) {
    names(form_wide)[names(form_wide) %in% game_cols] <- paste("-", length(game_cols):1)
  }

  tbl_form <- gt::gt(form_wide) %>%
    style_gt_anythings(
      title    = "Recent Form",
      subtitle = paste0(tournament, " — last 5 played matches (most recent rightmost)")
    ) %>%
    gt::cols_align(align = "center",
                   columns = dplyr::starts_with("- ")) %>%
    gt::cols_align(align = "left",  columns = Team) %>%
    gt::cols_align(align = "right", columns = c(Pts, PPG))

  # Colour W/D/L cells
  result_cols <- names(form_wide)[names(form_wide) %in% paste("-", 1:5)]
  for (col in result_cols) {
    tbl_form <- tbl_form %>%
      gt::tab_style(
        style     = list(gt::cell_fill("#2E7D32"), gt::cell_text(color = "white", weight = "bold")),
        locations = gt::cells_body(columns = !!rlang::sym(col), rows = !!rlang::sym(col) == "W")
      ) %>%
      gt::tab_style(
        style     = list(gt::cell_fill("#E03C31"), gt::cell_text(color = "white", weight = "bold")),
        locations = gt::cells_body(columns = !!rlang::sym(col), rows = !!rlang::sym(col) == "L")
      ) %>%
      gt::tab_style(
        style     = list(gt::cell_fill("#767676"), gt::cell_text(color = "white", weight = "bold")),
        locations = gt::cells_body(columns = !!rlang::sym(col), rows = !!rlang::sym(col) == "D")
      )
  }

  # ------------------------------------------------------------------
  # 5. Home vs Away breakdown
  # ------------------------------------------------------------------
  tbl_homeaway_df <- ep_played %>%
    dplyr::mutate(
      # Derive home/away games played from total_points_home/away
      # (accurate method: count rows in ts with team_type a/b)
      NULL
    ) %>%
    dplyr::select(team, total_points_home, total_points_away,
                  total_gd_home, total_gd_away) %>%
    dplyr::left_join(
      ts %>%
        dplyr::filter(team_type %in% c("a", "b"), points_status == "Played") %>%
        dplyr::group_by(team) %>%
        dplyr::summarise(
          gp_home = sum(team_type == "a"),
          gp_away = sum(team_type == "b"),
          .groups = "drop"
        ),
      by = "team"
    ) %>%
    dplyr::mutate(
      ppg_home = round(total_points_home / pmax(gp_home, 1), 2),
      ppg_away = round(total_points_away / pmax(gp_away, 1), 2)
    ) %>%
    dplyr::arrange(-total_points_home, -total_points_away) %>%
    dplyr::rename(
      Team    = team,
      `Home GP`  = gp_home,
      `Home Pts` = total_points_home,
      `Home GD`  = total_gd_home,
      `Home PPG` = ppg_home,
      `Away GP`  = gp_away,
      `Away Pts` = total_points_away,
      `Away GD`  = total_gd_away,
      `Away PPG` = ppg_away
    )

  tbl_homeaway <- gt::gt(tbl_homeaway_df) %>%
    style_gt_anythings(
      title    = "Home vs Away Breakdown",
      subtitle = tournament
    ) %>%
    gt::tab_spanner(label = "Home", columns = dplyr::starts_with("Home")) %>%
    gt::tab_spanner(label = "Away", columns = dplyr::starts_with("Away")) %>%
    gt::cols_align(align = "right", columns = -Team) %>%
    gt::cols_align(align = "left",  columns = Team) %>%
    gt::tab_style(
      style     = gt::cell_text(color = "#0B3D91", weight = "bold"),
      locations = gt::cells_column_spanners(spanners = "Home")
    ) %>%
    gt::tab_style(
      style     = gt::cell_text(color = "#767676", weight = "bold"),
      locations = gt::cells_column_spanners(spanners = "Away")
    )

  # Return all tables as a named list
  list(
    tbl_current   = tbl_current,
    tbl_projected = tbl_projected,
    tbl_league    = tbl_league,
    tbl_form      = tbl_form,
    tbl_homeaway  = tbl_homeaway
  )
}
