# R/make_figures_plotly.R
# Interactive plotly figures for the Shiny dashboard.
# make_figures() (ggplot2) is kept unchanged and used for PDF downloads.
#
# Strategy:
#   Line / scatter charts  → strip geom_label_repel then ggplotly()
#   Heatmaps               → native plot_ly() — week_game is a factor, so we
#                            use categoryarray for axis order and per-cell
#                            annotations for the outcome letter (W/D/L).
#   Bar charts             → ggplotly() directly (they convert cleanly)

library(plotly)
library(dplyr)
library(tidyr)

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

# Remove geom_label_repel / geom_text_repel layers (ggplotly doesn't support them)
.strip_repel <- function(p) {
  p$layers <- Filter(
    function(l) !inherits(l$geom, c("GeomLabelRepel", "GeomTextRepel")),
    p$layers
  )
  p
}

# Remove plain geom_text layers (to replace with team-coloured versions)
.strip_geom_text <- function(p) {
  p$layers <- Filter(
    function(l) !inherits(l$geom, "GeomText"),
    p$layers
  )
  p
}

# Add any ggplot2 layer within a pipe chain
.add_layer <- function(p, layer) p + layer

# ggplotly creates one "markers" mode trace per (team, game_outcome) combination,
# resulting in a legend entry for every team × outcome pair.
# This collapses that into 3 clean Win / Draw / Loss shape entries.
.fix_outcome_legend <- function(p) {
  # Hide all markers-only traces from the legend (these are the per-team outcome
  # groups ggplotly generates for the shape aesthetic).
  p$x$data <- lapply(p$x$data, function(tr) {
    if (identical(tr$mode, "markers")) tr$showlegend <- FALSE
    tr
  })
  # Add three neutral shape-only legend entries that match ggplot2 shapes
  #   Win  → shape 17 → plotly "triangle-up"
  #   Draw → shape 16 → plotly "circle"
  #   Loss → shape  4 → plotly "x" (line marker)
  p |>
    plotly::add_trace(
      inherit = FALSE, type = "scatter", mode = "markers",
      x = numeric(0), y = numeric(0),
      name       = "Win",
      marker     = list(symbol = "triangle-up", size = 9, color = "#333333"),
      showlegend = TRUE, hoverinfo = "none"
    ) |>
    plotly::add_trace(
      inherit = FALSE, type = "scatter", mode = "markers",
      x = numeric(0), y = numeric(0),
      name       = "Draw",
      marker     = list(symbol = "circle", size = 9, color = "#333333"),
      showlegend = TRUE, hoverinfo = "none"
    ) |>
    plotly::add_trace(
      inherit = FALSE, type = "scatter", mode = "markers",
      x = numeric(0), y = numeric(0),
      name       = "Loss",
      marker     = list(
        symbol = "x", size = 9, color = "#333333",
        line   = list(width = 2, color = "#333333")
      ),
      showlegend = TRUE, hoverinfo = "none"
    )
}

# Shared plotly layout that matches the ANYthings brand
.brand <- function(p) {
  p |>
    plotly::layout(
      font          = list(family = "Inter, Arial, sans-serif", size = 11, color = "#333333"),
      paper_bgcolor = "#FFFFFF",
      plot_bgcolor  = "#FFFFFF",
      legend        = list(orientation = "h", y = -0.18, font = list(size = 10)),
      margin        = list(t = 60, r = 20, b = 80, l = 60)
    ) |>
    plotly::config(
      displayModeBar         = TRUE,
      modeBarButtonsToRemove = c("lasso2d", "select2d"),
      displaylogo            = FALSE
    )
}

# Build a named matrix (rows = row_levels, cols = col_levels) from long data.
# Numeric values — NA where no match exists.
.wide_num <- function(df, row_col, col_col, val_col, row_levels, col_levels) {
  m <- matrix(
    NA_real_,
    nrow = length(row_levels), ncol = length(col_levels),
    dimnames = list(row_levels, col_levels)
  )
  for (i in seq_len(nrow(df))) {
    r <- as.character(df[[row_col]][i])
    c <- as.character(df[[col_col]][i])
    if (!is.na(r) && !is.na(c) && r %in% row_levels && c %in% col_levels) {
      m[r, c] <- as.numeric(df[[val_col]][i])
    }
  }
  m
}

# Same but for character/hover values — "" where no match.
.wide_chr <- function(df, row_col, col_col, val_col, row_levels, col_levels) {
  m <- matrix(
    "",
    nrow = length(row_levels), ncol = length(col_levels),
    dimnames = list(row_levels, col_levels)
  )
  for (i in seq_len(nrow(df))) {
    r <- as.character(df[[row_col]][i])
    c <- as.character(df[[col_col]][i])
    v <- df[[val_col]][i]
    if (!is.na(r) && !is.na(c) && r %in% row_levels && c %in% col_levels && !is.na(v)) {
      m[r, c] <- as.character(v)
    }
  }
  m
}

# Build a plotly annotation list (one entry per non-empty cell).
# x_col / y_col are the column names whose values match the categorical axis.
.hm_annotations <- function(df, x_col, y_col, text_col,
                              bold = TRUE, font_size = 12, color = "#1A1A1A") {
  df <- df[!is.na(df[[text_col]]) & nchar(trimws(df[[text_col]])) > 0, ]
  lapply(seq_len(nrow(df)), function(i) {
    txt <- as.character(df[[text_col]][i])
    list(
      x         = as.character(df[[x_col]][i]),
      y         = as.character(df[[y_col]][i]),
      text      = if (bold) paste0("<b>", txt, "</b>") else txt,
      showarrow = FALSE,
      font      = list(size = font_size, color = color),
      xanchor   = "center",
      yanchor   = "middle"
    )
  })
}

.pred_colorbar <- function() {
  list(
    title    = list(text = "Prediction"),
    tickvals = c(0, 0.5, 1),
    ticktext = c("Unpredicted", "Neutral", "Predicted"),
    len      = 0.55
  )
}

# ---------------------------------------------------------------------------
# Main function
# ---------------------------------------------------------------------------

#' Build interactive plotly figures for the Shiny app.
#'
#' @param data       Named list returned by run_rankings().
#' @param gg_figures Named list returned by make_figures() — used for ggplotly
#'                   conversion of the non-heatmap charts.
#' @return Named list of plotly objects (same names as make_figures()).
make_figures_plotly <- function(data, gg_figures) {

  ts        <- data$team_stats
  ep_played <- data$end_points_played

  n_teams <- dplyr::n_distinct(ep_played$team)

  # Gradient colour scales (matching ggplot2 make_figures)
  pred_cs <- list(
    list(0,   "#E03C31"),   # ANYthings Signal Red
    list(0.5, "#F5F5F5"),   # neutral
    list(1,   "#2E7D32")    # dark green
  )
  ppg_cs <- list(
    list(0, "#F5F5F5"),
    list(1, "#0B3D91")      # ANYthings System Blue
  )

  ep_proj <- data$end_points_projected

  # ---- 1–3. Line charts -------------------------------------------------------
  # Strip repel → add plain geom_text at the line endpoint → ggplotly renders
  # it as permanent SVG text (always visible, same team colour as the line).
  # nudge_x = 2 shifts the label 2 days right; hjust = 0 left-aligns it from
  # that point. The 18 % x-axis right-expansion in x_scale gives enough room.

  fig_elo <- gg_figures$fig_elo |>
    .strip_repel() |>
    .add_layer(ggplot2::geom_text(
      data        = ep_played,
      ggplot2::aes(label = team, fontface = "bold"),
      hjust       = 0,
      nudge_x     = 2,
      size        = 2.8,
      show.legend = FALSE
    )) |>
    plotly::ggplotly(tooltip = "all") |>
    .brand() |>
    .fix_outcome_legend()

  fig_points <- gg_figures$fig_points |>
    .strip_repel() |>
    .add_layer(ggplot2::geom_text(
      data        = ep_proj,
      ggplot2::aes(label = team, fontface = "bold"),
      hjust       = 0,
      nudge_x     = 2,
      size        = 2.8,
      show.legend = FALSE
    )) |>
    plotly::ggplotly(tooltip = "all") |>
    .brand() |>
    .fix_outcome_legend()

  fig_ppg <- gg_figures$fig_ppg |>
    .strip_repel() |>
    .add_layer(ggplot2::geom_text(
      data        = ep_proj,
      ggplot2::aes(label = team, fontface = "bold"),
      hjust       = 0,
      nudge_x     = 2,
      size        = 2.8,
      show.legend = FALSE
    )) |>
    plotly::ggplotly(tooltip = "all") |>
    .brand() |>
    .fix_outcome_legend()

  # ---- 4. ELO vs Points scatter -----------------------------------------------
  # colour = team is only in geom_point's local aes, so specify it explicitly.
  fig_comp <- gg_figures$fig_comp |>
    .strip_repel() |>
    .add_layer(ggplot2::geom_text(
      ggplot2::aes(label = team, colour = team),
      # vjust       = -0.9,
      # hjust       = 1,
      size        = 3, 
      nudge_y     = -0.1,
      show.legend = FALSE
    )) |>
    plotly::ggplotly(tooltip = "all") |>
    .brand()

  # ---- Heatmap shared setup ---------------------------------------------------
  # week_game is a factor; levels define the correct display order
  hm_base <- ts |>
    dplyr::filter(!is.na(opponent)) |>
    dplyr::mutate(
      team          = factor(team,     levels = ep_played$team),
      week_game_chr = as.character(week_game)
    )

  team_order  <- ep_played$team
  round_order <- levels(hm_base$week_game)  # factor levels = correct axis order

  # Dynamic annotation font size: shrink as teams grow
  heat_px <- max(9L, min(16L, round(105 / n_teams)))

  # ---- 5. Heatmap: round outcomes ---------------------------------------------
  hm_r <- hm_base |>
    dplyr::mutate(hover = paste0(
      "<b>", team, "</b> vs ", opponent, "<br>",
      week_game_chr, "<br>",
      "Result: <b>", game_outcome, "</b><br>",
      "ELO win prob: ", sprintf("%.2f", team_prediction_elo)
    ))

  z_r     <- .wide_num(hm_r, "week_game_chr", "team", "prediction_gradient", round_order, team_order)
  hover_r <- .wide_chr(hm_r, "week_game_chr", "team", "hover",               round_order, team_order)
  ann_r   <- .hm_annotations(
    hm_r |> dplyr::distinct(team, week_game_chr, game_outcome),
    x_col = "team", y_col = "week_game_chr", text_col = "game_outcome",
    bold = TRUE, font_size = heat_px
  )

  fig_heatmap_round <- plotly::plot_ly(
    type          = "heatmap",
    z             = z_r,
    x             = team_order,
    y             = round_order,
    colorscale    = pred_cs,
    zmin = 0, zmax = 1,
    text          = hover_r,
    hovertemplate = "%{text}<extra></extra>",
    colorbar      = .pred_colorbar(),
    xgap = 3, ygap = 3
  ) |>
    plotly::layout(
      title = list(
        text    = "<b>Match Outcomes by Round</b>",
        font    = list(size = 14, color = "#1A1A1A"),
        x       = 0.02, xanchor = "left"
      ),
      xaxis = list(
        title         = "",
        tickangle     = -45,
        type          = "category",
        categoryorder = "array",
        categoryarray = team_order
      ),
      yaxis = list(
        title         = "Round",
        type          = "category",
        categoryorder = "array",
        categoryarray = round_order
      ),
      annotations   = ann_r,
      font          = list(family = "Inter, Arial, sans-serif"),
      paper_bgcolor = "#FFFFFF",
      plot_bgcolor  = "#FFFFFF",
      margin        = list(t = 60, b = 100, l = 60, r = 20)
    ) |>
    plotly::config(displayModeBar = FALSE)

  # ---- 6. Heatmap: head-to-head matchup ---------------------------------------
  hm_m <- ts |>
    dplyr::filter(!is.na(opponent)) |>
    dplyr::mutate(
      team     = factor(team,     levels = ep_played$team),
      opponent = factor(opponent, levels = ep_played$team),
      team_chr = as.character(team),
      opp_chr  = as.character(opponent),
      hover    = paste0(
        "<b>", team, "</b> vs ", opponent, "<br>",
        "Result: <b>", game_outcome, "</b><br>",
        "ELO win prob: ", sprintf("%.2f", team_prediction_elo)
      )
    )

  opp_order   <- ep_played$team

  # z matrix: rows = team (y-axis), cols = opponent (x-axis)
  z_m     <- .wide_num(hm_m, "team_chr", "opp_chr", "prediction_gradient", team_order, opp_order)
  hover_m <- .wide_chr(hm_m, "team_chr", "opp_chr", "hover",               team_order, opp_order)
  ann_m   <- .hm_annotations(
    hm_m |> dplyr::distinct(team_chr, opp_chr, game_outcome),
    x_col = "opp_chr", y_col = "team_chr", text_col = "game_outcome",
    bold = TRUE, font_size = heat_px
  )

  fig_heatmap_matchup <- plotly::plot_ly(
    type          = "heatmap",
    z             = z_m,
    x             = opp_order,
    y             = team_order,
    colorscale    = pred_cs,
    zmin = 0, zmax = 1,
    text          = hover_m,
    hovertemplate = "%{text}<extra></extra>",
    colorbar      = .pred_colorbar(),
    xgap = 3, ygap = 3
  ) |>
    plotly::layout(
      title = list(
        text    = "<b>Head-to-Head Results vs ELO Prediction</b><br><sup>Green = matched · Red = upset</sup>",
        font    = list(size = 14, color = "#1A1A1A"),
        x       = 0.02, xanchor = "left"
      ),
      xaxis = list(
        title         = "Opponent",
        tickangle     = -45,
        type          = "category",
        categoryorder = "array",
        categoryarray = opp_order
      ),
      yaxis = list(
        title         = "Team",
        type          = "category",
        categoryorder = "array",
        categoryarray = team_order
      ),
      annotations   = ann_m,
      font          = list(family = "Inter, Arial, sans-serif"),
      paper_bgcolor = "#FFFFFF",
      plot_bgcolor  = "#FFFFFF",
      margin        = list(t = 80, b = 100, l = 60, r = 20)
    ) |>
    plotly::config(displayModeBar = FALSE)

  # ---- 7. Heatmap: PPG by round -----------------------------------------------
  hm_p <- hm_base |>
    dplyr::mutate(hover = paste0(
      "<b>", team, "</b> vs ", opponent, "<br>",
      week_game_chr, "<br>",
      "Result: <b>", game_outcome, "</b><br>",
      "PPG: ", round(points_per_game, 2)
    ))

  # Use points_fill (capped 0–3) if available, otherwise points_per_game
  ppg_val_col <- if ("points_fill" %in% names(hm_p)) "points_fill" else "points_per_game"
  z_p     <- .wide_num(hm_p, "week_game_chr", "team", ppg_val_col,  round_order, team_order)
  hover_p <- .wide_chr(hm_p, "week_game_chr", "team", "hover",      round_order, team_order)
  ann_p   <- .hm_annotations(
    hm_p |> dplyr::distinct(team, week_game_chr, game_outcome),
    x_col = "team", y_col = "week_game_chr", text_col = "game_outcome",
    bold = TRUE, font_size = heat_px
  )

  fig_heatmap_ppg <- plotly::plot_ly(
    type          = "heatmap",
    z             = z_p,
    x             = team_order,
    y             = round_order,
    colorscale    = ppg_cs,
    zmin = 0, zmax = 3,
    text          = hover_p,
    hovertemplate = "%{text}<extra></extra>",
    colorbar      = list(title = list(text = "PPG"),
                         tickvals = c(0, 1, 2, 3),
                         ticktext = c("0", "1", "2", "3"), len = 0.55),
    xgap = 3, ygap = 3
  ) |>
    plotly::layout(
      title = list(
        text    = "<b>Points Per Game by Round</b>",
        font    = list(size = 14, color = "#1A1A1A"),
        x       = 0.02, xanchor = "left"
      ),
      xaxis = list(
        title         = "",
        tickangle     = -45,
        type          = "category",
        categoryorder = "array",
        categoryarray = team_order
      ),
      yaxis = list(
        title         = "Round",
        type          = "category",
        categoryorder = "array",
        categoryarray = round_order
      ),
      annotations   = ann_p,
      font          = list(family = "Inter, Arial, sans-serif"),
      paper_bgcolor = "#FFFFFF",
      plot_bgcolor  = "#FFFFFF",
      margin        = list(t = 60, b = 100, l = 60, r = 20)
    ) |>
    plotly::config(displayModeBar = FALSE)

  # ---- 8. Attack vs Defence ---------------------------------------------------
  # Strip the hardcoded-dark geom_text, then add it back inheriting colour = team
  # from the plot-level aes so labels match their point colours.
  fig_attack_defense <- gg_figures$fig_attack_defense |>
    .strip_geom_text() |>
    .strip_repel() |>
    .add_layer(ggplot2::geom_text(
      ggplot2::aes(label = team),
      #vjust       = -0.9,
      #hjust       = 1,
      nudge_y     = -0.1,
      size        = 3,
      fontface = "bold",
      show.legend = FALSE
    )) |>
    plotly::ggplotly(tooltip = "all") |>
    .brand()

  # ---- 9–10. Bar charts -------------------------------------------------------
  fig_home_away <- plotly::ggplotly(gg_figures$fig_home_away, tooltip = "all") |>
    .brand()

  fig_elo_momentum <- plotly::ggplotly(gg_figures$fig_elo_momentum, tooltip = "all") |>
    .brand()

  # Return named list matching make_figures() output
  list(
    fig_elo             = fig_elo,
    fig_points          = fig_points,
    fig_ppg             = fig_ppg,
    fig_comp            = fig_comp,
    fig_heatmap_round   = fig_heatmap_round,
    fig_heatmap_matchup = fig_heatmap_matchup,
    fig_heatmap_ppg     = fig_heatmap_ppg,
    fig_attack_defense  = fig_attack_defense,
    fig_home_away       = fig_home_away,
    fig_elo_momentum    = fig_elo_momentum
  )
}
