# R/normalize_data.R
# Detects the source format of uploaded match data and normalizes it to the
# standard internal schema expected by run_rankings().
#
# Standard schema columns:
#   Dato       <Date>   match date
#   Runde      <int>    round number
#   Hjemmelag  <chr>    home team name
#   Bortelag   <chr>    away team name
#   Resultat   <chr>    score string: "2 - 1"  (always home - away)
#   Turnering  <chr>    tournament / league name

library(dplyr)
library(stringr)
library(readxl)
library(lubridate)

# ---------------------------------------------------------------------------
# Source detection
# ---------------------------------------------------------------------------

#' Detect the data source format from a raw data frame's column names.
#'
#' @param df Raw data frame as read from the uploaded file.
#' @return Character scalar: "fotball_no", "profixio", "gothia",
#'         "norway_cup", or "unknown".
detect_source <- function(df) {
  cols <- tolower(names(df))

  # fotball.no: Norwegian columns including "hjemmelag" and "resultat"
  if (all(c("hjemmelag", "bortelag", "resultat") %in% cols)) {
    return("fotball_no")
  }

  # Profixio: often uses "hjemme" / "borte" without "lag", or "hjemmelag"
  # without "resultat" (separate score columns)
  if ("hjemmelag" %in% cols && any(c("hjemmemål", "hjemmemaal", "hjem_score") %in% cols)) {
    return("profixio")
  }
  if (all(c("home_team", "away_team", "home_score", "away_score") %in% cols)) {
    return("profixio")
  }

  # Gothia Cup: English/Swedish columns
  if (all(c("hometeam", "awayteam") %in% cols) ||
      all(c("home", "away", "score") %in% cols) ||
      all(c("hemmalag", "bortalag") %in% cols)) {
    return("gothia")
  }

  # Norway Cup: may use fotball.no backend (same schema) or English labels
  if (all(c("home", "away", "result") %in% cols)) {
    return("norway_cup")
  }

  "unknown"
}

# ---------------------------------------------------------------------------
# Per-source normalizers
# ---------------------------------------------------------------------------

#' Normalize a fotball.no data frame to the standard schema.
#' This is essentially a no-op since fotball.no already uses the right columns.
normalize_fotball_no <- function(df) {
  df %>%
    mutate(
      Dato      = as.Date(Dato),
      Runde     = suppressWarnings(as.integer(Runde)),
      Turnering = as.character(Turnering),
      Resultat  = str_trim(as.character(Resultat))
    ) %>%
    select(Dato, Runde, Hjemmelag, Bortelag, Resultat, Turnering)
}

#' Normalize a Profixio data frame to the standard schema.
#' Profixio can export with separate home/away goal columns or a combined score.
normalize_profixio <- function(df) {
  cols <- tolower(names(df))

  # Rename to standard Norwegian if using English labels
  if ("home_team" %in% cols) df <- rename(df, Hjemmelag = home_team, Bortelag = away_team)
  if ("hjemmelag" %in% cols && !"Hjemmelag" %in% names(df)) df <- rename(df, Hjemmelag = hjemmelag, Bortelag = bortelag)

  # Build Resultat from separate score columns if needed
  if (!"Resultat" %in% names(df)) {
    home_col <- intersect(c("home_score","hjemmemål","hjemmemaal","hjem_score"), cols)[1]
    away_col <- intersect(c("away_score","bortemål","bortemaal","borte_score"), cols)[1]
    if (!is.na(home_col) && !is.na(away_col)) {
      df <- df %>%
        mutate(Resultat = paste0(
          .data[[which(cols == home_col)]], " - ",
          .data[[which(cols == away_col)]]
        ))
    } else {
      stop("Profixio file: cannot find score columns. Please use manual mapping.")
    }
  } else {
    # Normalize delimiter: "2-1" or "2:1" → "2 - 1"
    df <- df %>%
      mutate(Resultat = str_replace(Resultat, "^(\\d+)\\s*[-:]\\s*(\\d+)$", "\\1 - \\2"))
  }

  # Date column: try common names
  date_col <- intersect(c("dato","date","kampdag","spilledato"), cols)[1]
  if (!is.na(date_col) && date_col != "dato") {
    df <- df %>% rename(Dato = all_of(which(cols == date_col)))
  }

  # Round column
  round_col <- intersect(c("runde","round","rnd","kampnr"), cols)[1]
  if (!is.na(round_col) && round_col != "runde") {
    df <- df %>% rename(Runde = all_of(which(cols == round_col)))
  } else if (!"Runde" %in% names(df)) {
    df <- df %>% mutate(Runde = row_number())
  }

  # Tournament name
  tourn_col <- intersect(c("turnering","tournament","league","serie"), cols)[1]
  if (!is.na(tourn_col) && tourn_col != "turnering") {
    df <- df %>% rename(Turnering = all_of(which(cols == tourn_col)))
  } else if (!"Turnering" %in% names(df)) {
    df <- df %>% mutate(Turnering = "Unknown Tournament")
  }

  df %>%
    mutate(
      Dato  = as.Date(Dato),
      Runde = suppressWarnings(as.integer(Runde))
    ) %>%
    select(Dato, Runde, Hjemmelag, Bortelag, Resultat, Turnering)
}

#' Normalize a Gothia Cup data frame to the standard schema.
normalize_gothia <- function(df) {
  cols <- tolower(names(df))

  # Map home/away team columns
  home_col <- intersect(c("hometeam","home_team","home","hemmalag"), cols)[1]
  away_col <- intersect(c("awayteam","away_team","away","bortalag"), cols)[1]
  df <- df %>%
    rename(Hjemmelag = all_of(which(cols == home_col)),
           Bortelag  = all_of(which(cols == away_col)))

  # Score: may be "Score" = "2-1" or separate columns
  if ("score" %in% cols) {
    df <- df %>%
      mutate(Resultat = str_replace(score, "^(\\d+)\\s*[-:]\\s*(\\d+)$", "\\1 - \\2"))
  } else {
    hs <- intersect(c("homegoals","home_goals","home_score","hemmamål"), cols)[1]
    as_ <- intersect(c("awaygoals","away_goals","away_score","bortamål"), cols)[1]
    df <- df %>%
      mutate(Resultat = paste0(.data[[which(cols == hs)]], " - ", .data[[which(cols == as_)]]))
  }

  # Date
  date_col <- intersect(c("date","dato","matchdate","spelldatum"), cols)[1]
  df <- df %>% rename(Dato = all_of(which(cols == date_col)))

  # Round
  round_col <- intersect(c("round","runde","matchnumber","omgång"), cols)[1]
  if (!is.na(round_col)) {
    df <- df %>% rename(Runde = all_of(which(cols == round_col)))
  } else {
    df <- df %>% mutate(Runde = row_number())
  }

  # Tournament
  tourn_col <- intersect(c("tournament","group","turnering","division"), cols)[1]
  if (!is.na(tourn_col)) {
    df <- df %>% rename(Turnering = all_of(which(cols == tourn_col)))
  } else {
    df <- df %>% mutate(Turnering = "Gothia Cup")
  }

  df %>%
    mutate(
      Dato  = as.Date(Dato),
      Runde = suppressWarnings(as.integer(Runde))
    ) %>%
    select(Dato, Runde, Hjemmelag, Bortelag, Resultat, Turnering)
}

#' Normalize a Norway Cup data frame (English column names) to the standard schema.
normalize_norway_cup <- function(df) {
  cols <- tolower(names(df))

  home_col <- intersect(c("home","home_team","hjemmelag"), cols)[1]
  away_col <- intersect(c("away","away_team","bortelag"), cols)[1]
  df <- df %>%
    rename(Hjemmelag = all_of(which(cols == home_col)),
           Bortelag  = all_of(which(cols == away_col)))

  if ("result" %in% cols) {
    df <- df %>%
      mutate(Resultat = str_replace(result, "^(\\d+)\\s*[-:]\\s*(\\d+)$", "\\1 - \\2"))
  } else {
    hs <- intersect(c("home_score","home_goals","hgoals"), cols)[1]
    as_ <- intersect(c("away_score","away_goals","agoals"), cols)[1]
    df <- df %>%
      mutate(Resultat = paste0(.data[[which(cols == hs)]], " - ", .data[[which(cols == as_)]]))
  }

  date_col <- intersect(c("date","dato","match_date"), cols)[1]
  df <- df %>% rename(Dato = all_of(which(cols == date_col)))

  round_col <- intersect(c("round","runde","match_number"), cols)[1]
  if (!is.na(round_col)) {
    df <- df %>% rename(Runde = all_of(which(cols == round_col)))
  } else {
    df <- df %>% mutate(Runde = row_number())
  }

  tourn_col <- intersect(c("tournament","turnering","league","group"), cols)[1]
  if (!is.na(tourn_col)) {
    df <- df %>% rename(Turnering = all_of(which(cols == tourn_col)))
  } else {
    df <- df %>% mutate(Turnering = "Norway Cup")
  }

  df %>%
    mutate(
      Dato  = as.Date(Dato),
      Runde = suppressWarnings(as.integer(Runde))
    ) %>%
    select(Dato, Runde, Hjemmelag, Bortelag, Resultat, Turnering)
}

# ---------------------------------------------------------------------------
# Manual mapping helper (for "unknown" sources)
# ---------------------------------------------------------------------------

#' Apply a user-supplied column mapping to produce the standard schema.
#'
#' @param df Raw data frame.
#' @param mapping Named list: standard column name → actual column name in df.
#'   Required keys: date_col, round_col, home_col, away_col, score_col (or
#'   home_score_col + away_score_col), tournament_col (or tournament_name).
#' @return Normalized data frame in the standard schema.
normalize_manual <- function(df, mapping) {
  out <- df

  out <- out %>%
    rename(
      Dato      = all_of(mapping$date_col),
      Hjemmelag = all_of(mapping$home_col),
      Bortelag  = all_of(mapping$away_col)
    )

  # Score
  if (!is.null(mapping$score_col)) {
    out <- out %>%
      mutate(Resultat = str_replace(
        as.character(.data[[mapping$score_col]]),
        "^(\\d+)\\s*[-:]\\s*(\\d+)$", "\\1 - \\2"
      ))
  } else {
    out <- out %>%
      mutate(Resultat = paste0(
        .data[[mapping$home_score_col]], " - ",
        .data[[mapping$away_score_col]]
      ))
  }

  # Round
  if (!is.null(mapping$round_col)) {
    out <- out %>% rename(Runde = all_of(mapping$round_col))
  } else {
    out <- out %>% mutate(Runde = row_number())
  }

  # Tournament
  if (!is.null(mapping$tournament_col)) {
    out <- out %>% rename(Turnering = all_of(mapping$tournament_col))
  } else {
    out <- out %>% mutate(Turnering = mapping$tournament_name %||% "Unknown Tournament")
  }

  out %>%
    mutate(
      Dato  = as.Date(Dato),
      Runde = suppressWarnings(as.integer(Runde))
    ) %>%
    select(Dato, Runde, Hjemmelag, Bortelag, Resultat, Turnering)
}

# ---------------------------------------------------------------------------
# Main dispatcher
# ---------------------------------------------------------------------------

#' Normalize a raw match data frame to the standard pipeline schema.
#'
#' @param df Raw data frame (from readxl or read.csv).
#' @param source Optional character override. If NULL, auto-detected.
#' @param manual_mapping List for "unknown" sources. See normalize_manual().
#' @return Normalized data frame ready for run_rankings().
normalize_matches <- function(df, source = NULL, manual_mapping = NULL) {
  if (is.null(source)) source <- detect_source(df)

  result <- switch(source,
    fotball_no  = normalize_fotball_no(df),
    profixio    = normalize_profixio(df),
    gothia      = normalize_gothia(df),
    norway_cup  = normalize_norway_cup(df),
    unknown     = {
      if (is.null(manual_mapping)) {
        stop("Unknown data source format. Please provide a manual_mapping list.")
      }
      normalize_manual(df, manual_mapping)
    },
    stop("Unrecognized source: ", source)
  )

  # Final validation
  required <- c("Dato", "Runde", "Hjemmelag", "Bortelag", "Resultat", "Turnering")
  missing <- setdiff(required, names(result))
  if (length(missing) > 0) {
    stop("Normalization failed. Missing columns: ", paste(missing, collapse = ", "))
  }

  # Only keep rows where result has been entered (not scheduled-only rows)
  result <- result %>%
    filter(!is.na(Dato))

  result
}

#' Read one or more uploaded files and return a single normalized data frame.
#'
#' @param file_paths Character vector of file paths (from Shiny fileInput$datapath).
#' @param source Optional override passed to normalize_matches().
#' @param manual_mapping Optional mapping for unknown formats.
#' @return Single combined and normalized data frame.
read_and_normalize <- function(file_paths, source = NULL, manual_mapping = NULL) {
  dfs <- lapply(file_paths, function(path) {
    ext <- tolower(tools::file_ext(path))
    raw <- if (ext %in% c("xlsx", "xls")) {
      readxl::read_excel(path)
    } else {
      read.csv(path, stringsAsFactors = FALSE)
    }
    normalize_matches(raw, source = source, manual_mapping = manual_mapping)
  })
  dplyr::bind_rows(dfs)
}
