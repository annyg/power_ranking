#' @description
#' Read a single NFF player "Statistikk" page and return tidy tibbles
#' (`seasons`, `tournaments`, `teams`) plus a one-row `log` tibble containing
#' URL, status, player_name, and any error message. Set `verbose = TRUE` to
#' print progress messages.
#'
#' @param url Character scalar. NFF player page URL.
#' @param verbose Logical. Print progress/details. Default FALSE.
#' @return A named list: `seasons`, `tournaments`, `teams`, `log`.
#'
#' @ImportFrom rvest read_html html_elements html_text2 html_table
#' @ImportFrom dplyr mutate rename bind_rows across any_of tibble
#' @ImportFrom stringr str_squish str_replace_all
#' @ImportFrom purrr map compact
#' @ImportFrom tibble tibble as_tibble
#' @ImportFrom janitor clean_names
nff_read_player <- function(url, verbose = FALSE) {
  stopifnot(length(url) == 1, is.character(url), !is.na(url))
  if (verbose) message("Fetching: ", url)

  # --- Fetch safely ----------------------------------------------------------
  err_msg <- NULL
  pg <- try(rvest::read_html(url), silent = TRUE)
  if (inherits(pg, "try-error")) {
    err_msg <- as.character(attr(pg, "condition")$message)
    if (verbose) message("  ✖ Failed: ", err_msg)
    return(list(
      seasons     = tibble::tibble(),
      tournaments = tibble::tibble(),
      teams       = tibble::tibble(),
      log         = tibble::tibble(
        source_url  = url,
        status      = "failed",
        player_name = NA_character_,
        n_tables    = 0L,
        note        = NA_character_,
        error       = err_msg
      )
    ))
  }

  # --- Player name -----------------------------------------------------------
  player_name <- pg |>
    rvest::html_elements("h1, .player-header__name, .person-header__title") |>
    rvest::html_text2() |>
    stringr::str_squish()
  player_name <- if (length(player_name)) player_name[1] else NA_character_
  if (verbose) message("  ✓ Player: ", player_name %||% "<unknown>")

  # --- Tables ----------------------------------------------------------------
  raw_tbls <- pg |>
    rvest::html_elements("table") |>
    rvest::html_table(fill = TRUE)

  .clean_tbl <- function(x) {
    if (!is.data.frame(x)) return(tibble::tibble())
    out <- janitor::clean_names(x)
    out <- out[, colSums(!is.na(out) & out != "") > 0, drop = FALSE]
    if (nrow(out)) out <- out[rowSums(!is.na(out) & out != "") > 0, , drop = FALSE]
    tibble::as_tibble(out)
  }

  tbls <- lapply(raw_tbls, .clean_tbl)
  tbls <- tbls[vapply(tbls, nrow, integer(1)) > 0]
  if (verbose) message("  ✓ Tables found: ", length(tbls))

  is_season_tbl <- function(x) any(names(x) %in% "sesong")
  is_tourn_tbl  <- function(x) any(names(x) %in% "turneringskategori")
  is_team_tbl   <- function(x) any(names(x) %in% "lag") && !is_tourn_tbl(x)

  seasons_lst     <- purrr::map(tbls, ~ if (is_season_tbl(.x)) .x else NULL) |> purrr::compact()
  tournaments_lst <- purrr::map(tbls, ~ if (is_tourn_tbl(.x)) .x else NULL)  |> purrr::compact()
  teams_lst       <- purrr::map(tbls, ~ if (is_team_tbl(.x)) .x else NULL)   |> purrr::compact()

  bind_or_empty <- function(lst) if (length(lst)) dplyr::bind_rows(lst) else tibble::tibble()

  seasons     <- bind_or_empty(seasons_lst)
  tournaments <- bind_or_empty(tournaments_lst)
  teams       <- bind_or_empty(teams_lst)

  # ----- Typing (force stable types) -----------------------------------------
  norm_num <- function(x) {
    x <- ifelse(x %in% c("", "–", "-", NA), NA, x)
    x <- stringr::str_replace_all(x, ",", ".")
    suppressWarnings(as.numeric(x))
  }

  if ("sesong" %in% names(seasons)) seasons$sesong <- as.character(seasons$sesong)
  if ("turneringskategori" %in% names(tournaments)) tournaments$turneringskategori <- as.character(tournaments$turneringskategori)
  if ("lag" %in% names(teams)) teams$lag <- as.character(teams$lag)

  seasons <- seasons |>
    dplyr::mutate(
      dplyr::across(dplyr::any_of(c("kamper","mal","gule_kort","rodt_kort")), ~ suppressWarnings(as.integer(.x))),
      dplyr::across(dplyr::any_of(c("mal_snitt","mal_per_kamp","mal_snitt_")), norm_num)
    )
  tournaments <- tournaments |>
    dplyr::mutate(
      dplyr::across(dplyr::any_of(c("kamper","mal","gule_kort","rodt_kort")), ~ suppressWarnings(as.integer(.x)))
    )
  teams <- teams |>
    dplyr::mutate(
      dplyr::across(dplyr::any_of(c("kamper","mal","gule_kort","rodt_kort")), ~ suppressWarnings(as.integer(.x)))
    )

  add_meta <- function(df) {
    if (!nrow(df)) return(df)
    dplyr::mutate(df, player_name = player_name, source_url = url, .after = 1)
  }

  if (verbose) {
    message(sprintf("  ✓ Parsed: seasons=%d, tournaments=%d, teams=%d",
                    nrow(seasons), nrow(tournaments), nrow(teams)))
  }

  list(
    seasons     = add_meta(seasons),
    tournaments = add_meta(tournaments),
    teams       = add_meta(teams),
    log         = tibble::tibble(
      source_url  = url,
      status      = "ok",
      player_name = player_name,
      n_tables    = length(tbls),
      note        = NA_character_,
      error       = NA_character_
    )
  )
}


#' gs_read_urls
#'
#' @description
#' Read a column of profile URLs and (optionally) metadata from Google Sheets.
#' Supports either a single sheet for both URLs and metadata, or separate sheets.
#' Returns a list with `urls` (character) and `meta` (tibble or NULL).
#'
#' @param sheet_urls Character. Google Sheet URL or Spreadsheet ID for the URL list.
#' @param worksheet_urls Character or NULL. Tab name for the URL sheet (NULL = first tab).
#' @param url_col Character. Column name containing profile URLs in the URL sheet (default "Profil").
#'
#' @param sheet_meta Character or NULL. Google Sheet URL/ID for metadata. If NULL,
#'        metadata are read from `sheet_urls` instead.
#' @param worksheet_meta Character or NULL. Tab name for metadata sheet (NULL = first tab).
#' @param meta_url_col Character. Column in the metadata sheet that contains the profile URLs
#'        to match against the scraper's `source_url`. Defaults to `url_col` if metadata come
#'        from the same sheet; otherwise default "Profil".
#' @param meta_cols Character vector of additional metadata columns to bring (e.g., c("Posisjon","LSkole2024")).
#'        If NULL or length 0, `meta` will be returned as NULL.
#'
#' @param strip_query Logical. If TRUE, strips query strings (?...) when normalizing/joining.
#'        Use this if your Sheets store base URLs while scraped pages have ?fiksId=...
#'
#' @return list with:
#'   - urls: character vector of unique, non-missing, http(s) URLs
#'   - meta: tibble with columns `source_url` + meta_cols (or NULL if `meta_cols` missing)
#'
#' @ImportFrom googlesheets4 read_sheet
#' @ImportFrom dplyr pull rename mutate group_by summarise across all_of
#' @ImportFrom stringr str_trim str_detect str_replace
#' @ImportFrom tibble tibble
gs_read_urls <- function(sheet_urls,
                         worksheet_urls = NULL,
                         url_col = "Profil",
                         sheet_meta = NULL,
                         worksheet_meta = NULL,
                         meta_url_col = NULL,
                         meta_cols = NULL,
                         strip_query = FALSE) {
  # ---- Normalizer -----------------------------------------------------------
  normalize_url <- function(x) {
    x <- stringr::str_trim(as.character(x))
    if (strip_query) x <- sub("\\?.*$", "", x)
    x <- stringr::str_replace(x, "/+$", "")
    x <- stringr::str_replace(x, "(?<!:)//+", "/")
    x
  }

  # ---- Read URL sheet -------------------------------------------------------
  tb_urls <- googlesheets4::read_sheet(sheet_urls, sheet = worksheet_urls, guess_max = 10000)
  if (!(url_col %in% names(tb_urls))) {
    stop("Column '", url_col, "' not found in URL sheet.")
  }

  urls <- tb_urls |>
    dplyr::pull({{ url_col }}) |>
    as.character() |>
    stringr::str_trim()
  urls <- urls[!is.na(urls) & urls != ""]
  urls <- unique(urls)
  urls <- urls[stringr::str_detect(urls, "^https?://")]

  # ---- Metadata (optional) --------------------------------------------------
  meta <- NULL
  if (!is.null(meta_cols) && length(meta_cols)) {
    # Decide source for metadata
    read_from <- if (is.null(sheet_meta)) sheet_urls else sheet_meta
    tab_meta  <- if (is.null(sheet_meta)) worksheet_urls else worksheet_meta
    # Default meta_url_col
    if (is.null(meta_url_col)) {
      meta_url_col <- if (is.null(sheet_meta)) url_col else "Profil"
    }

    tb_meta <- googlesheets4::read_sheet(read_from, sheet = tab_meta, guess_max = 10000)

    missing_cols <- setdiff(c(meta_url_col, meta_cols), names(tb_meta))
    if (length(missing_cols)) {
      stop("Missing column(s) in metadata sheet: ", paste(missing_cols, collapse = ", "))
    }

    meta <- tb_meta |>
      dplyr::rename(source_url = !!meta_url_col) |>
      dplyr::mutate(source_url = normalize_url(source_url)) |>
      dplyr::group_by(source_url) |>
      dplyr::summarise(
        dplyr::across(dplyr::all_of(meta_cols), ~ {
          v <- .x
          v <- v[!is.na(v) & v != ""]
          if (length(v)) v[1] else NA_character_
        }),
        .groups = "drop"
      )
  }

  list(urls = urls, meta = meta)
}



#' nff_read_many
#'
#' @description
#' Scrape multiple NFF player pages (URLs), robust to failures, with verbose logs.
#' Harmonizes column types before binding. Optionally joins a pre-read metadata
#' tibble (`meta_tbl`) onto selected tables via `source_url`.
#'
#' @param urls Character vector of player URLs.
#' @param pause Seconds to sleep between requests (default 1).
#' @param verbose Logical. Print per-URL progress + summary (default TRUE).
#' @param meta_tbl Tibble or NULL. If provided, must contain `source_url` + columns to join.
#' @param join_to Character vector of tables to enrich with metadata.
#'        Any of c("seasons","tournaments","teams"). Default c("seasons","tournaments").
#' @param fill_missing Logical. If TRUE, fills missing metadata columns with `fill_values`.
#' @param fill_values Named list with default values; e.g. list(Posisjon="ukjent", LSkole2024="ukjent").
#' @param strip_query Logical. If TRUE, strips query strings (?...) from `source_url` before joining.
#'
#' @return list(seasons, tournaments, teams, log), with attributes `failed_urls`, `parsed_n`.
#'
#' @ImportFrom purrr safely map_lgl map map_chr compact
#' @ImportFrom dplyr bind_rows left_join
#' @ImportFrom tibble tibble
#' @ImportFrom stringr str_trim str_replace
nff_read_many <- function(urls,
                          pause = 1,
                          verbose = TRUE,
                          meta_tbl = NULL,
                          join_to = c("seasons","tournaments"),
                          fill_missing = FALSE,
                          fill_values = list(Posisjon="ukjent", LSkole2024="ukjent"),
                          strip_query = FALSE) {
  stopifnot(is.character(urls), length(urls) >= 1)

  # Normalizer for joining
  normalize_url <- function(x) {
    x <- stringr::str_trim(as.character(x))
    if (strip_query) x <- sub("\\?.*$", "", x)
    x <- stringr::str_replace(x, "/+$", "")
    x <- stringr::str_replace(x, "(?<!:)//+", "/")
    x
  }

  # Scrape safely
  safe_one <- purrr::safely(nff_read_player, otherwise = NULL)
  res <- vector("list", length(urls))
  for (i in seq_along(urls)) {
    if (verbose) message(sprintf("[%d/%d] %s", i, length(urls), urls[i]))
    res[[i]] <- safe_one(urls[i], verbose = verbose)
    if (pause > 0 && i < length(urls)) Sys.sleep(pause)
  }

  ok <- purrr::map_lgl(res, ~ !is.null(.x$result))
  failed_urls <- urls[!ok]
  parsed_n <- sum(ok)
  total_n  <- length(urls)
  res_ok <- purrr::map(res[ok], "result")

  # Harmonizer: coerce mixed-type columns to character before binding
  harmonize_for_bind <- function(lst) {
    lst <- purrr::compact(lst)
    if (!length(lst)) return(lst)
    all_cols <- unique(unlist(purrr::map(lst, names)))
    if (!length(all_cols)) return(lst)
    for (nm in all_cols) {
      present <- purrr::map_lgl(lst, ~ nm %in% names(.x))
      if (!any(present)) next
      classes <- purrr::map_chr(lst[present], ~ class(.x[[nm]])[1])
      has_char <- any(classes == "character")
      has_non  <- any(classes != "character")
      if (has_char && has_non) {
        lst[present] <- purrr::map(lst[present], function(df) {
          df[[nm]] <- as.character(df[[nm]])
          df
        })
      }
    }
    lst
  }

  rb <- function(name) {
    if (!length(res_ok)) return(tibble::tibble())
    lst <- purrr::map(res_ok, name)
    lst <- harmonize_for_bind(lst)
    dplyr::bind_rows(lst)
  }

  seasons_df     <- rb("seasons")
  tournaments_df <- rb("tournaments")
  teams_df       <- rb("teams")

  # Logs
  logs_ok <- if (length(res_ok)) dplyr::bind_rows(purrr::map(res_ok, "log")) else tibble::tibble()
  logs_fail <- if (length(failed_urls)) {
    tibble::tibble(
      source_url  = failed_urls,
      status      = "failed",
      player_name = NA_character_,
      n_tables    = 0L,
      note        = NA_character_,
      error       = "read_html() failed"
    )
  } else tibble::tibble()
  logs <- dplyr::bind_rows(logs_ok, logs_fail)

  # Optional join with pre-read metadata
  if (!is.null(meta_tbl) && nrow(meta_tbl)) {
    # Normalize URLs in outputs
    if (nrow(seasons_df))     seasons_df$source_url     <- normalize_url(seasons_df$source_url)
    if (nrow(tournaments_df)) tournaments_df$source_url <- normalize_url(tournaments_df$source_url)
    if (nrow(teams_df))       teams_df$source_url       <- normalize_url(teams_df$source_url)

    # Ensure meta has source_url normalized too
    meta_tbl <- meta_tbl
    meta_tbl$source_url <- normalize_url(meta_tbl$source_url)

    do_join <- function(df) {
      if (!nrow(df)) return(df)
      out <- dplyr::left_join(df, meta_tbl, by = "source_url")
      if (fill_missing && length(fill_values)) {
        for (nm in intersect(names(fill_values), names(out))) {
          out[[nm]][is.na(out[[nm]])] <- fill_values[[nm]]
        }
      }
      out
    }

    if ("seasons" %in% join_to)     seasons_df     <- do_join(seasons_df)
    if ("tournaments" %in% join_to) tournaments_df <- do_join(tournaments_df)
    if ("teams" %in% join_to)       teams_df       <- do_join(teams_df)
  }

  out <- list(
    seasons     = seasons_df,
    tournaments = tournaments_df,
    teams       = teams_df,
    log         = logs
  )
  attr(out, "failed_urls") <- failed_urls
  attr(out, "parsed_n")    <- parsed_n

  if (verbose) {
    msg <- sprintf("Parsed %d of %d URL(s); failed %d.", parsed_n, total_n, length(failed_urls))
    message(msg)
    if (length(failed_urls)) message("Failed URLs:\n - ", paste(failed_urls, collapse = "\n - "))
  }

  out
}
