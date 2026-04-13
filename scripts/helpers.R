# install.packages(c("httr2","readxl","dplyr","purrr","tibble","stringr"))

#' download_tournament_calendar
#' @description
#' Download an NFF tournament calendar (Excel) from a URL and return a list of
#' tibbles (one per sheet). Optionally save the downloaded file locally.
#'
#' @param url Character. Fully encoded URL to the Excel export endpoint.
#' @param save_path Character or NULL. If given, the Excel file is also saved here.
#' @return A named list of tibbles, one per sheet (often the first is the main schedule).
#'
#' @ImportFrom httr2 request req_user_agent req_timeout req_retry req_perform resp_status resp_body_raw
#' @ImportFrom readxl excel_sheets read_xlsx
#' @ImportFrom purrr set_names map
#' @ImportFrom tibble as_tibble
#' @ImportFrom dplyr mutate across
download_tournament_calendar <- function(url, save_path = NULL) {
  # Fetch
  resp <- httr2::request(url) |>
    httr2::req_user_agent("R/httr2 (contact: your@email)") |>
    httr2::req_timeout(30) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_perform()

  status <- httr2::resp_status(resp)
  if (status >= 400) stop("Request failed with status ", status, ".")

  # Write to a temp .xlsx (readxl needs a path)
  tf <- tempfile(fileext = ".xlsx")
  writeBin(httr2::resp_body_raw(resp), tf)

  # Optionally save a copy
  if (!is.null(save_path)) {
    dir.create(dirname(save_path), showWarnings = FALSE, recursive = TRUE)
    file.copy(tf, save_path, overwrite = TRUE)
  }

  # Read all sheets to tibbles (common case is a single sheet)
  sheets <- readxl::excel_sheets(tf)
  out <- purrr::map(sheets, ~ readxl::read_xlsx(tf, sheet = .x)) |>
    purrr::set_names(sheets)

  out
}


