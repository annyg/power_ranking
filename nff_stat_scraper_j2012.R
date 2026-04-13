library(googlesheets4)
library(dplyr)
library(stringr)
library(purrr)
library(readr)

# source("R/nff_stat_scraper_functions.R")

# gs4_auth()

sheet_url <- "https://docs.google.com/spreadsheets/d/13JL5igW7L1wu_s4-eTk5bTkvmstjlc_FRhkgOlIYee0/edit?gid=0#gid=0"  # <- replace
# urls <- gs_read_urls(sheet_url, worksheet = "Spillerprofiler", col = "Profil")

# out <- nff_read_many(
#   urls,
#   pause = 1,
#   verbose = TRUE,
#   meta_sheet      = sheet_url,
#   meta_worksheet  = "Spillerprofiler",
#   meta_url_col    = "Profil",
#   meta_cols       = c("Posisjon","LSkole2024"),
#   join_to         = c("seasons","tournaments"),    # add "teams" if you want
#   fill_missing    = TRUE,
#   fill_values     = list(Posisjon="ukjent", LSkole2024="ukjent"),
#   strip_query     = FALSE
# )

# teams <- out$teams

# 1) Read URLs (from one sheet) + metadata (from another)
gs <- gs_read_urls(
  sheet_urls      = sheet_url,
  worksheet_urls  = "Spillerprofiler",
  url_col         = "Profil",
  sheet_meta      = sheet_url,
  worksheet_meta  = "Spillerprofiler",
  meta_url_col    = "Profil",
  meta_cols       = c("Posisjon","LSkole2024"),
  strip_query     = FALSE   # set TRUE if your Sheets lack ?fiksId=… while scraped URLs include it
)

# 2) Scrape + join
out <- nff_read_many(
  urls         = gs$urls,
  pause        = 1,
  verbose      = TRUE,
  meta_tbl     = gs$meta,
  join_to      = c("seasons","tournaments"),                  # add "teams" if needed
  fill_missing = TRUE,
  fill_values  = list(Posisjon = "ukjent", LSkole2024 = "ukjent"),
  strip_query  = FALSE
)

# Results
out$seasons
out$tournaments
out$log


seasons <- out$seasons
tournaments <- out$tournaments

write_sheet(seasons, ss = sheet_url, sheet = "Sesong")
write_sheet(tournaments, ss = sheet_url, sheet = "Turneringer")
# write_sheet(teams, ss = sheet_url, sheet = "Lag")
