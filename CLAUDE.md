# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this project does

Norwegian youth football power ranking tool. Computes Elo-based ratings, points projections, and match predictions from match result Excel files (fotball.no, Profixio, Gothia Cup, Norway Cup). Outputs: interactive Shiny dashboard (hosted on Posit Connect Cloud) with PDF report download, plus static Quarto HTML/DOCX reports.

## Two parallel systems

### 1. Shiny app (current, primary)
`app.R` — interactive dashboard, deployed to Posit Connect Cloud. Uses the functional pipeline in `R/`.

### 2. Quarto reports (legacy, still maintained)
`scripts/*.qmd` — static reports rendered per-tournament. Source scripts directly rather than using functions.

---

## Shiny app architecture (`R/` directory)

The app pipeline has four function layers called in sequence:

```
read_and_normalize(file_paths)        # R/normalize_data.R
  → run_rankings(matches, k_factor)   # R/run_rankings.R
    → make_figures(data)              # R/make_figures.R
    → make_tables(data)              # R/make_tables.R
```

### `R/normalize_data.R`
Detects the source format from column names and maps to the standard schema.

- `detect_source(df)` → `"fotball_no"` | `"profixio"` | `"gothia"` | `"norway_cup"` | `"unknown"`
- `normalize_matches(df, source, manual_mapping)` — dispatcher
- `read_and_normalize(file_paths, ...)` — reads one or more uploaded files and returns a single combined normalized data frame

**Standard schema** (what the pipeline expects after normalization):
`Dato` (Date), `Runde` (int), `Hjemmelag` (chr), `Bortelag` (chr), `Resultat` (chr, format: `"2 - 1"`), `Turnering` (chr)

### `R/run_rankings.R`
`run_rankings(matches, k_factor = 60)` — wraps the full v7 pipeline. Returns a named list:
- `team_stats` — long-format, one row per team per match/week (the central object)
- `matches` — wide-format matches with Elo and strength scores
- `team_rankings` — aggregate summary per team
- `end_points_played` / `end_points_projected` — latest snapshot per team (played / including projections)
- `end_points_played_wl` / `end_points_projected_wl` — win/draw/loss counts
- `lines_data` — slope/intercept for ELO-vs-points reference lines
- `date_minimum`, `date_maximum`, `tournament`, `date_season_start`, `k_factor`

### `R/make_figures.R`
`make_figures(data)` — returns a named list of ggplot objects:
`fig_elo`, `fig_points`, `fig_ppg`, `fig_comp`, `fig_heatmap_round`, `fig_heatmap_matchup`, `fig_heatmap_ppg`, `fig_attack_defense`, `fig_home_away`, `fig_elo_momentum`

Defines `theme_anythings_football()` (inline ANYthings brand theme — Inter/Arial, `#1A1A1A`, `#D9D9D9` grid) and `scale_colour_football()` / `scale_fill_football()` (ggsci category20 palette for team differentiation).

### `R/make_tables.R`
`make_tables(data)` — returns a named list of gt objects:
`tbl_current`, `tbl_projected`, `tbl_league`, `tbl_form`, `tbl_homeaway`

Defines `style_gt_anythings()` helper that applies brand styling to any gt table (black header `#1A1A1A`, alternating rows `#F5F5F5`, borders `#D9D9D9`, System Blue for promotion spots, Signal Red for relegation).

### `R/make_report.R`
`generate_report(data, figures, tables, output_path)` — renders `report_template.Rmd` to PDF via `rmarkdown::render()`.

### `report_template.Rmd`
Parameterized R Markdown PDF template. Accepts `params$data`, `params$figures`, `params$tables`, `params$tournament`, `params$k_factor`. Uses XeLaTeX with Arial font and ANYthings header/footer styling. Requires `tinytex` on the server.

---

## Quarto reports (legacy pipeline)

### Data input
Match data in `data/` as `.xlsx` files: `kamper_[tournament]_[YY-MM-DD].xlsx`. Columns: `Dato`, `Runde`, `Hjemmelag`, `Bortelag`, `Resultat` (`"2 - 1"`), `Turnering`.

### Script pipeline (sourced, not functions)
Each `.qmd` sets `data` (xlsx path) and `k_factor`, then:
1. `matches <- readxl::read_excel(data)`
2. `source("power_rankings_v6.R")` or `v7.R` — expects `matches` + `k_factor` in scope
3. `source("figures_and_tables_v1.R")` or `v2.R` — expects `team_stats_by_round_w_strength` + `tournament`

**v7 vs v6**: v7 adds multi-tournament season ordering (`group_by(team, Turnering)`) — use v7 when combining vest + øst + høst datasets. v6 is fine for single-tournament reports.

**v2 vs v1 figures**: v2 uses `ggrepel` for labels and removes `facet_wrap` — use v2 for combined multi-tournament views.

---

## Key parameters

- **`k_factor`**: Elo sensitivity. Higher = more volatile. Default 60. Range used in practice: 60–100.
- **G-factor**: Goal-difference multiplier in `update_elo()` — 1.0 (draw/0), 1.1 (1-goal), 1.5 (2–3), 2.0 (4–5), 2.5 (6+). This is the v6/v7 formula; the old `shiny_dash.R` uses a different formula.
- **`strength_score`**: `total_points + (total_gd / games_played)`. Secondary ranking metric alongside Elo.
- **Projection threshold**: ELO win probability ≥0.55 → win (3 pts), 0.45–0.55 → draw (1 pt), <0.45 → loss.

---

## Deployment (Posit Connect Cloud)

- `DESCRIPTION` file declares all package dependencies for Connect's package detection
- Run `renv::init()` + `renv::snapshot()` to generate `renv.lock` before publishing
- PDF download requires `tinytex` on the Connect instance
- App entry point: `app.R` (standard Shiny single-file convention)

---

## Branding (ANYthings)

This app uses ANYthings corporate visual identity. Key values for inline use (when `anythings` package is unavailable):

| Token | Hex | Use |
|---|---|---|
| ANYthings Black | `#1A1A1A` | Headings, navbar, table headers |
| Body text | `#333333` | Body, axis labels |
| Medium Grey | `#767676` | Captions, subtitles, secondary |
| Light Grey | `#D9D9D9` | Grid lines, borders |
| Near White | `#F5F5F5` | Alternate rows, backgrounds |
| Signal Red | `#E03C31` | Alerts, relegation, loss |
| System Blue | `#0B3D91` | Links, promotion, home venue |

Web font: **Inter** (Google Fonts). Fallback: Arial. Code: JetBrains Mono.

bslib theme is defined inline in `app.R` as `theme_anythings_shiny`. ggplot theme is `theme_anythings_football()` in `R/make_figures.R`.

---

## Web scraping (player stats)

`nff_stat_scraper_functions.R` — three functions:
- `nff_read_player(url)` — scrapes a single NFF player page → list of tibbles (`seasons`, `tournaments`, `teams`, `log`)
- `gs_read_urls(sheet_urls, ...)` — reads player profile URLs + metadata from a Google Sheet
- `nff_read_many(urls, ...)` — batch scraper with retry, rate-limiting, metadata join

`nff_stat_scraper_j2012.R` — example usage for a specific team, sourcing from Google Sheets (`gs4_auth()` required).

`scripts/helpers.R` — `download_tournament_calendar(url, save_path)` downloads NFF schedule Excel exports via `httr2`.

---

## Key R packages

**App**: `shiny`, `bslib`, `dplyr`, `tidyr`, `lubridate`, `stringr`, `readxl`, `writexl`, `ggplot2`, `ggrepel`, `ggsci`, `gt`, `DT`, `rmarkdown`, `rlang`

**Reports (Quarto)**: `tidyverse`, `lubridate`, `readxl`, `ggplot2`, `ggrepel`, `hrbrthemes`, `ggsci`, `gt`

**Scraper**: `httr2`, `rvest`, `googlesheets4`, `janitor`, `purrr`
