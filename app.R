# app.R
# Football Analytics Dashboard
# ANYthings — Power Rankings & Match Analytics
#
# Hosted on: Posit Connect Cloud
# Brand:     ANYthings visual identity (Inter, #1A1A1A, #E03C31, #0B3D91)

library(shiny)
library(bslib)
library(dplyr)
library(readxl)
library(gt)
library(DT)
library(ggplot2)
library(tidyr)
library(lubridate)
library(stringr)
library(ggrepel)
library(ggsci)
library(rmarkdown)
library(plotly)

# Load pipeline layers
source("R/normalize_data.R")
source("R/run_rankings.R")
source("R/make_figures.R")
source("R/make_figures_plotly.R")
source("R/make_tables.R")

# ---------------------------------------------------------------------------
# ANYthings bslib theme
# ---------------------------------------------------------------------------
theme_anythings_shiny <- bslib::bs_theme(
  version      = 5,
  bootswatch   = NULL,
  bg           = "#FFFFFF",
  fg           = "#1A1A1A",
  primary      = "#0B3D91",   # System Blue
  secondary    = "#767676",   # Medium Grey
  danger       = "#E03C31",   # Signal Red
  success      = "#2E7D32",
  warning      = "#F5A623",
  info         = "#0B3D91",
  base_font    = bslib::font_google("Inter", wght = c(400, 600, 700)),
  heading_font = bslib::font_google("Inter", wght = c(700)),
  code_font    = bslib::font_google("JetBrains Mono"),
  font_scale   = 0.95,
  `--bs-border-color`       = "#D9D9D9",
  `--bs-body-bg`            = "#F5F5F5",
  `--bs-card-border-color`  = "#D9D9D9",
  `--bs-navbar-bg`          = "#1A1A1A",
  `--bs-navbar-color`       = "#FFFFFF",
  `--bs-navbar-hover-color` = "#D9D9D9",
  `--bs-navbar-active-color`= "#FFFFFF"
) |>
  bslib::bs_add_rules("

    /* ── Page & layout ───────────────────────────────────────────── */
    body { background-color: #F5F5F5; }

    /* ── Navbar ──────────────────────────────────────────────────── */
    .navbar { border-bottom: 3px solid #0B3D91 !important; }

    .navbar .nav-link {
      font-size: 0.82rem;
      font-weight: 600;
      letter-spacing: 0.03em;
      padding-bottom: 0.6rem !important;
      transition: color 0.15s ease;
    }
    .navbar .nav-link.active {
      border-bottom: 3px solid #0B3D91;
      margin-bottom: -3px;
    }
    .navbar .nav-link:hover { color: #D9D9D9 !important; }

    /* ── Cards ───────────────────────────────────────────────────── */
    .card {
      background-color: #FFFFFF;
      border: 1px solid #D9D9D9;
      border-radius: 3px;
      box-shadow: 0 1px 4px rgba(26, 26, 26, 0.07);
    }
    .card-header {
      background-color: #1A1A1A !important;
      color: #FFFFFF !important;
      font-weight: 700;
      font-size: 0.72rem;
      letter-spacing: 0.08em;
      text-transform: uppercase;
      padding: 0.5rem 1rem;
      border-bottom: 2px solid #0B3D91 !important;
    }

    /* ── Sidebar ─────────────────────────────────────────────────── */
    .bslib-sidebar-layout > .sidebar {
      background-color: #FFFFFF !important;
      border-right: 1px solid #D9D9D9 !important;
    }
    .bslib-sidebar-layout > .sidebar .sidebar-content {
      padding: 1rem 1.1rem;
    }

    /* ── Form controls ───────────────────────────────────────────── */
    .control-label, label:not(.form-check-label) {
      font-weight: 600;
      font-size: 0.8rem;
      color: #1A1A1A;
      margin-bottom: 0.25rem;
    }
    .form-text, .shiny-input-container .help-block {
      color: #767676;
      font-size: 0.78rem;
    }
    hr { border-color: #D9D9D9 !important; opacity: 1 !important; }

    /* ── Slider (K-factor) ───────────────────────────────────────── */
    .irs--shiny .irs-bar {
      background-color: #0B3D91;
      border-top-color: #0B3D91;
      border-bottom-color: #0B3D91;
    }
    .irs--shiny .irs-bar--single { border-left-color: #0B3D91; }
    .irs--shiny .irs-handle       { border-color: #0B3D91; }
    .irs--shiny .irs-single,
    .irs--shiny .irs-from,
    .irs--shiny .irs-to {
      background-color: #0B3D91;
      font-size: 0.75rem;
    }

    /* ── Buttons ─────────────────────────────────────────────────── */
    .btn-primary {
      background-color: #0B3D91;
      border-color: #0B3D91;
      font-weight: 600;
      letter-spacing: 0.04em;
    }
    .btn-primary:hover, .btn-primary:focus {
      background-color: #092f72;
      border-color: #092f72;
    }
    .btn-outline-primary {
      color: #0B3D91;
      border-color: #0B3D91;
      font-weight: 600;
      letter-spacing: 0.04em;
    }
    .btn-outline-primary:hover {
      background-color: #0B3D91;
      border-color: #0B3D91;
    }

    /* ── Alerts ──────────────────────────────────────────────────── */
    .alert-success {
      background-color: #F0F7F0;
      border-left: 4px solid #2E7D32;
      border-top: none;
      border-right: none;
      border-bottom: none;
      border-radius: 0;
      color: #1A1A1A;
      font-size: 0.85rem;
    }

    /* ── Badges ──────────────────────────────────────────────────── */
    .badge {
      font-size: 0.72rem;
      font-weight: 600;
      letter-spacing: 0.04em;
      padding: 0.3em 0.65em;
      border-radius: 2px;
    }

    /* ── Select / file inputs ────────────────────────────────────── */
    .form-control, .form-select {
      border-color: #D9D9D9;
      border-radius: 3px;
      font-size: 0.85rem;
    }
    .form-control:focus, .form-select:focus {
      border-color: #0B3D91;
      box-shadow: 0 0 0 0.2rem rgba(11, 61, 145, 0.15);
    }

    /* ── DT table ────────────────────────────────────────────────── */
    .dataTables_wrapper { font-size: 0.83rem; color: #333333; }
    table.dataTable thead th {
      background-color: #1A1A1A;
      color: #FFFFFF;
      font-weight: 600;
      font-size: 0.72rem;
      letter-spacing: 0.06em;
      text-transform: uppercase;
      border-bottom: 2px solid #0B3D91 !important;
    }
    table.dataTable tbody tr:hover { background-color: #F5F5F5 !important; }
    table.dataTable tbody tr.odd  { background-color: #FAFAFA; }
    table.dataTable tbody tr.even { background-color: #FFFFFF; }
    .dataTables_info, .dataTables_paginate { font-size: 0.78rem; color: #767676; }

    /* ── Progress bar ────────────────────────────────────────────── */
    .progress-bar { background-color: #0B3D91; }

  ")

# ---------------------------------------------------------------------------
# Known sources list (for override dropdown)
# ---------------------------------------------------------------------------
SOURCE_CHOICES <- c(
  "Auto-detect"  = "auto",
  "fotball.no"   = "fotball_no",
  "Profixio"     = "profixio",
  "Gothia Cup"   = "gothia",
  "Norway Cup"   = "norway_cup",
  "Manual mapping" = "unknown"
)

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------
ui <- bslib::page_navbar(
  title = shiny::tags$span(
    shiny::tags$strong("Football Analytics"),
    shiny::tags$span(" | ANYthings", style = "font-size: 0.75em; color: #767676; margin-left: 4px;")
  ),
  theme     = theme_anythings_shiny,
  fillable  = FALSE,
  id        = "navbar",

  # -------------------------------------------------------------------------
  # Tab 1: Upload
  # -------------------------------------------------------------------------
  bslib::nav_panel(
    title = shiny::tagList(shiny::icon("upload"), "Upload"),
    bslib::page_sidebar(
      sidebar = bslib::sidebar(
        width = 300,
        shiny::fileInput(
          "files",
          "Upload match result file(s)",
          multiple = TRUE,
          accept   = c(".xlsx", ".xls", ".csv"),
          placeholder = "fotball.no, Profixio, Gothia, …"
        ),
        shiny::selectInput(
          "source_override",
          "Data source",
          choices  = SOURCE_CHOICES,
          selected = "auto"
        ),

        # Manual mapping UI — shown only when "Manual mapping" is selected
        shiny::conditionalPanel(
          condition = "input.source_override == 'unknown'",
          shiny::hr(),
          shiny::strong("Manual column mapping"),
          shiny::uiOutput("mapping_ui")
        ),

        shiny::hr(),
        shiny::sliderInput(
          "k_factor",
          "K-factor (Elo sensitivity)",
          min   = 20, max = 100, step = 5, value = 60
        ),
        shiny::helpText("Higher K = more volatile ratings after each match."),
        shiny::hr(),
        shiny::actionButton(
          "run_analysis",
          "Run Analysis",
          class = "btn-primary w-100",
          icon  = shiny::icon("play")
        ),
        shiny::br(),
        shiny::uiOutput("analysis_status")
      ),

      # Main panel — data preview
      bslib::card(
        bslib::card_header("Data preview"),
        shiny::uiOutput("source_badge"),
        shiny::br(),
        DT::dataTableOutput("data_preview")
      )
    )
  ),

  # -------------------------------------------------------------------------
  # Tab 2: Standings
  # -------------------------------------------------------------------------
  bslib::nav_panel(
    title = "Standings",
    bslib::layout_column_wrap(
      width = 1,
      bslib::card(
        bslib::card_header("Current Standings"),
        shiny::uiOutput("tbl_current")
      ),
      bslib::card(
        bslib::card_header("Projected Final Standings"),
        shiny::uiOutput("tbl_projected")
      ),
      bslib::card(
        bslib::card_header("League Summary"),
        shiny::uiOutput("tbl_league")
      )
    )
  ),

  # -------------------------------------------------------------------------
  # Tab 3: ELO & Ratings
  # -------------------------------------------------------------------------
  bslib::nav_panel(
    title = "ELO & Ratings",
    bslib::layout_column_wrap(
      width = 1,
      bslib::card(
        bslib::card_header("ELO Rating Over Season"),
        plotly::plotlyOutput("fig_elo", height = "480px")
      ),
      bslib::layout_column_wrap(
        width = 1/2,
        bslib::card(
          bslib::card_header("ELO vs Points"),
          plotly::plotlyOutput("fig_comp", height = "400px")
        ),
        bslib::card(
          bslib::card_header("ELO Momentum — Most Recent Round"),
          plotly::plotlyOutput("fig_elo_momentum", height = "400px")
        )
      )
    )
  ),

  # -------------------------------------------------------------------------
  # Tab 4: Form & Points
  # -------------------------------------------------------------------------
  bslib::nav_panel(
    title = "Form & Points",
    bslib::layout_column_wrap(
      width = 1,
      bslib::layout_column_wrap(
        width = 1/2,
        bslib::card(
          bslib::card_header("Cumulative Points"),
          plotly::plotlyOutput("fig_points", height = "420px")
        ),
        bslib::card(
          bslib::card_header("Points Per Game"),
          plotly::plotlyOutput("fig_ppg", height = "420px")
        )
      ),
      bslib::card(
        bslib::card_header("Recent Form — Last 5 Matches"),
        shiny::uiOutput("tbl_form")
      )
    )
  ),

  # -------------------------------------------------------------------------
  # Tab 5: Match Analysis
  # -------------------------------------------------------------------------
  bslib::nav_panel(
    title = "Match Analysis",
    bslib::layout_column_wrap(
      width = 1,
      bslib::card(
        bslib::card_header("Match Outcomes by Round"),
        plotly::plotlyOutput("fig_heatmap_round", height = "600px")
      ),
      bslib::layout_column_wrap(
        width = 1/2,
        bslib::card(
          bslib::card_header("Head-to-Head Results"),
          plotly::plotlyOutput("fig_heatmap_matchup", height = "480px")
        ),
        bslib::card(
          bslib::card_header("PPG by Round"),
          plotly::plotlyOutput("fig_heatmap_ppg", height = "480px")
        )
      )
    )
  ),

  # -------------------------------------------------------------------------
  # Tab 6: Attack & Defence
  # -------------------------------------------------------------------------
  bslib::nav_panel(
    title = "Attack & Defence",
    bslib::card(
      bslib::card_header("Attack vs Defence Profile"),
      plotly::plotlyOutput("fig_attack_defense", height = "520px")
    )
  ),

  # -------------------------------------------------------------------------
  # Tab 7: Home & Away
  # -------------------------------------------------------------------------
  bslib::nav_panel(
    title = "Home & Away",
    bslib::layout_column_wrap(
      width = 1/2,
      bslib::card(
        bslib::card_header("Home vs Away Performance"),
        plotly::plotlyOutput("fig_home_away", height = "480px")
      ),
      bslib::card(
        bslib::card_header("Home vs Away Breakdown"),
        shiny::uiOutput("tbl_homeaway")
      )
    )
  ),

  # -------------------------------------------------------------------------
  # Tab 8: Download
  # -------------------------------------------------------------------------
  bslib::nav_panel(
    title = "Download",
    bslib::page_sidebar(
      sidebar = bslib::sidebar(
        width = 280,
        shiny::h5("Export options"),
        shiny::hr(),
        shiny::downloadButton(
          "download_pdf",
          "Download PDF Report",
          class = "btn-primary w-100"
        ),
        shiny::br(), shiny::br(),
        shiny::downloadButton(
          "download_xlsx",
          "Download Standings (.xlsx)",
          class = "btn-outline-primary w-100"
        ),
        shiny::hr(),
        shiny::helpText(
          "The PDF report contains all charts and tables from this dashboard.",
          "Excel export includes current and projected standings."
        )
      ),
      bslib::card(
        bslib::card_header("Report preview"),
        shiny::uiOutput("download_status")
      )
    )
  ),

  # -------------------------------------------------------------------------
  # Footer
  # -------------------------------------------------------------------------
  bslib::nav_spacer(),
  bslib::nav_item(
    shiny::tags$a(
      "ANYthings",
      href   = "#",
      style  = "color: #767676; font-size: 0.85em;"
    )
  )
)

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------
server <- function(input, output, session) {

  # -- Raw uploaded data ------------------------------------------------------
  raw_data <- shiny::reactive({
    shiny::req(input$files)
    tryCatch({
      src <- if (input$source_override == "auto") NULL else input$source_override
      read_and_normalize(input$files$datapath, source = src)
    }, error = function(e) {
      shiny::showNotification(
        paste("Data error:", conditionMessage(e)),
        type     = "error",
        duration = 10
      )
      NULL
    })
  })

  # -- Detected source badge --------------------------------------------------
  output$source_badge <- shiny::renderUI({
    shiny::req(input$files)
    df  <- tryCatch(readxl::read_excel(input$files$datapath[1]), error = function(e) NULL)
    shiny::req(df)
    src <- detect_source(df)
    src_label <- switch(src,
      fotball_no  = list(label = "fotball.no",  cls = "bg-primary"),
      profixio    = list(label = "Profixio",    cls = "bg-info text-dark"),
      gothia      = list(label = "Gothia Cup",  cls = "bg-success"),
      norway_cup  = list(label = "Norway Cup",  cls = "bg-warning text-dark"),
      list(label = "Unknown format", cls = "bg-danger")
    )
    shiny::tags$span(
      class = paste("badge", src_label$cls),
      paste("Detected source:", src_label$label)
    )
  })

  # -- Manual mapping UI (only for unknown format) ----------------------------
  output$mapping_ui <- shiny::renderUI({
    shiny::req(input$files, input$source_override == "unknown")
    df <- tryCatch(readxl::read_excel(input$files$datapath[1]), error = function(e) NULL)
    shiny::req(df)
    cols <- c("— select —", names(df))
    shiny::tagList(
      shiny::selectInput("map_date",       "Date column",       cols),
      shiny::selectInput("map_round",      "Round column",      cols),
      shiny::selectInput("map_home",       "Home team column",  cols),
      shiny::selectInput("map_away",       "Away team column",  cols),
      shiny::selectInput("map_score",      "Score column (combined)", cols),
      shiny::helpText("— OR use separate goal columns —"),
      shiny::selectInput("map_home_score", "Home goals column", cols),
      shiny::selectInput("map_away_score", "Away goals column", cols),
      shiny::selectInput("map_tournament", "Tournament column (optional)", cols),
      shiny::textInput("map_tournament_name", "Tournament name (fallback)", value = "Custom Tournament")
    )
  })

  # -- Data preview -----------------------------------------------------------
  output$data_preview <- DT::renderDataTable({
    shiny::req(raw_data())
    DT::datatable(
      head(raw_data(), 25),
      options = list(
        dom        = "tp",
        pageLength = 10,
        scrollX    = TRUE
      ),
      class  = "compact hover",
      rownames = FALSE
    )
  })

  # -- Core pipeline (triggered by button) ------------------------------------
  pipeline_result <- shiny::eventReactive(input$run_analysis, {
    shiny::req(raw_data())
    shiny::withProgress(message = "Running ELO pipeline…", value = 0, {
      shiny::incProgress(0.2, detail = "Normalising data")
      df <- raw_data()
      shiny::incProgress(0.3, detail = "Calculating Elo ratings")
      result <- run_rankings(df, k_factor = input$k_factor)
      shiny::incProgress(0.2, detail = "Building figures")
      figs <- make_figures(result)
      shiny::incProgress(0.2, detail = "Building interactive figures")
      figs_plotly <- make_figures_plotly(result, figs)
      shiny::incProgress(0.1, detail = "Building tables")
      tbls <- make_tables(result)
      list(data = result, figures = figs, figures_plotly = figs_plotly, tables = tbls)
    })
  })

  # -- Status indicator -------------------------------------------------------
  output$analysis_status <- shiny::renderUI({
    if (!is.null(pipeline_result())) {
      n_teams   <- nrow(pipeline_result()$data$end_points_played)
      n_matches <- nrow(pipeline_result()$data$matches)
      shiny::div(
        class = "alert alert-success p-2 mt-2",
        shiny::icon("check-circle"),
        shiny::strong(" Analysis complete"),
        shiny::br(),
        shiny::tags$small(
          sprintf("%d teams · %d matches · K=%d",
                  n_teams, n_matches, input$k_factor)
        )
      )
    }
  })

  # -- Helper: safe figure access (plotly figures for the dashboard) ----------
  get_fig <- function(name) {
    shiny::req(pipeline_result())
    pipeline_result()$figures_plotly[[name]]
  }
  get_tbl <- function(name) {
    shiny::req(pipeline_result())
    pipeline_result()$tables[[name]]
  }

  # -- Plot outputs (plotly for the dashboard; ggplot2 figures are used for PDF)
  output$fig_elo             <- plotly::renderPlotly({ get_fig("fig_elo") })
  output$fig_points          <- plotly::renderPlotly({ get_fig("fig_points") })
  output$fig_ppg             <- plotly::renderPlotly({ get_fig("fig_ppg") })
  output$fig_comp            <- plotly::renderPlotly({ get_fig("fig_comp") })
  output$fig_heatmap_round   <- plotly::renderPlotly({ get_fig("fig_heatmap_round") })
  output$fig_heatmap_matchup <- plotly::renderPlotly({ get_fig("fig_heatmap_matchup") })
  output$fig_heatmap_ppg     <- plotly::renderPlotly({ get_fig("fig_heatmap_ppg") })
  output$fig_attack_defense  <- plotly::renderPlotly({ get_fig("fig_attack_defense") })
  output$fig_home_away       <- plotly::renderPlotly({ get_fig("fig_home_away") })
  output$fig_elo_momentum    <- plotly::renderPlotly({ get_fig("fig_elo_momentum") })

  # -- GT table outputs -------------------------------------------------------
  output$tbl_current   <- shiny::renderUI({ shiny::HTML(gt::as_raw_html(get_tbl("tbl_current"))) })
  output$tbl_projected <- shiny::renderUI({ shiny::HTML(gt::as_raw_html(get_tbl("tbl_projected"))) })
  output$tbl_league    <- shiny::renderUI({ shiny::HTML(gt::as_raw_html(get_tbl("tbl_league"))) })
  output$tbl_form      <- shiny::renderUI({ shiny::HTML(gt::as_raw_html(get_tbl("tbl_form"))) })
  output$tbl_homeaway  <- shiny::renderUI({ shiny::HTML(gt::as_raw_html(get_tbl("tbl_homeaway"))) })

  # -- Download: PDF report ---------------------------------------------------
  output$download_pdf <- shiny::downloadHandler(
    filename = function() {
      safe_name <- stringr::str_replace_all(
        pipeline_result()$data$tournament,
        "[^A-Za-z0-9_-]", "_"
      )
      paste0("football_analytics_", safe_name, "_",
             format(Sys.Date(), "%Y%m%d"), ".pdf")
    },
    content = function(file) {
      shiny::req(pipeline_result())
      res  <- pipeline_result()$data
      figs <- pipeline_result()$figures
      tbls <- pipeline_result()$tables

      # Render in a temp dir so relative paths work
      tmp_dir  <- tempdir()
      tmp_rmd  <- file.path(tmp_dir, "report_template.Rmd")
      file.copy("report_template.Rmd", tmp_rmd, overwrite = TRUE)

      # Locate TinyTeX and add its binaries to PATH.
      # tinytex_root() can fail in VSCode/renv sessions where the R profile
      # hasn't registered the TinyTeX PATH, so fall back to the standard
      # Windows location (%APPDATA%/TinyTeX) if auto-detect returns empty.
      local({
        root <- tryCatch(tinytex::tinytex_root(), error = function(e) "")
        if (!nzchar(root)) {
          candidate <- file.path(Sys.getenv("APPDATA"), "TinyTeX")
          if (dir.exists(candidate)) root <- candidate
        }
        if (nzchar(root)) {
          tryCatch(tinytex::use_tinytex(root), error = function(e) NULL)
        }
      })
      shiny::validate(
        shiny::need(
          nzchar(Sys.which("xelatex")),
          "PDF export requires TinyTeX. Run tinytex::install_tinytex(repository = 'github') in the R console."
        )
      )

      shiny::withProgress(message = "Rendering PDF report…", value = 0.5, {
        out <- rmarkdown::render(
          input       = tmp_rmd,
          output_file = file,
          params      = list(
            data       = res,
            figures    = figs,
            tables     = tbls,
            tournament = res$tournament,
            k_factor   = res$k_factor
          ),
          envir = new.env(parent = globalenv()),
          quiet = TRUE
        )
      })
    }
  )

  # -- Download: Excel standings ----------------------------------------------
  output$download_xlsx <- shiny::downloadHandler(
    filename = function() {
      safe_name <- stringr::str_replace_all(
        pipeline_result()$data$tournament,
        "[^A-Za-z0-9_-]", "_"
      )
      paste0("standings_", safe_name, "_",
             format(Sys.Date(), "%Y%m%d"), ".xlsx")
    },
    content = function(file) {
      shiny::req(pipeline_result())
      res <- pipeline_result()$data

      current_df <- res$end_points_played %>%
        dplyr::left_join(res$end_points_played_wl, by = "team") %>%
        dplyr::select(team, games_played, win, draw, loss,
                      total_gd, total_points, points_per_game, team_elo) %>%
        dplyr::arrange(-total_points, -total_gd, -team_elo) %>%
        dplyr::mutate(rank = dplyr::row_number(), .before = 1)

      projected_df <- res$end_points_projected %>%
        dplyr::left_join(res$end_points_projected_wl, by = "team") %>%
        dplyr::select(team, games_played, win, draw, loss,
                      total_gd, total_points, points_per_game, team_elo) %>%
        dplyr::arrange(-total_points, -total_gd, -team_elo) %>%
        dplyr::mutate(rank = dplyr::row_number(), .before = 1)

      writexl::write_xlsx(
        list(
          "Current Standings"   = current_df,
          "Projected Standings" = projected_df,
          "Team Rankings"       = res$team_rankings
        ),
        path = file
      )
    }
  )

  # -- Download status --------------------------------------------------------
  output$download_status <- shiny::renderUI({
    if (is.null(pipeline_result())) {
      shiny::div(
        class = "text-muted p-3",
        shiny::icon("info-circle"),
        " Run an analysis first to enable downloads."
      )
    } else {
      res <- pipeline_result()$data
      shiny::div(
        class = "p-3",
        shiny::tags$ul(
          class = "list-unstyled",
          shiny::tags$li(shiny::icon("check", class = "text-success"),
                         " ELO analysis complete"),
          shiny::tags$li(shiny::icon("check", class = "text-success"),
                         paste0(" ", nrow(res$end_points_played), " teams")),
          shiny::tags$li(shiny::icon("check", class = "text-success"),
                         paste0(" Tournament: ", res$tournament)),
          shiny::tags$li(shiny::icon("check", class = "text-success"),
                         paste0(" K-factor: ", res$k_factor))
        ),
        shiny::p(
          class = "text-muted",
          shiny::tags$small("PDF report includes all charts and tables. Requires a LaTeX installation (tinytex).")
        )
      )
    }
  })
}

# ---------------------------------------------------------------------------
shiny::shinyApp(ui = ui, server = server)
