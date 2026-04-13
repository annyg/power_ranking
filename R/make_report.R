# R/make_report.R
# Thin wrapper around rmarkdown::render() for the PDF report.
# Called by app.R's downloadHandler.

#' Render the ANYthings PDF report.
#'
#' @param data      Output of run_rankings().
#' @param figures   Output of make_figures().
#' @param tables    Output of make_tables().
#' @param output_path Destination file path for the rendered PDF.
#' @param template  Path to the .Rmd template (default: report_template.Rmd).
#' @return Invisibly, the output file path.
generate_report <- function(data, figures, tables,
                             output_path,
                             template = "report_template.Rmd") {
  rmarkdown::render(
    input       = template,
    output_file = output_path,
    params      = list(
      data       = data,
      figures    = figures,
      tables     = tables,
      tournament = data$tournament,
      k_factor   = data$k_factor
    ),
    envir = new.env(parent = globalenv()),
    quiet = TRUE
  )
  invisible(output_path)
}
