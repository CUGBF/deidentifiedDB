#' Plot Counts of Different Diagnostics Result Categories Over Time
#'
#' @param diagnostics_tbl Tibble with diagnostics data (should contain at least
#' two columns: 'run date' and 'result')
#' @param start_date start date for the plot
#' @param end_date end date for the plot
#'
#' @return Line plot with Counts of Different Diagnostics Result
#' Categories Over Time
#' @export
#'
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
plot_diagnostics_daily <- function(diagnostics_tbl,
                                   start_date = NULL,
                                   end_date = NULL) {
  stopifnot(all(c("run_date", "result") %in% colnames(diagnostics_tbl)))

  if (is.null(start_date)) {
    start_date <- min(diagnostics_tbl$run_date)
  }
  if (is.null(end_date)) {
    start_date <- max(diagnostics_tbl$run_date)
  }

  diagnostics_tbl %>%
    dplyr::filter(
      .data$run_date >= start_date,
      .data$run_date <= end_date
    ) %>%
    dplyr::group_by(.data$run_date, .data$result) %>%
    dplyr::summarise(count = dplyr::n()) %>%
    ggpubr::ggline(
      x = "run_date",
      y = "count",
      color = "result",
      title = "Result Status - Daily Counts",
      xlab = "Diagnostics Run Date",
      ylab = "Number of Samples",
      legend.title = "Result",
      yscale = "log10",
      ggtheme = ggplot2::theme_minimal()
    )
}
