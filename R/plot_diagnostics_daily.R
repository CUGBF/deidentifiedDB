#' Plot
#'
#' @param diagnostics_tbl
#'
#' @return
#' @export
#'
#' @examples
plot_diagnostics_daily <- function(diagnostics_tbl) {
  stopifnot(all(c("run_date", "result") %in% colnames(diagnostics_tbl)))

  diagnostics_tbl %>%
    group_by(run_date, result) %>%
    summarise(count = n()) %>%
    ggline(
      x = "run_date",
      y = "count",
      color = "result",
      title = "Result Status - Daily Counts",
      xlab = "Diagnostics Run Date",
      ylab = "Number of Samples",
      legend.title = "Result",
      yscale = "log10",
      ggtheme = theme_minimal()
    )
}
