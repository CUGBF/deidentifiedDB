#' Plot Weekly Test Positivity Rate
#'
#' @param diagnostics_tbl Tibble with diagnostics data (should contain at least
#' two columns: 'run date' and 'result')
#' @param start_date start date for the plot
#' @param end_date end date for the plot
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom magrittr "%>%"
plot_weekly_tpr <- function(diagnostics_tbl,
                            start_date = NULL,
                            end_date = NULL) {
  stopifnot(all(c("run_date", "result") %in% colnames(diagnostics_tbl)))
  stopifnot(all(c("positive", "negative") %in% unique(diagnostics_tbl$result)))

  if (is.null(start_date)) {
    start_date <- min(diagnostics_tbl$run_date)
  }
  if (is.null(end_date)) {
    start_date <- max(diagnostics_tbl$run_date)
  }

  diagnostics_tbl %>%
    dplyr::filter(.data$result %in% c("positive", "negative")) %>%
    dplyr::mutate(result = factor(.data$result,
      levels = c("positive", "negative")
    )) %>%
    dplyr::group_by(.data$run_date, .data$result) %>%
    dplyr::summarise(count = dplyr::n()) %>%
    ggpubr::ggbarplot(
      x = "run_date",
      y = "count",
      fill = "result",
      alpha = 0.7,
      palette = "npg",
      remove = NULL,
      title = "Weekly Test Positivity Rate",
      xlab = "Diagnostics Run Date",
      ylab = "Number of Samples",
      legend.title = "Result",
      ggtheme = ggplot2::theme_minimal()
    )
}
