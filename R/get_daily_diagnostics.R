#' Title
#'
#' @param diagnostics_tbl
#' @param start_date
#' @param end_date
#' @param add_variables
#'
#' @return
#' @export
#'
#' @examples
get_daily_diagnostics <- function(diagnostics_tbl,
                                  start_date,
                                  end_date,
                                  add_variables = NULL){

  stopifnot(all(c("testkit_id","run_date", "result") %in% colnames(diagnostics_tbl)))

  if (!(lubridate::is.Date(diagnostics_tbl$run_date))){
    diagnostics_tbl <- diagnostics_tbl %>%
      dplyr::mutate(run_date = lubridate::as_date(.data$run_date))
  }

  start_date <- lubridate::as_date(start_date)
  end_date <- lubridate::as_date(end_date)

  grouping_variable <- c("run_date",
                         "result")

  if (!is.null(add_variables)){
    grouping_variable <- c(grouping_variable,
                           add_variables)
  }

  diagnostics_tbl <- diagnostics_tbl %>%
    dplyr::filter(
      .data$run_date >= start_date,
      .data$run_date <= end_date
    )

  count_tbl <- diagnostics_tbl %>%
    dplyr::group_by(grouping_variable) %>%
    dplyr::summarise(count = dplyr::n())

  repeat_samples <- diagnostics_tbl %>%
    dplyr::group_by(.data$testkit_id) %>%
    dplyr::filter(dplyr::n() > 1)

  output_list = list(count_tbl = count_tbl,
                     repeat_samples = repeat_samples)

  return(output_list)
}
