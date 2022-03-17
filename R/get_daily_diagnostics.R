#' Get Daily Diangostics Counts
#'
#' @param diagnostics_tbl Tibble containing the following columns 1)testkit_id,
#' 2) run_Date
#' 3)result
#' @param start_date Filter sample collection table for samples collected after
#' this date. Provide date as in "2021-08-01" for August 1, 2021
#' @param end_date Filter sample collection table for samples collected on or
#' before this date. Provide date as in "2021-08-01" for August 1, 2021
#' @param add_variables variables to group_by
#'
#' @return Tibble with daily counts of different diagnostics result categories
#' @export
#'
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
get_daily_diagnostics <- function(diagnostics_tbl,
                                  start_date,
                                  end_date,
                                  add_variables = NULL) {
  stopifnot(all(c("testkit_id", "run_date", "result") %in% colnames(diagnostics_tbl)))

  if (!(lubridate::is.Date(diagnostics_tbl$run_date))) {
    diagnostics_tbl <- diagnostics_tbl %>%
      dplyr::mutate(run_date = lubridate::as_date(.data$run_date))
  }

  start_date <- lubridate::as_date(start_date)
  end_date <- lubridate::as_date(end_date)

  diagnostics_tbl <- diagnostics_tbl %>%
    dplyr::filter(
      .data$run_date >= start_date,
      .data$run_date <= end_date
    )

  grouping_variable <- c(
    "run_date",
    "result"
  )

  if (!is.null(add_variables)) {
    grouping_variable <- c(
      grouping_variable,
      add_variables
    )
  }

  count_tbl <- diagnostics_tbl %>%
    dplyr::group_by(grouping_variable) %>%
    dplyr::summarise(count = dplyr::n()) %>%
    dplyr::ungroup()

  repeat_samples <- diagnostics_tbl %>%
    dplyr::group_by(.data$testkit_id) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::ungroup()

  output_list <- list(
    count_tbl = count_tbl,
    repeat_samples = repeat_samples
  )

  return(output_list)
}
