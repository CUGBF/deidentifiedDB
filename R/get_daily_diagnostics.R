#' Get Daily Diagnostics Counts
#'
#' @param diagnostics_tbl Tibble containing the following columns 1)testkit_id,
#' 2) run_Date
#' 3)result
#' @param start_date Filter sample collection table for samples collected after
#' this date. Provide date as in "2021-08-01" for August 1, 2021
#' @param end_date Filter sample collection table for samples collected on or
#' before this date. Provide date as in "2021-08-01" for August 1, 2021
#' @param ... Names of columns to group_by
#'
#' @return Tibble with daily counts of different diagnostics result categories
#' @export
#'
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
get_daily_diagnostics <- function(diagnostics_tbl,
                                  start_date,
                                  end_date,
                                  ...) {
  stopifnot(all(c("testkit_id", "run_date", "result",
                  "control") %in% colnames(diagnostics_tbl)))


  if (!(lubridate::is.Date(diagnostics_tbl$run_date))) {
    diagnostics_tbl <- diagnostics_tbl %>%
      dplyr::mutate(run_date = lubridate::as_date(.data$run_date))
  }

  start_date <- lubridate::as_date(start_date)
  end_date <- lubridate::as_date(end_date)

  diagnostics_tbl <- diagnostics_tbl %>%
    dplyr::mutate(control = as.logical(control)) %>%
    dplyr::filter(
      !.data$control,
      .data$run_date >= start_date,
      .data$run_date <= end_date
    )

  # https://stackoverflow.com/questions/57704792/is-it-possible-to-pass-multible-variables-to-the-same-curly-curly

  count_tbl <- diagnostics_tbl %>%
    dplyr::group_by(!!!dplyr::ensyms(...)) %>%
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
