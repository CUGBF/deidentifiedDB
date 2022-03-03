#' Reorganize Diagnostics data
#'
#' @param diagnostics_tbl Diagnostics data in tibble format
#' @param run_date_fmt Date format used to specify run date
#' (for example, 08-01-2020 for August 1, 2020)
#'
#' @return A (better) reorganized diagnostics_tbl
#'
#' @examples
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
polish_diagnostics <- function(diagnostics_tbl, run_date_fmt = c("mdy")) {
  stopifnot(all(c(
    "run_date",
    "ct_rnasep_rep1",
    "ct_rnasep_rep2",
    "ct_N_rep1",
    "ct_N_rep2",
    "result",
    "machine",
    "thermocycler",
    "pcr_type",
    "plate"
  ) %in% colnames(diagnostics_tbl)))

  output_tbl <- diagnostics_tbl %>%
    dplyr::mutate(
      run_date = lubridate::date(lubridate::parse_date_time(.data$run_date, orders = run_date_fmt)),
      control = detect_control(.data$testkit_id, .data$result),
      dplyr::across(tidyselect::vars_select_helpers$where(is.character), stringr::str_trim),
      dplyr::across(c(.data$ct_rnasep_rep1,
                      .data$ct_rnasep_rep2,
                      .data$ct_N_rep1,
                      .data$ct_N_rep2), tidy_up_ct),
      dplyr::across(c(.data$result,
                      .data$machine,
                      .data$thermocycler,
                      .data$pcr_type), stringr::str_to_lower),
      plate = as.numeric(.data$plate)
    ) %>%
    dplyr::arrange(.data$run_date)

  return(output_tbl)
}
