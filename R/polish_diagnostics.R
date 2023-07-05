#' Reorganize Diagnostics data
#'
#' @param diagnostics_tbl Diagnostics data in tibble format
#' @param run_date_fmt Date format used to specify run date
#' (for example, 08-01-2020 for August 1, 2020)
#'
#' @return A (better) reorganized diagnostics_tbl
#'
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
    "plate"
  ) %in% colnames(diagnostics_tbl)))

  if (grepl("-", diagnostics_tbl$run_date[1])) {  
    output_tbl <- diagnostics_tbl %>%
      dplyr::mutate(
        run_date = lubridate::date("run_date"),
        control = detect_control(testkit_id, "result"),
        dplyr::across(tidyselect::vars_select_helpers$where(is.character), stringr::str_trim),
        dplyr::across(c(
          "ct_rnasep_rep1",
          "ct_rnasep_rep2",
          "ct_N_rep1",
          "ct_N_rep2"
        ), tidy_up_ct),
        dplyr::across(c(
          "result",
          "plate"
        ), stringr::str_to_lower)
      ) %>%
      dplyr::arrange("run_date")
  } else {
    output_tbl <- diagnostics_tbl %>%
      dplyr::mutate(
        run_date = lubridate::date(lubridate::parse_date_time("run_date", orders = run_date_fmt)),
        control = detect_control("testkit_id", "result"),
        dplyr::across(tidyselect::vars_select_helpers$where(is.character), stringr::str_trim),
        dplyr::across(c(
          "ct_rnasep_rep1",
          "ct_rnasep_rep2",
          "ct_N_rep1",
          "ct_N_rep2"
        ), tidy_up_ct),
        dplyr::across(c(
          "result",
          "plate"
        ), stringr::str_to_lower)
      ) %>%
      dplyr::arrange("run_date")
  }


  return(output_tbl)
}
