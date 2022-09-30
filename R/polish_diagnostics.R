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
    "machine",
    "thermocycler",
    "pcr_type",
    "plate"
  ) %in% colnames(diagnostics_tbl)))

@@ -35,10 +32,7 @@ polish_diagnostics <- function(diagnostics_tbl, run_date_fmt = c("mdy")) {
          .data$ct_N_rep2
        ), tidy_up_ct),
        dplyr::across(c(
          .data$result,
          .data$machine,
          .data$thermocycler,
          .data$pcr_type
        ), stringr::str_to_lower),
        plate = as.numeric(.data$plate)
      ) %>%
@@ -56,10 +50,7 @@ polish_diagnostics <- function(diagnostics_tbl, run_date_fmt = c("mdy")) {
          .data$ct_N_rep2
        ), tidy_up_ct),
        dplyr::across(c(
          .data$result,
          .data$machine,
          .data$thermocycler,
          .data$pcr_type
        ), stringr::str_to_lower),
        plate = as.numeric(.data$plate)
      ) %>%
      dplyr::arrange(.data$run_date)
  }
  return(output_tbl)
}
