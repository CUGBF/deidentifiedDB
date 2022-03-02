#' Reorganize Diagnostics data
#'
#' @param diagnostics_tbl Diagnostics data in tibble format
#' @param run_date_fmt Date format used to specify run date
#' (for example, 08-01-2020 for August 1, 2020)
#'
#' @return A (better) reorganized diagnostics_tbl
#'
#' @examples
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
    mutate(
      run_date = date(parse_date_time(run_date, orders = run_date_fmt)),
      control = detect_control(testkit_id, result),
      across(where(is.character), str_trim),
      across(ct_rnasep_rep1:ct_N_rep2, tidy_up_ct),
      result = str_to_lower(result),
      machine = str_to_lower(machine),
      thermocycler = str_to_lower(thermocycler),
      pcr_type = str_to_lower(pcr_type),
      plate = as.numeric(plate)
    ) %>%
    arrange(run_date)

  return(output_tbl)
}
