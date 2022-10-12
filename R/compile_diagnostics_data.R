#' Compile weekly diagnostics data from REDDI before appending to the
#' deidentifiedDB SQLite database
#'
#' @param filepath filepath to the diagnostics weekly data from REDDI
#' @param run_date_fmt Date format used to specify run date
#' (for example, 08-01-2020 for August 1, 2020)
#'
#' @return Tibble with diagnostics data from REDDI
#' @export
#'
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
compile_diagnostics_data <- function(filepath, run_date_fmt = c("mdy")) {
  diagnostics_tbl <- read_diagnostics_csv(filepath) %>%
    polish_diagnostics()

  stopifnot(nrow(diagnostics_tbl) != 0)

  stopifnot(all(c(
    "testkit_id",
    "hashed_id",
    "run_date",
    "plate",
    "result",
    "control"
  ) %in% colnames(diagnostics_tbl)))

  diagnostics_tbl <- diagnostics_tbl %>%
    dplyr::select(
      "testkit_id",
      "hashed_id",
      "run_date",
      "plate",
      "result",
      "control"
    )

  return(diagnostics_tbl)
}
