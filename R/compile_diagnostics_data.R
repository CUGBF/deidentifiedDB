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
#' @examples
compile_new_diagnostics <- function(filepath, run_date_fmt = c("mdy")) {
  new_diagnostics <- read_diagnostics_csv(filepath) %>%
    polish_diagnostics()

  stopifnot(nrow(new_diagnostics) != 0)

  return(new_diagnostics)
}
