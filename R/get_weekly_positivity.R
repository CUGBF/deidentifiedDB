#' Get Weekly Positivity Rate
#'
#' @param weekly_positivity_tbl Tibble containing the following columns 1)start_date,
#' 2) end_date,
#' 3) positivity_rate
#' @param start_date Filter sample collection table for samples collected after
#' this date. Provide date as in "2021-08-01" for August 1, 2021
#' @param end_date Filter sample collection table for samples collected on or
#' before this date. Provide date as in "2021-08-01" for August 1, 2021
#' @param ... Names of columns to group_by
#'
#' @return Tibble with weekly positivity
#' @export
#'
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
get_weekly_positivity <- function(diagnostics_tbl,
                                  start_date,
                                  end_date,
                                  ...) {
  stopifnot(all(c(
    "testkit_id", "run_date", "result",
    "control"
  ) %in% colnames(diagnostics_tbl)))

  daily_diagnostics_counts <- deidentifiedDB::get_daily_diagnostics(
    diagnostics_table,
    start_date = start_date,
    end_date = end_date,
    run_date,
    result
  )

  positive <- 0
  negative <- 0

  start_date_var <- FALSE
  table_var <- FALSE

  start_date <- 0
  output_tbl <- 0

  count_tbl <- daily_diagnostics_counts[['count_tbl']]

  setDT(count_tbl)

  for (x in 1:nrow(count_tbl)) {
    if (start_date_var == FALSE) {
       start_date = count_tbl[x]$run_date
       start_date_var = TRUE
    }

    if (!is.na(count_tbl[x]$result)) {
      if (count_tbl[x]$result == "positive") {
        positive <- positive + count_tbl[x]$count
      }

      if (count_tbl[x]$result == "negative") {
        negative <- negative + count_tbl[x]$count
      }
    }

    if (x == nrow(count_tbl) || 
      (count_tbl[x]$run_date != start_date && wday(count_tbl[x + 1]$run_date) == 2) ||    
      (as.period(interval(start_date,count_tbl[x + 1]$run_date)) %>% day()) > (8 - wday(start_date)) ||
      (wday(start_date) == 1 && (as.period(interval(start_date,count_tbl[x + 1]$run_date)) %>% day()) > 1)) {
      
      positivity_rate <- round((100 * positive / (positive + negative)), digits = 3)
      if (table_var == FALSE) {
        output_tbl <- data.frame(start_date, count_tbl[x]$run_date, positivity_rate)
        names(output_tbl) <- c("start_date", "end_date", "positivity_rate")
        table_var = TRUE
      }
      else {
        temp <- data.frame(start_date, count_tbl[x]$run_date, positivity_rate)
        names(temp) <- c("start_date", "end_date", "positivity_rate")
        output_tbl <- rbind(output_tbl, temp)
      }

      positive <- 0
      negative <- 0
      start_date_var <- FALSE
    }

  return(output_tbl)
}
