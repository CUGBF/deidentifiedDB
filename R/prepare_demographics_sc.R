#' Transform COVID19 Sample Collection and deidentified Demographics Data
#' from REDDI for the deidentifiedDB demographics and sample_collection tables
#'
#' @param filepath Path to the CSV file containing Sample Collection and
#' deidentified Demographics Data
#' @param date_fmt Format used to specify dates (Default: MM/DD/YYYY)
#' @param time_zone Time zone for collection time (Default: "America/New_York")
#'
#' @return Tibble with the Sample Collection and
#' the deidentified Demographics Data organized to make it convenient to update
#' deidentified DB database
#' @export
#'
#' @examples
#' @importFrom magrittr "%>%"
prepare_demographics_sc <- function(filepath,
                                    date_fmt = c("%m/%d/%y"),
                                    time_zone = "America/New_York") {
  output_tbl <- read_demographics_csv(filepath) %>%
    do_prelim_tidy_sc(
      date_fmt = date_fmt,
      time_zone = time_zone
    )

  return(output_tbl)
}
