#' Tibble with Collection Week Info
#'
#' @param sc_tbl_no_missing tibble containing sample collection information
#' no patient ID or collection date missing
#'
#' @return Tibble with Collection Week Number, Start and End dates
#'
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
week_dates_info <- function(sc_tbl_no_missing) {
  stopifnot(
    "collection_date"
    %in% colnames(sc_tbl_no_missing)
  )

  if (!("collection_week" %in% colnames(sc_tbl_no_missing))) {
    sc_tbl_no_missing <- sc_tbl_no_missing %>%
      dplyr::mutate(collection_week = lubridate::week(collection_date))
  }

  output_tbl <- sc_tbl_no_missing %>%
    dplyr::select(collection_week, collection_date) %>%
    dplyr::distinct() %>%
    dplyr::group_by(collection_week) %>%
    dplyr::mutate(
      week_start = min(collection_date),
      week_end = max(collection_date),
      collection_week = as.factor(collection_week)
    ) %>%
    dplyr::select(-c(collection_date)) %>%
    dplyr::distinct()

  return(output_tbl)
}
