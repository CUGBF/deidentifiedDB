#' Tibble with Collection Week Info
#'
#' @param sc_tbl_no_missing tibble containing sample collection information
#' no patient ID or collection date missing
#' @param time_zone Time zone for collection time (Default: "America/New_York")
#'
#' @return Tibble with Collection Week Number, Start and End dates
#'
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
week_dates_info <- function(sc_tbl_no_missing,
                            time_zone = "America/New_York") {
  stopifnot(
    "collection_date"
    %in% colnames(sc_tbl_no_missing)
  )

  if (!(all(lubridate::is.Date(sc_tbl_no_missing$collection_date)) ||
    all(lubridate::is.timepoint(sc_tbl_no_missing$collection_date)))) {
    sc_tbl_no_missing <- sc_tbl_no_missing %>%
      dplyr::mutate(collection_date = lubridate::date(
        lubridate::as_datetime(.data$collection_date,
          tz = time_zone
        )
      ))
  }

  if (!("collection_week" %in% colnames(sc_tbl_no_missing))) {
    sc_tbl_no_missing <- sc_tbl_no_missing %>%
      dplyr::mutate(collection_week = lubridate::week(.data$collection_date))
  }

  output_tbl <- sc_tbl_no_missing %>%
    dplyr::select("collection_week", "collection_date") %>%
    dplyr::distinct() %>%
    dplyr::group_by(.data$collection_week) %>%
    dplyr::mutate(
      week_start = min(.data$collection_date),
      week_end = max(.data$collection_date),
      collection_week = as.factor(.data$collection_week)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-c(.data$collection_date)) %>%
    dplyr::distinct() %>%
    dplyr::arrange(.data$collection_week)

  return(output_tbl)
}
