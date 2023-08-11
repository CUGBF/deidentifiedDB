#' Compute Weekly Test Positivity Rate (TPR)
#'
#' @param sc_tbl Sample Collection table from deidentifiedDB database
#' @param start_date Filter sample collection table for samples collected after
#' this date. Provide date as in "2021-08-01" for August 1, 2021
#' @param end_date Filter sample collection table for samples collected on or
#' before this date. Provide date as in "2021-08-01" for August 1, 2021
#' @param n_days Duration of time (in days) for which only the first collected
#' COVID-19 positive sample  for a patient is retained
#' @param time_zone Time zone for collection time (Default: "America/New_York")
#'
#' @return Tibble containing Weekly Test Positivity Rate (TPR) data after
#' excluding redundant COVID-19 positive samples
#' @export
#'
#' @importFrom magrittr "%>%"

get_positivity <- function(sc_tbl,
                           start_date = "2021-01-01",
                           end_date = as.character(
                             lubridate::date(lubridate::now())
                           ),
                           n_days = 30,
                           time_zone = "America/New_York") {
  stopifnot(all(c(
    "testkit_id",
    "rymedi_result",
    "collection_date",
    "population",
    "order_priority",
    "gender",
    "patient_id"
  ) %in% colnames(sc_tbl)))

  sc_tbl_no_missing <- get_sc_wo_redundant(sc_tbl,
    start_date = start_date,
    end_date = end_date,
    n_days = n_days,
    time_zone = time_zone
  )

  week_dates_info <- week_dates_info(sc_tbl_no_missing,
    time_zone = time_zone
  )

  stopifnot(all(c(
    "collection_year",
    "collection_week",
    "week_start",
    "week_end"
  ) %in% colnames(week_dates_info)))

  output_tbl <- sc_tbl_no_missing %>%
    dplyr::filter(order_priority %in% c(
      "SYMPTOMATIC",
      "EXPOSED",
      "SURVEILLANCE"
    )) %>%
    dplyr::group_by(
      collection_year,
      collection_week,
      population,
      order_priority,
      rymedi_result
    ) %>%
    dplyr::summarize(count = dplyr::n()) %>%
    dplyr::mutate(
      count = replace(
        count, rymedi_result == "NEGATIVE" &
          count == 0,
        1
      ),
      order_priority = factor(order_priority,
        levels = c(
          "SYMPTOMATIC",
          "EXPOSED",
          "SURVEILLANCE"
        )
      )
    ) %>%
    tidyr::pivot_wider(
      names_from = rymedi_result,
      values_from = count
    ) %>%
    dplyr::mutate(
      POSITIVE = replace(POSITIVE, is.na(POSITIVE), 0),
      NEGATIVE = replace(NEGATIVE, is.na(NEGATIVE), 0),
      TOTAL = NEGATIVE + POSITIVE,
      POSITIVITY = round((POSITIVE / TOTAL) * 100, 2),
      collection_week = as.factor(collection_week)
    ) %>%
    dplyr::left_join(week_dates_info,
      by = c(
        "collection_year",
        "collection_week"
      )
    ) %>%
    dplyr::relocate(
      "collection_year",
      "collection_week",
      "week_start",
      "week_end",
      "order_priority",
      "TOTAL",
      "POSITIVE",
      "NEGATIVE",
      "POSITIVITY"
    ) %>%
    dplyr::arrange(
      collection_year,
      collection_week,
      population,
      order_priority
    )

  stopifnot(nrow(output_tbl) > 0)

  return(output_tbl)
}
