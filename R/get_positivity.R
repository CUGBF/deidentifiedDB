#' Compute Weekly Test Positivity Rate (TPR)
#'
#' @param sc_tbl Sample Collection table from deidentifiedDB database
#' @param start_date Filter sample collection table for samples collected after
#' this date. Provide date as in "2021-08-01" for August 1, 2021
#' @param end_date Filter sample collection table for samples collected on or
#' before this date. Provide date as in "2021-08-01" for August 1, 2021
#' @param n_days Duration of time (in days) for which only the first collected
#' COVID-19 positive sample  for a patient is retained
#'
#' @return Tibble containing Weekly Test Positivity Rate (TPR) data after
#' excluding redundant COVID-19 positive samples
#' @export
#'
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
get_positivity <- function(sc_tbl,
                           start_date = "2021-01-01",
                           end_date = lubridate::date(lubridate::now()),
                           n_days = 30) {
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
    n_days = n_days
  )

  week_dates_info <- week_dates_info(sc_tbl_no_missing)

  stopifnot(all(c(
    "collection_week",
    "week_start",
    "week_end"
  ) %in% colnames(week_dates_info)))

  output_tbl <- sc_tbl_no_missing %>%
    dplyr::filter(.data$order_priority %in% c(
      "SYMPTOMATIC",
      "EXPOSED",
      "SURVEILLANCE"
    )) %>%
    dplyr::group_by(
      .data$collection_week,
      .data$population,
      .data$order_priority,
      .data$rymedi_result
    ) %>%
    dplyr::summarize(count = dplyr::n()) %>%
    dplyr::mutate(
      count = replace(
        .data$count, .data$rymedi_result == "NEGATIVE" &
          .data$count == 0,
        1
      ),
      order_priority = factor(.data$order_priority,
        levels = c(
          "SYMPTOMATIC",
          "EXPOSED",
          "SURVEILLANCE"
        )
      )
    ) %>%
    tidyr::pivot_wider(
      names_from = .data$rymedi_result,
      values_from = .data$count
    ) %>%
    dplyr::mutate(
      POSITIVE = replace(.data$POSITIVE, is.na(.data$POSITIVE), 0),
      NEGATIVE = replace(.data$NEGATIVE, is.na(.data$NEGATIVE), 0),
      TOTAL = .data$NEGATIVE + .data$POSITIVE,
      POSITIVITY = round((.data$POSITIVE / .data$TOTAL) * 100, 2),
      collection_week = as.factor(.data$collection_week)
    ) %>%
    dplyr::full_join(week_dates_info,
      by = "collection_week"
    ) %>%
    dplyr::relocate(
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
      .data$collection_week,
      .data$population,
      .data$order_priority
    )

  stopifnot(nrow(output_tbl) > 0)

  return(output_tbl)
}
