#' Get COVID-19 positive samples after excluding redundant samples in a
#' probable single infection event
#'
#' @param sc_tbl Sample Collection table from deidentifiedDB database
#' @param start_date Filter sample collection table for samples collected on or
#' after this date. Provide date as in "2021-08-01" for August 1, 2021
#' @param end_date Filter sample collection table for samples collected on or
#' before this date. Provide date as in "2021-08-01" for August 1, 2021
#' @param n_days Duration of time (in days) for which only the first collected
#' COVID-19 positive sample  for a patient is retained
#'
#' @return Tibble containing sample collection data after excluding redundant
#' COVID-19 positive samples
#' @export
#'
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
get_sc_wo_redundant <- function(sc_tbl,
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
    "patient_id",
    "performing_facility"
  ) %in% colnames(sc_tbl)))

  sc_tbl_no_missing <- sc_tbl %>%
    dplyr::mutate(
      collection_date = lubridate::date(lubridate::as_datetime(.data$collection_date)),
      collection_week = lubridate::week(collection_date),
      collection_month = zoo::as.yearmon(collection_date),
      dplyr::across(
        c(
          .data$rymedi_result,
          .data$population,
          .data$order_priority,
          .data$gender,
          .data$performing_facility
        ),
        stringr::str_to_upper
      )
    ) %>%
    dplyr::filter(
      !(is.na(.data$collection_date) | is.na(.data$patient_id)),
      .data$collection_date >= start_date,
      .data$collection_date <= end_date,
      stringr::str_detect(
        .data$performing_facility,
        "CLEMSON"
      ),
      .data$rymedi_result %in% c("POSITIVE", "NEGATIVE")
    ) %>%
    dplyr::select(
      "testkit_id",
      "rymedi_result",
      "collection_week",
      "collection_month",
      "collection_date",
      "population",
      "order_priority",
      "gender",
      "patient_id",
      "performing_facility"
    ) %>%
    dplyr::arrange(
      .data$collection_date,
      .data$rymedi_result,
      .data$population,
      .data$order_priority,
      .data$performing_facility
    )


  ## Only retain the first COVID19 +ve sample from a patient in a period of n_days days
  ## Number of such multi-test possibly-single-infection samples :

  testkit_ids_to_exclude <- get_redundant_samples(
    sc_tbl_no_missing,
    n_days
  )

  output_tbl <- sc_tbl_no_missing %>%
    dplyr::filter(!(.data$testkit_id %in% testkit_ids_to_exclude))

  return(output_tbl)
}
