#' Get Testkit IDs of redundant COVID-19 positive samples {non-first (based on
#' collection date) COVID19 samples from probably a single-infection event}
#'
#' @param sc_tbl_no_missing Sample collection tibble without missing patient ID,
#' Rymedi results, and collection date data
#' @param n_days Duration of time (in days) for which only the first collected
#' COVID-19 positive sample  for a patient is retained
#'
#' @return Vector containing Testkit IDs of redundant samples
#'
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
get_redundant_samples <- function(sc_tbl_no_missing,
                                  n_days = 30) {
  stopifnot(all(c(
    "testkit_id",
    "rymedi_result",
    "collection_date",
    "patient_id"
  ) %in% colnames(sc_tbl_no_missing)))

  stopifnot(all(!is.na(sc_tbl_no_missing$patient_id)))
  stopifnot(all(!is.na(sc_tbl_no_missing$rymedi_result)))
  stopifnot(all(!is.na(sc_tbl_no_missing$collection_date)))
  stopifnot(all(!is.na(sc_tbl_no_missing$testkit_id)))


  positive_samples <- sc_tbl_no_missing %>%
    dplyr::filter(stringr::str_to_upper(.data$rymedi_result) == "POSITIVE")

  ## Only retain the first COVID19 +ve sample from a patient in a period of n_days days
  ## Number of such multi-test possibly-single-infection samples :

  multi_positive_patients <- positive_samples %>%
    dplyr::group_by(.data$patient_id) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::arrange(
      .data$patient_id,
      .data$collection_date
    ) %>%
    dplyr::group_split()

  output_vec <- c()

  for (n in seq_along(multi_positive_patients)) {
    temp_tbl <- multi_positive_patients[[n]] %>%
      dplyr::arrange(.data$collection_date)
    diff_days <- diff(temp_tbl[["collection_date"]])

    for (m in seq_along(diff_days)) {
      if (diff_days[m] <= n_days) {
        testkitid_to_remove <- temp_tbl$testkit_id[m + 1]
        output_vec <- c(
          output_vec,
          testkitid_to_remove
        )
      }
    }
  }

  return(output_vec)
}
