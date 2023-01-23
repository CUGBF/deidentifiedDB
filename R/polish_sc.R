#' Polish Sample Collection Tibble
#'
#' @param sc_tbl Tibble containing sample collection data
#' after tidy_up_location()
#'
#' @return Final tibble containing sample collection data ready to be appended
#' to deidentifiedDB SQLite database
#' @export
#'
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
polish_sc <- function(sc_tbl) {
  stopifnot(all(c(
    "testkit_id", "rymedi_result",
    "population", "order_priority",
    "collection_date", "result_date",
    "gender", "pregnancy_status",
    "zip_code_usps", "city_usps",
    "county_usps", "state_usps",
    "country_usps", "zip_code",
    "city", "state",
    "patient_id",
    "teskit_sku"
  ) %in% colnames(sc_tbl)))

  stopifnot(!(all(c("G7-PCR-SALIVA", "PCR- ANTERIOR-001") %in% unique(sc_tbl$population))))
  stopifnot(unique(sc_tbl$gender[!is.na(sc_tbl$gender)]) %in% c("M", "F"))
  stopifnot("PICKENS COUNTY" %in% unique(sc_tbl$county_usps))

  out_tbl <- sc_tbl %>%
    dplyr::select(
      "testkit_id",
      "rymedi_result",
      "population",
      "order_priority",
      "collection_date",
      "result_date",
      "gender",
      "pregnancy_status",
      "zip_code_usps",
      "city_usps",
      "county_usps",
      "state_usps",
      "country_usps",
      "zip_code",
      "city",
      "state",
      "patient_id",
      "teskit_sku"
    ) %>%
    dplyr::rename(
      zip_code_user_input = "zip_code",
      city_user_input = "city",
      state_user_input = "state",
      zip_code = "zip_code_usps",
      city = "city_usps",
      county = "county_usps",
      state = "state_usps",
      country = "country_usps"
    ) %>%
    dplyr::arrange(.data$collection_date) %>%
    dplyr::distinct()

  test_tbl <- out_tbl %>%
    dplyr::group_by(.data$testkit_id) %>%
    dplyr::filter(dplyr::n() > 1)

  stopifnot(nrow(test_tbl) == 0)

  return(out_tbl)
}
