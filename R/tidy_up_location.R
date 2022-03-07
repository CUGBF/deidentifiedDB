#' Tidy Up Location Information
#'
#' @param sc_tbl Tibble containing sample collection data
#' @param location_combinations Tibble with validated USPS location information
#' from lookup_zip_code()
#'
#' @return Tidy Tibble with Location Information
#' @export
#'
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
tidy_up_location <- function(sc_tbl,
                             location_combinations) {
  stopifnot(all(c(
    "zip_code",
    "state",
    "city"
  ) %in% colnames(sc_tbl)) &
    all(c(
      "zip_code",
      "state",
      "city",
      "zip_code_usps",
      "city_usps",
      "county_usps",
      "state_usps",
      "country_usps"
    ) %in% colnames(location_combinations)))

  out_tbl <- sc_tbl
  out_tbl[["zip_code_usps"]] <- NA_integer_
  out_tbl[["city_usps"]] <- NA_character_
  out_tbl[["county_usps"]] <- NA_character_
  out_tbl[["state_usps"]] <- NA_character_
  out_tbl[["country_usps"]] <- NA_character_

  for (n in seq_along(location_combinations$zip_code)) {
    loc_com_row <- location_combinations[n, ]
    rows_to_change <- which((out_tbl$zip_code == loc_com_row[["zip_code"]]) &
                              (out_tbl$city == loc_com_row[["city"]]) &
                              (out_tbl$state == loc_com_row[["state"]]))
    out_tbl[rows_to_change, "zip_code_usps"] <- loc_com_row[["zip_code_usps"]]
    out_tbl[rows_to_change, "city_usps"] <- loc_com_row[["city_usps"]]
    out_tbl[rows_to_change, "county_usps"] <- loc_com_row[["county_usps"]]
    out_tbl[rows_to_change, "state_usps"] <- loc_com_row[["state_usps"]]
    out_tbl[rows_to_change, "country_usps"] <- loc_com_row[["country_usps"]]
  }

  return(out_tbl)
}
