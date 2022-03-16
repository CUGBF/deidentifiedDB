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

  out_tbl <- out_tbl %>%
    dplyr::arrange(
      .data$zip_code,
      .data$state,
      .data$city
    )

  storage_tbl <- out_tbl %>%
    dplyr::select(c(
      "testkit_id",
      "zip_code",
      "state",
      "city"
    ))

  location_combinations <- location_combinations %>%
    dplyr::arrange(
      .data$zip_code,
      .data$state,
      .data$city
    ) %>%
    dplyr::mutate(
      zip_code = replace(
        .data$zip_code,
        is.na(.data$zip_code),
        00000
      ),
      state = replace(
        .data$state,
        is.na(.data$state),
        "NA"
      ),
      city = replace(
        .data$city,
        is.na(.data$city),
        "NA"
      ),
    )

  out_tbl <- out_tbl %>%
    dplyr::mutate(
      zip_code = replace(
        .data$zip_code,
        is.na(.data$zip_code),
        00000
      ),
      state = replace(
        .data$state,
        is.na(.data$state),
        "NA"
      ),
      city = replace(
        .data$city,
        is.na(.data$city),
        "NA"
      ),
    )


  for (n in seq_along(location_combinations$zip_code)) {
    loc_com_row <- location_combinations[n, ]

    current_zip_code <- loc_com_row %>%
      dplyr::pull(.data$zip_code)

    current_city <- loc_com_row %>%
      dplyr::pull(.data$city)

    current_state <- loc_com_row %>%
      dplyr::pull(.data$state)

    rows_to_change <- which((out_tbl$zip_code == current_zip_code) &
      (out_tbl$city == current_city) &
      (out_tbl$state == current_state))

    if (sum(rows_to_change) == 0) {
      next
    }

    out_tbl[rows_to_change, "zip_code_usps"] <- loc_com_row %>%
      dplyr::pull(.data$zip_code_usps)

    out_tbl[rows_to_change, "city_usps"] <- loc_com_row %>%
      dplyr::pull(.data$city_usps)

    out_tbl[rows_to_change, "county_usps"] <- loc_com_row %>%
      dplyr::pull(.data$county_usps)

    out_tbl[rows_to_change, "state_usps"] <- loc_com_row %>%
      dplyr::pull(.data$state_usps)

    out_tbl[rows_to_change, "country_usps"] <- loc_com_row %>%
      dplyr::pull(.data$country_usps)

    rm(list = c("loc_com_row"))
  }

  out_tbl <- out_tbl %>%
    dplyr::select(-c(
      "zip_code",
      "state",
      "city"
    )) %>%
    dplyr::left_join(storage_tbl,
      by = "testkit_id"
    ) %>%
    dplyr::group_by(.data$testkit_id) %>%
    dplyr::arrange(.data$city) %>%
    dplyr::slice_head() %>%
    dplyr::ungroup()

  return(out_tbl)
}
