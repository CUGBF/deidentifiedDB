#' Compile Reference Location Information
#'
#' @param location_tbl Tibble with "zip_code", "state", and "city" columns
#' @param us_zip_codes_filepath Path to the csv file containing US zip codes
#' (downloaded from UnitedStatesZipCodes.org)
#' @param global_regions_filepath Path to the csv file of the UN Dataset downloaded
#' from https://unece.org/trade/cefact/UNLOCODE-Download)
#'
#' @return Tibble with validated USPS location information
#'
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
lookup_zip_code <- function(location_tbl,
                            us_zip_codes_filepath,
                            global_regions_filepath) {
  stopifnot(all(c(
    "zip_code",
    "state",
    "city"
  ) %in% colnames(location_tbl)))

  us_zip_codes <- build_us_zip(us_zip_codes_filepath)
  us_geo_entities <- get_us_entities(us_zip_codes_filepath)
  global_regions_vec <- get_global_entities(global_regions_filepath)

  location_memory <- tibble::tibble(
    zip_code = character(),
    state = character(),
    city = character(),
    zip_code_usps = integer(),
    city_usps = character(),
    county_usps = character(),
    state_usps = character(),
    country_usps = character()
  )

  location_tbl[["zip_code_usps"]] <- NA_integer_
  location_tbl[["city_usps"]] <- NA_character_
  location_tbl[["county_usps"]] <- NA_character_
  location_tbl[["state_usps"]] <- NA_character_
  location_tbl[["country_usps"]] <- NA_character_

  for (n in seq_along(location_tbl$zip_code)) {
    location_combination <- location_tbl[n, c("zip_code", "state", "city")]


    temp_location_info <- location_memory %>%
      dplyr::filter(
        .data$zip_code == location_combination[["zip_code"]],
        .data$state == location_combination[["state"]],
        .data$city == location_combination[["city"]]
      )

    if (nrow(temp_location_info) == 1) {
      in_zip <- as.integer(temp_location_info[["zip_code_usps"]])
      in_state <- temp_location_info[["state_usps"]]
      in_county <- temp_location_info[["county_usps"]]
      in_city <- temp_location_info[["city_usps"]]
      in_country <- temp_location_info[["country_usps"]]
    } else {
      # cities in the state of the current record
      cities_in_state <- us_zip_codes %>%
        dplyr::filter(.data$state == location_combination[["state"]]) %>%
        dplyr::pull(.data$primary_city)

      # Is the city in current record within acceptable city entries for the state ?
      acceptable_city_status <- sum(stringr::str_detect(
        (us_zip_codes %>%
          dplyr::filter(.data$state == location_combination[["state"]]) %>%
          dplyr::pull(.data$acceptable_cities)),
        location_combination[["city"]]
      ), na.rm = TRUE) > 0

      if (location_combination["state"] %in% us_geo_entities) {
        cleaned_up_zip <- tidy_up_zip(location_combination[["zip_code"]])
        if (!is.na(cleaned_up_zip) && cleaned_up_zip %in% us_zip_codes$zip) {
          in_zip <- cleaned_up_zip
          zip_df <- us_zip_codes %>%
            dplyr::filter(.data$zip == cleaned_up_zip) %>%
            dplyr::slice_head()
        } else if (!is.na(location_combination[["city"]]) && !is.na(location_combination[["state"]]) &&
          (location_combination["city"] %in% (cities_in_state))) {
          zip_df <- us_zip_codes %>%
            dplyr::filter(
              .data$state == location_combination[["state"]],
              .data$primary_city == location_combination[["city"]]
            ) %>%
            dplyr::arrange(.data$zip) %>%
            dplyr::slice_head()
          in_zip <- zip_df$zip
        } else if (!is.na(location_combination[["city"]]) && !is.na(location_combination[["state"]]) &&
          (acceptable_city_status)) {
          zip_df <- us_zip_codes %>%
            dplyr::filter(
              .data$state == location_combination[["state"]],
              stringr::str_detect(.data$acceptable_cities, location_combination[["city"]])
            ) %>%
            dplyr::arrange(.data$zip) %>%
            dplyr::slice_head()
          in_zip <- zip_df$zip
        } else {
          zip_df <- tibble::tibble(
            zip = NA_integer_,
            state = location_combination[["state"]],
            county = NA_character_,
            primary_city = location_combination[["city"]],
            country = "US"
          )
          in_zip <- zip_df$zip
        }
        in_state <- zip_df$state
        in_county <- zip_df$county
        in_city <- zip_df$primary_city
        in_country <- zip_df$country
      } else if (!(is.na(location_combination[["state"]]) || (location_combination[["state"]] %in% us_geo_entities))) {
        if (location_combination[["state"]] %in% global_regions_vec) {
          in_country <- names(which(global_regions_vec == location_combination[["state"]]))[1]
        } else {
          in_country <- NA_character_
        }
        in_zip <- NA_integer_
        in_state <- location_combination[["state"]]
        in_county <- NA_character_
        in_city <- location_combination[["city"]]
      } else {
        cleaned_up_zip <- tidy_up_zip(location_combination[["zip_code"]])
        if (!is.na(cleaned_up_zip) && cleaned_up_zip %in% us_zip_codes$zip) {
          in_zip <- cleaned_up_zip
          zip_df <- us_zip_codes %>%
            dplyr::filter(.data$zip == cleaned_up_zip) %>%
            dplyr::slice_head()
          in_state <- zip_df$state
          in_county <- zip_df$county
          in_city <- zip_df$primary_city
          in_country <- zip_df$country
        } else {
          in_zip <- NA_integer_
          in_state <- NA_character_
          in_county <- NA_character_
          in_city <- NA_character_
          in_country <- NA_character_
        }
      }
      location_memory_row <- tibble::tibble(
        zip_code = location_combination[["zip_code"]],
        state = location_combination[["state"]],
        city = location_combination[["city"]],
        zip_code_usps = as.integer(in_zip),
        city_usps = in_city,
        county_usps = in_county,
        state_usps = in_state,
        country_usps = in_country
      )
      location_memory <- dplyr::bind_rows(
        location_memory,
        location_memory_row
      )
    }
    location_tbl[n, "zip_code_usps"] <- as.integer(in_zip)
    location_tbl[n, "city_usps"] <- in_city
    location_tbl[n, "county_usps"] <- in_county
    location_tbl[n, "state_usps"] <- in_state
    location_tbl[n, "country_usps"] <- in_country
  }
  location_tbl <- location_tbl %>%
    dplyr::mutate(zip_code_usps = as.integer(.data$zip_code_usps))

  return(location_tbl)
}
