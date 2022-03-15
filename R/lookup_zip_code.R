#' Compile Reference Location Information
#'
#' @param location_tbl Tibble with "zip_code", "state", and "city" columns
#' @param us_zip_codes_filepath Path to the csv file containing US zip codes
#' (downloaded from UnitedStatesZipCodes.org)
#' @param global_regions_filepath Path to the csv file of the UN Dataset downloaded
#' from https://unece.org/trade/cefact/UNLOCODE-Download)
#'
#' @return Tibble with validated USPS location information
#' @export
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

    current_zip <- location_combination %>%
      dplyr::pull(.data$zip_code)

    current_state <- location_combination %>%
      dplyr::pull(.data$state)

    current_city <- location_combination %>%
      dplyr::pull(.data$city)

    temp_location_info <- location_memory %>%
      dplyr::filter(
        .data$zip_code == current_zip,
        .data$state == current_state,
        .data$city == current_city
      )

    stopifnot(nrow(temp_location_info) <= 1)

    if (nrow(temp_location_info) == 1) {

      in_zip <- as.integer(temp_location_info %>%
        dplyr::pull(.data$zip_code_usps))

      in_state <- temp_location_info %>%
        dplyr::pull(.data$state_usps)

      in_county <- temp_location_info %>%
        dplyr::pull(.data$county_usps)

      in_city <- temp_location_info %>%
        dplyr::pull(.data$city_usps)

      in_country <- temp_location_info %>%
        dplyr::pull(.data$country_usps)

    } else {
      # cities in the state of the current record
      cities_in_state <- us_zip_codes %>%
        dplyr::filter(.data$state == current_state) %>%
        dplyr::pull(.data$primary_city)

      # Is the city in current record within acceptable city entries for the state ?
      acceptable_cities_in_state <- us_zip_codes %>%
        dplyr::filter(.data$state == current_state) %>%
        dplyr::pull(.data$acceptable_cities)

      acceptable_city_status <- sum(stringr::str_detect(
        acceptable_cities_in_state,
        current_city
      ), na.rm = TRUE) > 0

      if (current_state %in% us_geo_entities) {
        cleaned_up_zip <- tidy_up_zip(current_zip)

        if (!is.na(cleaned_up_zip) && cleaned_up_zip %in% us_zip_codes$zip) {
          in_zip <- cleaned_up_zip
          zip_df <- us_zip_codes %>%
            dplyr::filter(.data$zip == cleaned_up_zip) %>%
            dplyr::slice_head()
        } else if (!is.na(current_city) && !is.na(current_state) &&
          (current_city %in% (cities_in_state))) {
          zip_df <- us_zip_codes %>%
            dplyr::filter(
              .data$state == current_state,
              .data$primary_city == current_city
            ) %>%
            dplyr::arrange(.data$zip) %>%
            dplyr::slice_head()
          in_zip <- zip_df$zip
        } else if (!is.na(current_city) && !is.na(current_state) &&
          (acceptable_city_status)) {
          zip_df <- us_zip_codes %>%
            dplyr::filter(
              .data$state == current_state,
              stringr::str_detect(.data$acceptable_cities, current_city)
            ) %>%
            dplyr::arrange(.data$zip) %>%
            dplyr::slice_head()
          in_zip <- zip_df$zip
        } else {
          zip_df <- tibble::tibble(
            zip = NA_integer_,
            state = current_state,
            county = NA_character_,
            primary_city = current_city,
            country = "US"
          )
          in_zip <- zip_df$zip
        }
        in_state <- zip_df$state
        in_county <- zip_df$county
        in_city <- zip_df$primary_city
        in_country <- zip_df$country
      } else if (!(is.na(current_state) || (current_state %in% us_geo_entities))) {
        if (current_state %in% global_regions_vec) {
          in_country <- names(which(global_regions_vec == current_state))[1]
        } else {
          in_country <- NA_character_
        }
        in_zip <- NA_integer_
        in_state <- current_state
        in_county <- NA_character_
        in_city <- current_city
      } else {
        cleaned_up_zip <- tidy_up_zip(current_zip)
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
        zip_code = current_zip,
        state = current_state,
        city = current_city,
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

      rm(location_memory_row)
    }

    if (any(!is.na(c(in_zip, in_city, in_county, in_state, in_country)))){
      location_tbl[n, "zip_code_usps"] <- as.integer(in_zip)
      location_tbl[n, "city_usps"] <- in_city
      location_tbl[n, "county_usps"] <- in_county
      location_tbl[n, "state_usps"] <- in_state
      location_tbl[n, "country_usps"] <- in_country
    }

    rm(list = c("in_zip",
                "in_city",
                "in_county",
                "in_state",
                "in_country"))
  }

  return(location_tbl)
}
