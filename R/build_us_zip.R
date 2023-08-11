#' Build a tibble with US Zip Code Information
#'
#' @param filepath Path to the csv file containing US zip codes
#' (downloaded from UnitedStatesZipCodes.org)
#'
#' @return Tibble containing US zip codes
#'
#' @importFrom magrittr "%>%"

build_us_zip <- function(filepath) {
  test_tbl <- readr::read_csv(filepath,
    n_max = 1,
    show_col_types = FALSE
  )
  stopifnot(all(c(
    "zip",
    "primary_city",
    "acceptable_cities",
    "state",
    "county",
    "country"
  ) %in% colnames(test_tbl)))

  output_tbl <- readr::read_csv(filepath,
    show_col_types = FALSE
  ) %>%
    dplyr::select(
      "zip",
      "primary_city",
      "acceptable_cities",
      "state",
      "county",
      "country"
    ) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        ~ stringr::str_to_upper(.x,
          locale = "en"
        )
      ),
      zip = as.character(zip)
    ) %>%
    dplyr::arrange(
      country,
      state,
      county,
      primary_city
    )
}
