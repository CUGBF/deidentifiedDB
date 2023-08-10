#' Build a Vector containing US State Codes
#'
#' @param filepath Path to the csv file containing US zip codes
#' (downloaded from UnitedStatesZipCodes.org)
#'
#' @return Vector containing two letter codes of US states and territories
#' @export
#'
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
get_us_entities <- function(filepath) {
  us_zip_codes_tbl <- build_us_zip(filepath)
  stopifnot(all(c(
    "country",
    "state"
  ) %in% colnames(us_zip_codes_tbl)))
  stopifnot(c("US") %in% us_zip_codes_tbl$country)

  us_geo_entities <- us_zip_codes_tbl %>%
    dplyr::filter(country == "US") %>%
    dplyr::distinct(state) %>%
    dplyr::pull() %>%
    sort()

  return(us_geo_entities)
}
