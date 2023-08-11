#' Compile Sample Collection Data for deidentifiedDB SQLite Database
#'
#' @param sc_tbl Tibble containing sample collection data
#' @param us_zip_codes_filepath Path to the csv file containing US zip codes
#' (downloaded from UnitedStatesZipCodes.org)
#' @param global_regions_filepath Path to the csv file of the UN Dataset downloaded
#' from https://unece.org/trade/cefact/UNLOCODE-Download)
#'
#' @return Final tibble containing sample collection data ready to be appended
#' to deidentifiedDB SQLite database
#' @export
#'
#' @importFrom magrittr "%>%"

compile_sc_data <- function(sc_tbl,
                            us_zip_codes_filepath,
                            global_regions_filepath) {
  location_combinations <- sc_tbl %>%
    dplyr::select("city", "state", "zip_code") %>%
    dplyr::distinct()

  location_combinations <- lookup_zip_code(
    location_combinations,
    us_zip_codes_filepath,
    global_regions_filepath
  )

  output_tbl <- tidy_up_location(
    sc_tbl,
    location_combinations
  ) %>%
    polish_sc()

  return(output_tbl)
}
