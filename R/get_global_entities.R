#' Build a Vector containing names of US and International states/provinces
#'
#' @param filepath Path to the csv file of the UN Dataset downloaded
#' from https://unece.org/trade/cefact/UNLOCODE-Download)
#'
#' @return Vector containing names of US and International states/provinces
#'
#' @importFrom magrittr "%>%"

get_global_entities <- function(filepath) {
  test_tbl <- readr::read_csv(filepath,
    n_max = 1,
    show_col_types = FALSE
  )
  stopifnot(ncol(test_tbl) == 4)

  global_regions_vec <- readr::read_csv(filepath,
    col_names = c(
      "country_code",
      "region_code",
      "region_name",
      "region_type"
    ),
    locale = readr::locale(encoding = "latin1"),
    show_col_types = FALSE
  ) %>%
    dplyr::mutate(
      region_name = stringr::str_to_upper(region_name,
        locale = "en"
      ),
      region_name = stringr::str_trim(
        stringr::str_replace_all(
          region_name,
          "\\[.*\\]",
          ""
        ),
        c("both")
      )
    ) %>%
    dplyr::select(
      "region_name",
      "country_code"
    ) %>%
    tidyr::drop_na() %>%
    dplyr::distinct() %>%
    dplyr::pull(region_name,
      name = country_code
    )

  return(global_regions_vec)
}
