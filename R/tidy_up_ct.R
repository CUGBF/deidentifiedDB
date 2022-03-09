#' Format the columns containing Ct values to remove non-numeric values
#'
#' @param ct_value Value from any of the columns containing Ct values from
#' qRT-PCR in the diagnostics table
#'
#' @return
#'
#' @importFrom magrittr "%>%"
tidy_up_ct <- function(ct_value) {
  if (is.numeric(ct_value)) {
    return(ct_value)
  }
  tidied_ct <- readr::parse_number(replace(
    ct_value,
    !stringr::str_detect(
      ct_value,
      stringr::regex("^[0-9.]*$",
        ignore_case = T
      )
    ),
    NA
  ))
  return(tidied_ct)
}
