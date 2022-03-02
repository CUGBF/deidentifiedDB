#' Format the columns containing Ct values to remove non-numeric values
#'
#' @param ct_value Value from any of the columns containing Ct values from
#' qRT-PCR in the diagnostics table
#'
#' @return
#'
#' @examples
tidy_up_ct <- function(ct_value) {
  tidied_ct <- parse_number(replace(
    ct_value,
    !str_detect(
      ct_value,
      regex("^[0-9.]*$",
        ignore_case = T
      )
    ),
    NA
  ))
  return(tidied_ct)
}
