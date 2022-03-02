#' Label samples on whether they were used as controls
#'
#' @param testkit_id Testkit ID
#' @param result Value from the 'result' column of the diagnostics table
#'
#' @return A logical value : TRUE if sample was used as a control, else FALSE
#'
#' @examples
detect_control <- function(testkit_id, result) {
  control_status <- ((str_detect(
    str_to_upper(testkit_id),
    "BLANK|NEGATIVE|POSITIVE"
  ) &
    str_detect(
      str_to_upper(testkit_id),
      "CONTROL|KNOWN"
    )) |
    str_detect(
      str_to_lower(result),
      "known positive"
    ))
  return(control_status)
}
