#' Find Mode
#'
#' @param values_vec vector containing values in a column
#'
#' @return mode value
#'
get_mode <- function(values_vec){
  # https://stackoverflow.com/questions/31400445/r-how-to-find-the-mode-of-a-vector
  uni_values <- unique(values_vec)
  output_value <- uni_values[which.max(tabulate(match(values_vec, uni_values)))]

  return(output_value)
}
