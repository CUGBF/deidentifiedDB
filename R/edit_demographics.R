#' Transform COVID19 Sample Collection and deidentified Demographics Data
#' from REDDI for the deidentifiedDB demographics and sample_collection tables
#'
#' @param filepath Path to the CSV file containing Sample Collection and
#' deidentified Demographics Data
#' @param date_fmt Format used to specify dates (Default: MM/DD/YYYY)
#' @param time_zone Time zone for collection time (Default: "America/New_York")
#'
#' @return Tibble with the Sample Collection and
#' the deidentified Demographics Data organized to make it convenient to update
#' deidentified DB database
#' @export
#'
#' @importFrom magrittr "%>%"
edit_demographics <- function(filepath,
                            date_fmt = c("%m/%d/%y"),
                            time_zone = "America/New_York"){
data <- read.csv(filepath)
rows <- nrow(data)
print(data[ ,6])

i <- 6
while( i < 7){
    data[ ,i] <- stri_sub(data[ ,i],-4,-1)
    i <- i +1
    print(i)
}
print(data)
}




test_that("Testing edit demographics() !", {
 data_demographics <- system.file("extdata",
  "data_demographics_sc_v2.csv",
   package = "deidentifiedDB"
)
output_tbl <- edit_demographics(data_demographics)
  expect_equal(nrow(output_tbl), 19)

})

                        