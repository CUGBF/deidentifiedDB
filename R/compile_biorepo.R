#' Compile Biorepository for inclusion in deidentifiedDB database
#'
#' @param filepath Path to the biorepository csv file
#'
#' @return Tibble containing biorepository information for deidentifiedDB database
#' @export
#'
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
compile_biorepo <- function(filepath) {
  test_tbl <- readr::read_csv(filepath,
    na = c(
      "", "NA", "N/A", "<NA>", "null",
      "Null", "Missing", "Error 404"
    ),
    n_max = 1,
    show_col_types = FALSE
  )
  stopifnot(all(c(
    "Box IDN",
    "Box Position 1",
    "Vial IDN 1",
    "Box Position 2",
    "Vial IDN 2",
    "Box Position 3",
    "Vial IDN 3"
  ) %in% colnames(test_tbl)))

  stopifnot(any(c(
    "TestKit ID",
    "TestKitID",
    "TestKitId"
  ) %in% colnames(test_tbl)))

  output_tbl <- readr::read_csv(filepath,
    col_names = c(
      "testkit_id", "box_idn", "box_position_1",
      "vial_idn_1", "box_position_2",
      "vial_idn_2", "box_position_3",
      "vial_idn_3", "sample_id", "date",
      "p1_a", "p1_b", "n1_a", "n1_b",
      "rymedi_result"
    ),
    na = c(
      "", "NA", "<NA>", "Missing",
      "Error 404", "None", "null", "NULL", "Null", "NO AMP", "NOAMP"
    ),
    show_col_types = FALSE,
    skip = 1
  ) %>%
    dplyr::select(
      "testkit_id", "box_idn", "box_position_1",
      "vial_idn_1", "box_position_2",
      "vial_idn_2", "box_position_3",
      "vial_idn_3"
    ) %>%
    dplyr::mutate_at(
      c("vial_idn_1", "vial_idn_2", "vial_idn_3"),
      ~ replace(., which(. == "NES"), "not_enough_sample")
    )

  return(output_tbl)
}
