#' Compile Metadata for GenBank Submission
#'
#' @param testkit_ids Testkit IDs to include in the GenBank Submission
#' @param sample_collection_tbl Path to the tibble containing
#' sample collection data
#' @param demographics_tbl Path to the tibble containing
#' demographics data
#' @param viralrecon_tbl Path to the tibble containing
#' viralrecon results
#' @param deidentifiedDB  Path to the deidentifiedDB SQLite File
#' @param time_zone Time zone for collection time (Default: "America/New_York")
#'
#' @return Tibble containing metadata information for GenBank submission.
#'
#' @export
#'
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
get_metadata_genbank <- function(testkit_ids,
                                 sample_collection_tbl = NULL,
                                 demographics_tbl = NULL,
                                 viralrecon_tbl = NULL,
                                 deidentifiedDB = NULL,
                                 time_zone = "America/New_York") {
  stopifnot(all(!is.null(c(
    sample_collection_tbl,
    demographics_tbl,
    viralrecon_tbl
  ))) | !is.null(deidentifiedDB))

  if (!is.null(deidentifiedDB)) {
    db_build <- DBI::dbConnect(RSQLite::SQLite(), deidentifiedDB)

    stopifnot(all(c(
      "sample_collection",
      "demographics",
      "viralrecon"
    ) %in% DBI::dbListTables(db_build)))

    demographics_tbl <- DBI::dbReadTable(db_build, "demographics")
    sample_collection_tbl <- DBI::dbReadTable(db_build, "sample_collection")
    viralrecon_tbl <- DBI::dbReadTable(db_build, "viralrecon")
    DBI::dbDisconnect(db_build)
  }

  metadata_vr <- viralrecon_tbl %>%
    dplyr::filter(
      .data$testkit_id %in% testkit_ids,
      !is.na(.data$median_coverage),
      !is.na(.data$clade),
      stringr::str_to_lower(.data$clade) != "none"
    ) %>%
    dplyr::select("testkit_id") %>%
    dplyr::distinct()

  if (!(lubridate::is.timepoint(sample_collection_tbl$collection_date))) {
    sample_collection_tbl <- sample_collection_tbl %>%
      dplyr::mutate(collection_date = lubridate::as_datetime(.data$collection_date,
        tz = time_zone
      ))
  }

  metadata_sc <- sample_collection_tbl %>%
    dplyr::filter(
      .data$testkit_id %in% metadata_vr$testkit_id,
      !is.na(.data$collection_date),
      !is.na(.data$patient_id)
    ) %>%
    dplyr::select(
      "testkit_id",
      "patient_id",
      "collection_date",
      "state",
      "county",
      "city",
      "gender"
    ) %>%
    dplyr::distinct()

  metadata_dem <- demographics_tbl %>%
    dplyr::filter(.data$patient_id %in% metadata_sc$patient_id) %>%
    dplyr::select(
      "patient_id",
      "birth_year"
    ) %>%
    dplyr::distinct()

  metadata_tbl <- dplyr::inner_join(metadata_sc,
    metadata_dem,
    by = "patient_id"
  ) %>%
    dplyr::distinct()

  metadata_tbl <- metadata_tbl %>%
    dplyr::mutate(
      age = lubridate::year(.data$collection_date) - .data$birth_year,
      age = replace(
        .data$age,
        is.na(.data$age),
        "unknown"
      ),
      gender = dplyr::recode(.data$gender,
        "M" = "Male",
        "F" = "Female"
      ),
      gender = replace(
        .data$gender,
        is.na(.data$gender),
        "gender unknown"
      ),
      city = replace(
        .data$city,
        is.na(.data$city),
        "CLEMSON"
      ),
      city = replace(
        .data$city,
        !(stringr::str_detect(
          .data$county,
          "PICKENS|ANDERSON|OCONEE|GREENVILLE"
        ) &
          .data$state == "SC"),
        "CLEMSON"
      ),
      city = stringr::str_to_title(.data$city),
      state = "South Carolina",
      `isolation-source` = "saliva"
    )

  metadata_tbl <- metadata_tbl %>%
    dplyr::mutate(
      sequence_ID = stringr::str_sub(.data$testkit_id, start = 8L),
      isolate = .data$sequence_ID,
      host = stringr::str_c("Homo sapiens; ", .data$gender, ", age ", .data$age),
      country = stringr::str_c("USA:South Carolina, ", .data$city),
      collection_date = as.character(lubridate::date(.data$collection_date))
    ) %>%
    dplyr::arrange(.data$collection_date) %>%
    dplyr::rename(`collection-date` = "collection_date")

  internal_tbl <- metadata_tbl %>%
    dplyr::select(
      "testkit_id",
      "sequence_ID"
    ) %>%
    dplyr::distinct() %>%
    dplyr::mutate(compiled_on = lubridate::date(lubridate::now()))

  submission_tbl <- metadata_tbl %>%
    dplyr::select(
      "sequence_ID",
      "isolate",
      "collection-date",
      "host",
      "country",
      "isolation-source"
    ) %>%
    dplyr::distinct()

  tbl_list <- list(
    int_tbl = internal_tbl,
    ext_tbl = submission_tbl
  )

  return(tbl_list)
}
