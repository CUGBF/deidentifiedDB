#' Get Pangolin Lineage Distribution of Sequenced Samples
#'
#' @param viralrecon_tbl viralrecon table from deidentifiedDB database
#' @param sample_collection_tbl Sample Collection table from deidentifiedDB database
#' @param start_date Filter sample collection table for samples collected after
#' this date. Provide date as in "2021-08-01" for August 1, 2021
#' @param end_date Filter sample collection table for samples collected on or
#' before this date. Provide date as in "2021-08-01" for August 1, 2021
#' @param n_days Duration of time (in days) for which only the first collected
#' COVID-19 positive sample  for a patient is retained
#' @param time_zone Time zone for collection time (Default: "America/New_York")
#'
#' @return Tibble containing monthly count of sequenced samples in
#' each Pangolin lineage
#' @export
#'
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
get_pangolin_distribution <- function(viralrecon_tbl,
                                      sample_collection_tbl,
                                      start_date = "2021-01-01",
                                      end_date = as.character(
                                        lubridate::date(lubridate::now())
                                      ),
                                      n_days = 30,
                                      time_zone = "America/New_York") {
  stopifnot(nrow(viralrecon_tbl) > 0 &
    nrow(sample_collection_tbl) > 0)
  stopifnot(all(c(
    "testkit_id",
    "lineage",
    "run_date"
  ) %in% colnames(viralrecon_tbl)))

  stopifnot(all(c(
    "testkit_id",
    "patient_id",
    "rymedi_result",
    "collection_date"
  ) %in% colnames(sample_collection_tbl)))

  if (!(lubridate::is.timepoint(sample_collection_tbl$collection_date))) {
    sample_collection_tbl <- sample_collection_tbl %>%
      dplyr::mutate(collection_date = lubridate::as_datetime(collection_date,
        tz = time_zone
      ))
  }

  if (!(lubridate::is.Date(viralrecon_tbl$run_date))) {
    viralrecon_tbl <- viralrecon_tbl %>%
      dplyr::mutate(run_date = lubridate::as_date(run_date))
  }

  lineage_assignment_tbl <- viralrecon_tbl %>%
    dplyr::filter(
      !is.na(lineage),
      stringr::str_to_lower(lineage) != "none"
    ) %>%
    dplyr::group_by(testkit_id) %>%
    dplyr::arrange(dplyr::desc(run_date)) %>%
    dplyr::slice_head() %>%
    dplyr::ungroup() %>%
    dplyr::select(
      "testkit_id",
      "lineage"
    )

  sc_tbl <- get_sc_wo_redundant(
    sample_collection_tbl,
    start_date,
    end_date,
    n_days
  )

  sc_positive_tbl <- sc_tbl %>%
    dplyr::filter(
      rymedi_result == c("POSITIVE"),
      testkit_id %in% lineage_assignment_tbl$testkit_id
    ) %>%
    dplyr::select(
      "testkit_id",
      "collection_month",
      "collection_date"
    )

  lineage_assignment_tbl <- dplyr::inner_join(lineage_assignment_tbl,
    sc_positive_tbl,
    by = "testkit_id"
  )

  output_tbl <- lineage_assignment_tbl %>%
    dplyr::group_by(
      collection_month,
      lineage
    ) %>%
    dplyr::summarise(n_sequenced_samples = dplyr::n())

  lineages_present <- sort(unique(output_tbl$lineage))

  output_tbl <- output_tbl %>%
    dplyr::mutate(lineage = factor(lineage,
      levels = lineages_present
    ))

  return(output_tbl)
}
