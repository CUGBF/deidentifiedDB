#' Get Nextclade Distribution of Sequenced Samples
#'
#' @param viralrecon_tbl viralrecon table from deidentifiedDB database
#' @param sample_collection_tbl Sample Collection table from deidentifiedDB database
#' @param start_date Filter sample collection table for samples collected after
#' this date. Provide date as in "2021-08-01" for August 1, 2021
#' @param end_date Filter sample collection table for samples collected on or
#' before this date. Provide date as in "2021-08-01" for August 1, 2021
#' @param n_days Duration of time (in days) for which only the first collected
#' COVID-19 positive sample  for a patient is retained
#'
#' @return Tibble containing monthly count of sequenced samples in
#' each Nextclade
#' @export
#'
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
get_nextclade_distribution <- function(viralrecon_tbl,
                                       sample_collection_tbl,
                                       start_date = "2021-01-01",
                                       end_date = lubridate::date(lubridate::now()),
                                       n_days = 30) {
  stopifnot(nrow(viralrecon_tbl) > 0 &
    nrow(sample_collection_tbl) > 0)
  stopifnot(all(c(
    "testkit_id",
    "clade",
    "run_date_time"
  ) %in% colnames(viralrecon_tbl)))

  stopifnot(all(c(
    "testkit_id",
    "patient_id",
    "rymedi_result",
    "collection_date"
  ) %in% colnames(sample_collection_tbl)))

  clade_assignment_tbl <- viralrecon_tbl %>%
    dplyr::filter(!is.na(clade)) %>%
    dplyr::group_by(.data$testkit_id) %>%
    dplyr::arrange(desc(.data$run_date_time)) %>%
    dplyr::slice_head() %>%
    dplyr::ungroup() %>%
    dplyr::select(
      "testkit_id",
      "clade"
    )

  sc_tbl <- get_sc_wo_redundant(
    sample_collection_tbl,
    start_date,
    end_date,
    n_days
  )

  sc_positive_tbl <- sc_tbl %>%
    dplyr::filter(
      .data$rymedi_result == c("POSITIVE"),
      .data$testkit_id %in% clade_assignment_tbl$testkit_id
    ) %>%
    dplyr::select(
      "testkit_id",
      "collection_month",
      "collection_date"
    )

  clade_assignment_tbl <- dplyr::inner_join(clade_assignment_tbl,
    sc_positive_tbl,
    by = "testkit_id"
  )

  output_tbl <- clade_assignment_tbl %>%
    dplyr::group_by(
      .data$collection_month,
      .data$clade
    ) %>%
    dplyr::summarise(n_sequenced_samples = dplyr::n())

  clades_present <- sort(unique(output_tbl$clade))

  output_tbl <- output_tbl %>%
    dplyr::mutate(clade = factor(.data$clade,
      levels = clades_present
    ))

  return(output_tbl)
}
