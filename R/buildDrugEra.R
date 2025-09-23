
#' Build the `drug_era` table
#'
#' @param cdm A `cdm_reference` object.
#' @param collapseDays Number of days that two exposures can be separated to be
#' collapsed in a single era.
#'
#' @return The lazy `drug_era` table.
#' @export
#'
#' @examples
#' \donttest{
#' library(omock)
#' library(OmopConstructor)
#' library(dplyr, warn.conflicts = TRUE)
#'
#' cdm <- mockCdmFromDataset(datasetName = "GiBleed", source = "duckdb")
#'
#' cdm$drug_era <- buildDrugEra(cdm = cdm)
#' cdm$drug_era |>
#'   glimpse()
#'
#' }
#'
buildDrugEra <- function(cdm,
                         collapseDays = 31L) {
  # input check
  cdm <- omopgenerics::validateCdmArgument(cdm = cdm)
  omopgenerics::assertNumeric(collapseDays, integerish = TRUE, min = 0, length = 1)
  collapseDays <- as.integer(collapseDays)
  if (collapseDays != 31L) {
    cli::cli_inform(c("!" = "Building {.pkg drug_era} with `collapseDays = {collapseDays}`, which is different from the OHDSI recommended collapseDays (30)."))
  }

  # prepare drug exposure
  drugEra <- cdm$drug_exposure |>
    dplyr::filter(.data$drug_concept_id != 0) |>
    dplyr::inner_join(
      cdm$concept |>
        dplyr::filter(
          .data$vocabulary_id == "RxNorm" &
            .data$concept_class_id == "Ingredient"
        ) |>
        dplyr::select("ingredient_concept_id" = "concept_id") |>
        dplyr::inner_join(
          cdm$concept_ancestor |>
            dplyr::select(
              "ingredient_concept_id" = "ancestor_concept_id",
              "drug_concept_id" = "descendant_concept_id"
            ),
          by = "ingredient_concept_id"
        ),
      by = "drug_concept_id"
    ) |>
    dplyr::select(
      "person_id",
      "drug_concept_id" = "ingredient_concept_id",
      "drug_era_start_date" = "drug_exposure_start_date",
      "drug_era_end_date" = "drug_exposure_end_date"
    )

  # create drug_era with collapseDays = 0
  drugEra <- drugEra |>
    dplyr::mutate(drug_exposure_count = 1L) |>
    collapseRecords(
      startDate = "drug_era_start_date",
      endDate = "drug_era_end_date",
      by = c("person_id", "drug_concept_id"),
      gap = 0L,
      toSummarise = "drug_exposure_count",
      name = "drug_era"
    )

  # create final drug_era
  if (collapseDays > 0) {
    drugEra <- drugEra |>
      dplyr::mutate(days_exposure = as.integer(clock::date_count_between(
        start = .data$drug_era_start_date,
        end = .data$drug_era_end_date,
        precision = "day"
      )) + 1L) |>
      dplyr::compute(name = "drug_era") |>
      collapseRecords(
        startDate = "drug_era_start_date",
        endDate = "drug_era_end_date",
        by = c("person_id", "drug_concept_id"),
        gap = collapseDays,
        toSummarise = c("drug_exposure_count", "days_exposure"),
        name = "drug_era"
      ) |>
      dplyr::mutate(
        days_era = as.integer(clock::date_count_between(
          start = .data$drug_era_start_date,
          end = .data$drug_era_end_date,
          precision = "day"
        )) + 1L,
        gap_days = as.integer(.data$days_era - .data$days_exposure)
      ) |>
      dplyr::select(!c("days_era", "days_exposure"))
  } else {
    drugEra <- drugEra |>
      dplyr::mutate(gap_days = 0L)
  }

  # final drug_era
  cdm$drug_era <- drugEra |>
    dplyr::ungroup() |>
    dplyr::arrange(.data$person_id) |>
    dplyr::mutate(drug_era_id = as.integer(dplyr::row_number())) |>
    dplyr::arrange() |>
    dplyr::select(
      "drug_era_id", "person_id", "drug_concept_id", "drug_era_start_date",
      "drug_era_end_date", "drug_exposure_count", "gap_days"
    ) |>
    dplyr::compute(name = "drug_era")

  return(cdm$drug_era)
}
