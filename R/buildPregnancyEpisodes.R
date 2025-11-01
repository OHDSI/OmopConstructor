
#' Build the `pregnancy_episodes` extenssion table
#'
#' @param cdm A cdm_reference object.
#'
#' @return A cdm_reference object with the `pregnancy_episodes` table.
#' @export
#'
buildPregnancyEpisodes <- function(cdm) {
  # input check
  cdm <- omopgenerics::validateCdmArgument(cdm = cdm)
  if ("pregnancy_episode" %in% names(cdm)) {
    cli::cli_abort(c(
      x = "The table `pregnancy_episode` is already present in the cdm.",
      i = "If you want to overwrite it please run {.run cdm$pregnancy_episode
      <- NULL} and try again."
    ))
  }

  tmpName <- "temp_pregnancy_episodes"

  # get starting events
  cdm <- startingEvents(cdm = cdm, name = tmpName)

}
startingEvents <- function(cdm, name) {
  # temp prefix
  prefix <- omopgenerics::tmpPrefix()

  # insert concepts
  nm <- omopgenerics::uniqueTableName(prefix = prefix)
  cdm <- omopgenerics::insertTable(cdm = cdm, name = nm, table = pregnancy_concepts)

  # check concepts with data_value != N/A
  isNotEmpty <- sum(pregnancy_concepts$data_value != "N/A") > 0

  # subset condition_occurrence
  co <- cdm$condition_occurrence |>
    dplyr::select(
      "person_id",
      "concept_id" = "condition_concept_id",
      "start_date" = "condition_start_date"
    ) |>
    dplyr::inner_join(
      cdm[[nm]] |>
        dplyr::select(!"data_value"),
      by = "concept_id"
    ) |>
    dplyr::mutate(
      type = "Cond",
      value_as_string = " ",
      value_as_number = as.numeric(NA)
    ) |>
    dplyr::compute(name = omopgenerics::uniqueTableName(prefix = prefix))

  # subset procedure_occurrence
  po <- cdm$procedure_occurrence |>
    dplyr::select(
      "person_id",
      "concept_id" = "procedure_concept_id",
      "start_date" = "procedure_date"
    ) |>
    dplyr::inner_join(
      cdm[[nm]] |>
        dplyr::select(!"data_value"),
      by = "concept_id"
    ) |>
    dplyr::mutate(
      type = "Proc",
      value_as_string = " ",
      value_as_number = as.numeric(NA)
    ) |>
    dplyr::compute(name = omopgenerics::uniqueTableName(prefix = prefix))

  # subset observation
  ob <- cdm$observation |>
    dplyr::select(
      "person_id",
      "concept_id" = "observation_concept_id",
      "start_date" = "observation_date",
      "value_as_string",
      "value_as_number"
    ) |>
    dplyr::inner_join(
      cdm[[nm]] |>
        dplyr::select(!"data_value"),
      by = "concept_id"
    ) |>
    dplyr::mutate(type = "Obs") |>
    dplyr::compute(name = omopgenerics::uniqueTableName(prefix = prefix))
  if (isNotEmpty) {
    ob <- ob |>
      dplyr::union_all(
        cdm$observation |>
          dplyr::select(
            "person_id",
            "concept_id" = "observation_concept_id",
            "start_date" = "observation_date",
            "value_as_string",
            "value_as_number"
          ) |>
          dplyr::inner_join(
            cdm[[nm]] |>
              dplyr::filter(.data$data_value != "N/A") |>
              dplyr::rename("value_as_string" = "data_value"),
            by = c("concept_id", "value_as_string")
          ) |>
          dplyr::mutate(type = "Obs") |>
          dplyr::compute(name = omopgenerics::uniqueTableName(prefix = prefix))
      )
  }

  # subset observation
  me <- cdm$measurement |>
    dplyr::select(
      "person_id",
      "concept_id" = "measurement_concept_id",
      "start_date" = "measurement_date",
      "value_as_string" = "value_source_value",
      "value_as_number"
    ) |>
    dplyr::inner_join(
      cdm[[nm]] |>
        dplyr::select(!"data_value"),
      by = "concept_id"
    ) |>
    dplyr::mutate(type = "Meas") |>
    dplyr::compute(name = omopgenerics::uniqueTableName(prefix = prefix))
  if (isNotEmpty) {
    me <- me |>
      dplyr::union_all(
        cdm$measurement |>
          dplyr::select(
            "person_id",
            "concept_id" = "measurement_concept_id",
            "start_date" = "measurement_date",
            "value_as_string" = "value_source_value",
            "value_as_number"
          ) |>
          dplyr::inner_join(
            cdm[[nm]] |>
              dplyr::filter(.data$data_value != "N/A") |>
              dplyr::rename("value_as_string" = "data_value"),
            by = c("concept_id", "value_as_string")
          ) |>
          dplyr::mutate(type = "Meas") |>
          dplyr::compute(name = omopgenerics::uniqueTableName(prefix = prefix))
      )
  }

  # subset to raw events
  events <- co |>
    dplyr::union_all(po) |>
    dplyr::union_all(ob) |>
    dplyr::union_all(me) |>
    dplyr::compute(name = omopgenerics::uniqueTableName(prefix = prefix))

  # events non drug
  eventsNonDrug <- events |>
    # convert gestational weeks to days
    dplyr::mutate(gest_value = dplyr::case_when(
      .data$category == "GEST" & !is.na(.data$value_as_number) ~ 7 * .data$value_as_number,
      .data$category == "GEST" & !is.na(.data$gest_value) ~ 7 * .data$gest_value,
      .default = NA_real_
    )) |>
    dplyr::select("person_id", "category", "gest_value", "start_date") |>
    # exclude people who have ONLY 1 glucose test
    dplyr::anti_join(
      events |>
        dplyr::filter(.data$category == "DIAB") |>
        dplyr::group_by(.data$person_id) |>
        dplyr::filter(dplyr::n() == 1) |>
        dplyr::select("person_id"),
      by = "person_id"
    ) |>
    # eliminate not plausible events for gestational
    dplyr::filter(!(.data$category == "GEST" & (is.na(.data$gest_value) | .data$gest_value > 301))) |>
    dplyr::compute(name = omopgenerics::uniqueTableName(prefix = prefix))

  # events drug
  eventsDrug <- cdm[[nm]] |>
    dplyr::select(
      "ancestor_concept_id" = "concept_id",
      "category",
      "gest_value"
    ) |>
    dplyr::filter(.data$category %in% c("CONTRA", "MTX", "OVULDR")) |>
    # get descendants
    dplyr::inner_join(
      cdm$concept_ancestor |>
        dplyr::select(
          "ancestor_concept_id",
          "concept_id" = "descendant_concept_id"
        ),
      by = "ancestor_concept_id"
    ) |>
    # get only concept_class_id of interest
    dplyr::inner_join(
      cdm$concept |>
        dplyr::filter(.data$concept_class_id %in% c(
          "Branded Drug", "Branded Pack", "Clinical Drug", "Clinical Pack",
          "Ingredient"
        )) |>
        dplyr::select("concept_id"),
      by = "concept_id"
    ) |>
    # join with drug exposure
    dplyr::inner_join(
      cdm$drug_exposure |>
        dplyr::select(
          "person_id",
          "concept_id" = "drug_concept_id",
          "start_date" = "drug_exposure_start_date"
        ),
      by = "concept_id"
    ) |>
    # subset individuals of interest
    dplyr::inner_join(
      eventsNonDrug |>
        dplyr::distinct(.data$person_id),
      by = "person_id"
    ) |>
    dplyr::compute(name = omopgenerics::uniqueTableName(prefix = prefix)) |>
    dplyr::select(!"ancestor_concept_id") |>
    dplyr::mutate(
      type = "Drug",
      value_as_string = " ",
      value_as_number = NA_real_
    )

  # join drugs and non drugs
  cdm[[name]] <- eventsNonDrug |>
    dplyr::select(
      "person_id", "category", "event_date" = "start_date", "gest_value"
    ) |>
    dplyr::union_all(
      eventsDrug |>
        dplyr::select(
          "person_id", "category", "event_date" = "start_date", "gest_value"
        )
    ) |>
    dplyr::compute(name = name)

  # delete temp tables
  omopgenerics::dropSourceTable(cdm = cdm, name = dplyr::starts_with(prefix))
}
