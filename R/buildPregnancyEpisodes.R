
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

  # get names for intermediate tables
  prefix <- omopgenerics::tmpPrefix()
  nms <- createNames(prefix = prefix)

  # insert concepts and tables
  cdm <- insertPregnancyTables(cdm = cdm, nms = nms)

  # get starting events
  cdm <- getPregnancyEvents(cdm = cdm, nms = nms)

  # valid outcomes tables
  vo <- dplyr::tibble(person_id = integer(), event_id = integer())
  cdm <- omopgenerics::insertTable(cdm = cdm, name = nms$valid_outcomes, table = vo)

  # populate life birth events
  cdm <- populateLifeBirth(cdm = cdm, nms = nms)

}
createNames <- function(prefix) {
  list(
    gest_est = omopgenerics::uniqueTableName(prefix = prefix),
    outcome_limit = omopgenerics::uniqueTableName(prefix = prefix),
    pregnancy_concepts = omopgenerics::uniqueTableName(prefix = prefix),
    term_durations = omopgenerics::uniqueTableName(prefix = prefix),
    pregnancy_events = omopgenerics::uniqueTableName(prefix = prefix),
    valid_outcomes = omopgenerics::uniqueTableName(prefix = prefix)
  )
}
insertPregnancyTables <- function(cdm, nms) {
  cdm |>
    omopgenerics::insertTable(name = nms$gest_est, table = gest_est) |>
    omopgenerics::insertTable(name = nms$outcome_limit, table = outcome_limit) |>
    omopgenerics::insertTable(name = nms$pregnancy_concepts, table = pregnancy_concepts) |>
    omopgenerics::insertTable(name = nms$term_durations, table = term_durations)
}
getPregnancyEvents <- function(cdm, nms) {
  # temp prefix
  prefix <- omopgenerics::tmpPrefix()

  # check concepts with data_value != N/A
  naIsEmpty <- cdm[[nms$pregnancy_concepts]] |>
    dplyr::filter(.data$data_value != "N/A") |>
    omopgenerics::isTableEmpty()

  # subset condition_occurrence
  co <- cdm$condition_occurrence |>
    dplyr::select(
      "person_id",
      "concept_id" = "condition_concept_id",
      "start_date" = "condition_start_date"
    ) |>
    dplyr::inner_join(
      cdm[[nms$pregnancy_concepts]] |>
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
      cdm[[nms$pregnancy_concepts]] |>
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
      cdm[[nms$pregnancy_concepts]] |>
        dplyr::select(!"data_value"),
      by = "concept_id"
    ) |>
    dplyr::mutate(type = "Obs") |>
    dplyr::compute(name = omopgenerics::uniqueTableName(prefix = prefix))
  if (!naIsEmpty) {
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
            cdm[[nms$pregnancy_concepts]] |>
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
      cdm[[nms$pregnancy_concepts]] |>
        dplyr::select(!"data_value"),
      by = "concept_id"
    ) |>
    dplyr::mutate(type = "Meas") |>
    dplyr::compute(name = omopgenerics::uniqueTableName(prefix = prefix))
  if (!naIsEmpty) {
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
            cdm[[nms$pregnancy_concepts]] |>
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
      .default = as.numeric(NA)
    )) |>
    dplyr::select("person_id", "category", "gest_value", "start_date") |>
    # exclude people who have ONLY 1 glucose test
    dplyr::anti_join(
      events |>
        dplyr::filter(.data$category == "DIAB") |>
        dplyr::group_by(.data$person_id) |>
        dplyr::summarise(n = dplyr::n()) |>
        dplyr::filter(.data$n == 1) |>
        dplyr::select("person_id"),
      by = "person_id"
    ) |>
    # eliminate not plausible events for gestational
    dplyr::filter(!(.data$category == "GEST" & (is.na(.data$gest_value) | .data$gest_value > 301))) |>
    dplyr::compute(name = omopgenerics::uniqueTableName(prefix = prefix))

  # events drug
  eventsDrug <- cdm[[nms$pregnancy_concepts]] |>
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
  cdm[[nms$pregnancy_events]] <- eventsNonDrug |>
    dplyr::select(
      "person_id", "category", "event_date" = "start_date", "gest_value"
    ) |>
    dplyr::union_all(
      eventsDrug |>
        dplyr::select(
          "person_id", "category", "event_date" = "start_date", "gest_value"
        )
    ) |>
    dplyr::group_by(.data$person_id) |>
    dplyr::arrange(.data$event_date) |>
    dplyr::mutate(event_id = as.integer(dplyr::row_number())) |>
    dplyr::ungroup() |>
    dplyr::compute(name = nms$pregnancy_events)

  # delete temp tables
  omopgenerics::dropSourceTable(cdm = cdm, name = dplyr::starts_with(prefix))
}
populateLifeBirth <- function(cdm, nms) {
  prefix <- omopgenerics::tmpPrefix()

  # LB events
  lbEvent <- cdm[[nms$preg]] |>
    dplyr::filter(.data$category == "LB") |>
    dplyr::distinct(.data$person_id)

  categories <- unique(c(outcome_limit$first_preg_category, outcome_limit$outcome_preg_category))

  events <- cdm[[nms$pregnancy_events]] |>
    dplyr::filter(.data$category %in% .env$categories) |>
    dplyr::inner_join(lbEvent, by = "person_id") |>
    dplyr::group_by(
      .data$person_id, .data$category, .data$event_date, .data$gest_value
    ) |>
    dplyr::filter(.data$event_id == min(.data$event_id, na.rm = TRUE)) |>
    dplyr::select("person_id", "event_id", "category", "event_date", "gest_value") |>
    dplyr::compute(name = omopgenerics::uniqueTableName(prefix = prefix))

  firstOutcomeEvent <- events |>
    dplyr::filter(.data$category == "LB") |>
    dplyr::group_by(.data$person_id) |>
    dplyr::arrange(.data$event_date, .data$event_id) |>
    dplyr::filter(dplyr::row_number() == 1) |>
    dplyr::compute(name = omopgenerics::uniqueTableName(prefix = prefix))

  while (!omopgenerics::isTableEmpty(table = firstOutcomeEvent)) {
    # add valid_outcomes
    cdm[[nms$valid_outcomes]] <- cdm[[nms$valid_outcomes]] |>
      dplyr::union_all(
        firstOutcomeEvent |>
          dplyr::select("person_id", "event_id")
      ) |>
      dplyr::compute(name = nms$valid_outcomes)

    # deleted events
    toDelete <- events |>
      dplyr::select(
        "person_id",
        "event_id",
        "event_date",
        "first_preg_category" = "category"
      ) |>
      dplyr::inner_join(
        firstOutcomeEvent |>
          dplyr::select(
            "person_id",
            "outcome_date" = "event_date",
            "outcome_preg_category" = "category"
          ),
        by = "person_id"
      ) |>
      dplyr::inner_join(
        cdm[[nms$outcome_limit]] |>
          dplyr::select("first_preg_category", "outcome_preg_category", "min_days"),
        by = c("first_preg_category", "outcome_preg_category")
      ) |>
      dplyr::filter(
        clock::date_count_between(start = .data$event_date, end = .data$outcome_date, precision = "day") + 1 < .data$min_days
      ) |>
      dplyr::select("person_id", "event_id") |>
      dplyr::compute(name = omopgenerics::uniqueTableName(prefix = prefix))


  }

  omopgenerics::dropSourceTable(cdm = cdm, name = dplyr::starts_with(prefix))
}
