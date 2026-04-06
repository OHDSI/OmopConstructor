
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
  if ("pregnancy_episode" %in% names(cdm) || "pregnancy_episodes" %in% names(cdm)) {
    cli::cli_abort(c(
      x = "The table `pregnancy_episodes` is already present in the cdm.",
      i = "If you want to overwrite it please run {.run cdm$pregnancy_episodes
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

  # populate Still Birth events
  cdm <- populateStillbirth(cdm = cdm, nms = nms)

  # populate Ectropic events
  cdm <- populateEctropic(cdm = cdm, nms = nms)

  return(cdm)
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
getFirstOutcomeEvent <- function(events, outcomeCategory, prefix) {
  events |>
    dplyr::filter(.data$category == .env$outcomeCategory) |>
    dplyr::group_by(.data$person_id) |>
    dplyr::arrange(.data$event_date, .data$event_id) |>
    dplyr::filter(dplyr::row_number() == 1) |>
    dplyr::ungroup() |>
    dplyr::compute(name = omopgenerics::uniqueTableName(prefix = prefix))
}
getTemporalInvalidOutcomes <- function(cdm, nms, firstOutcomeEvent, prefix) {
  cdm[[nms$valid_outcomes]] |>
    dplyr::inner_join(
      cdm[[nms$pregnancy_events]] |>
        dplyr::select(
          "person_id", "event_id",
          "prior_category" = "category",
          "prior_event_date" = "event_date"
        ),
      by = c("person_id", "event_id")
    ) |>
    dplyr::inner_join(
      firstOutcomeEvent |>
        dplyr::select(
          "person_id",
          "first_event_id" = "event_id",
          "first_category" = "category",
          "first_event_date" = "event_date"
        ),
      by = "person_id"
    ) |>
    dplyr::mutate(
      prior = dplyr::if_else(
        .data$prior_event_date <= .data$first_event_date, 1L, 0L
      ),
      distance = abs(as.integer(clock::date_count_between(
        start = .data$prior_event_date,
        end = .data$first_event_date,
        precision = "day"
      )) + 1L)
    ) |>
    dplyr::inner_join(
      cdm[[nms$outcome_limit]] |>
        dplyr::select(
          "first_category" = "first_preg_category",
          "prior_category" = "outcome_preg_category",
          "min_days_forward" = "min_days"
        ),
      by = c("first_category", "prior_category")
    ) |>
    dplyr::inner_join(
      cdm[[nms$outcome_limit]] |>
        dplyr::select(
          "prior_category" = "first_preg_category",
          "first_category" = "outcome_preg_category",
          "min_days_reverse" = "min_days"
        ),
      by = c("first_category", "prior_category")
    ) |>
    dplyr::filter(
      (.data$prior == 1L & .data$distance < .data$min_days_reverse) |
        (.data$prior == 0L & .data$distance < .data$min_days_forward)
    ) |>
    dplyr::distinct(.data$person_id, "event_id" = .data$first_event_id) |>
    dplyr::compute(name = omopgenerics::uniqueTableName(prefix = prefix))
}
appendValidOutcomes <- function(cdm, nms, x) {
  cdm[[nms$valid_outcomes]] <- cdm[[nms$valid_outcomes]] |>
    dplyr::union_all(
      x |>
        dplyr::select("person_id", "event_id")
    ) |>
    dplyr::compute(name = nms$valid_outcomes)
  cdm
}
dropCurrentOutcomeEvents <- function(events, firstOutcomeEvent, outcomeCategory, prefix) {
  events <- events |>
    dplyr::anti_join(
      firstOutcomeEvent |>
        dplyr::select("person_id", "event_id"),
      by = c("person_id", "event_id")
    ) |>
    dplyr::compute(name = omopgenerics::uniqueTableName(prefix = prefix))

  targetPeople <- events |>
    dplyr::filter(.data$category == .env$outcomeCategory) |>
    dplyr::distinct(.data$person_id)

  events |>
    dplyr::inner_join(targetPeople, by = "person_id") |>
    dplyr::compute(name = omopgenerics::uniqueTableName(prefix = prefix))
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
    # exclude people who have ONLY glucose test
    dplyr::anti_join(
      events |>
        dplyr::group_by(.data$person_id) |>
        dplyr::summarise(
          n_categories = dplyr::n_distinct(.data$category),
          has_diab = any(.data$category == "DIAB")
        ) |>
        dplyr::filter(.data$n_categories == 1 & .data$has_diab) |>
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
  lbEvent <- cdm[[nms$pregnancy_events]] |>
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
    dplyr::ungroup() |>
    dplyr::compute(name = omopgenerics::uniqueTableName(prefix = prefix))

  firstOutcomeEvent <- getFirstOutcomeEvent(
    events = events, outcomeCategory = "LB", prefix = prefix
  )

  while (!omopgenerics::isTableEmpty(table = firstOutcomeEvent)) {
    # add valid_outcomes
    cdm <- appendValidOutcomes(cdm = cdm, nms = nms, x = firstOutcomeEvent)

    # deleted events that dont satisfy outcome_limit criteria
    toDelete <- events |>
      dplyr::select(
        "person_id",
        "event_id",
        "e_date" = "event_date",
        "outcome_preg_category" = "category"
      ) |>
      dplyr::inner_join(
        firstOutcomeEvent |>
          dplyr::select(
            "person_id",
            "foe_date" = "event_date",
            "first_preg_category" = "category"
          ),
        by = "person_id"
      ) |>
      dplyr::inner_join(
        cdm[[nms$outcome_limit]] |>
          dplyr::select("first_preg_category", "outcome_preg_category", "min_days"),
        by = c("first_preg_category", "outcome_preg_category")
      ) |>
      dplyr::filter(
        clock::date_count_between(start = .data$e_date, end = .data$foe_date, precision = "day") + 1 < .data$min_days
      ) |>
      dplyr::select("person_id", "event_id") |>
      dplyr::compute(name = omopgenerics::uniqueTableName(prefix = prefix))

    # delete events
    events <- events |>
      dplyr::anti_join(toDelete, by = c("person_id", "event_id")) |>
      dplyr::compute(name = omopgenerics::uniqueTableName(prefix = prefix))

    # keep only people with a Life birth event
    lbEvent <- events |>
      dplyr::filter(.data$category == "LB") |>
      dplyr::distinct(.data$person_id)
    events <- events |>
      dplyr::inner_join(lbEvent, by = "person_id") |>
      dplyr::compute(name = omopgenerics::uniqueTableName(prefix = prefix))

    # calculate new first outcome event
    firstOutcomeEvent <- getFirstOutcomeEvent(
      events = events, outcomeCategory = "LB", prefix = prefix
    )
  }

  omopgenerics::dropSourceTable(cdm = cdm, name = dplyr::starts_with(prefix))
}
populateStillbirth <- function(cdm, nms) {
  prefix <- omopgenerics::tmpPrefix()

  # SB events
  sbEvent <- cdm[[nms$pregnancy_events]] |>
    dplyr::filter(.data$category == "SB") |>
    dplyr::distinct(.data$person_id)

  categories <- unique(c("AGP", "PCONF", outcome_limit$first_preg_category, outcome_limit$outcome_preg_category))

  events <- cdm[[nms$pregnancy_events]] |>
    dplyr::filter(.data$category %in% .env$categories) |>
    dplyr::inner_join(sbEvent, by = "person_id") |>
    dplyr::group_by(
      .data$person_id, .data$category, .data$event_date, .data$gest_value
    ) |>
    dplyr::filter(.data$event_id == min(.data$event_id, na.rm = TRUE)) |>
    dplyr::select("person_id", "event_id", "category", "event_date", "gest_value") |>
    dplyr::ungroup() |>
    dplyr::compute(name = omopgenerics::uniqueTableName(prefix = prefix))

  firstOutcomeEvent <- getFirstOutcomeEvent(
    events = events, outcomeCategory = "SB", prefix = prefix
  )

  while (!omopgenerics::isTableEmpty(table = firstOutcomeEvent)) {
    # get events that have a prior AGP or PCONF event in the prior 42 days
    firstOutcomeEventInv <- firstOutcomeEvent |>
      dplyr::select("person_id", "event_id", "outcome_date" = "event_date") |>
      dplyr::inner_join(
        events |>
          dplyr::filter(.data$category %in% c("AGP", "PCONF")) |>
          dplyr::select("person_id", "event_date"),
        by = "person_id"
      ) |>
      dplyr::mutate(days = clock::date_count_between(
        start = .data$event_date,
        end = .data$outcome_date,
        precision = "day"
      ) + 1) |>
      dplyr::filter(.data$days > 0 & .data$days <= 42) |>
      dplyr::distinct(.data$person_id, .data$event_id) |>
      dplyr::compute(name = omopgenerics::uniqueTableName(prefix = prefix))

    invalidOutcomes <- getTemporalInvalidOutcomes(
      cdm = cdm, nms = nms, firstOutcomeEvent = firstOutcomeEvent, prefix = prefix
    )

    tempValidOutcomes <- firstOutcomeEvent |>
      dplyr::anti_join(invalidOutcomes, by = c("person_id", "event_id")) |>
      dplyr::anti_join(firstOutcomeEventInv, by = c("person_id", "event_id")) |>
      dplyr::compute(name = omopgenerics::uniqueTableName(prefix = prefix))

    cdm <- appendValidOutcomes(cdm = cdm, nms = nms, x = tempValidOutcomes)

    events <- dropCurrentOutcomeEvents(
      events = events,
      firstOutcomeEvent = firstOutcomeEvent,
      outcomeCategory = "SB",
      prefix = prefix
    )

    firstOutcomeEvent <- getFirstOutcomeEvent(
      events = events, outcomeCategory = "SB", prefix = prefix
    )
  }

  omopgenerics::dropSourceTable(cdm = cdm, name = dplyr::starts_with(prefix))
}
populateEctropic <- function(cdm, nms) {
  prefix <- omopgenerics::tmpPrefix()

  # ECT events
  ectEvent <- cdm[[nms$pregnancy_events]] |>
    dplyr::filter(.data$category == "ECT") |>
    dplyr::distinct(.data$person_id)

  categories <- unique(c("AGP", "PCONF", "ECT_SURG1", "ECT_SURG2", "MTX",
                         outcome_limit$first_preg_category,
                         outcome_limit$outcome_preg_category))

  # events of interest
  events <- cdm[[nms$pregnancy_events]] |>
    dplyr::inner_join(ectEvent, by = "person_id") |>
    dplyr::filter(.data$category %in% .env$categories) |>
    dplyr::group_by(
      .data$person_id, .data$category, .data$event_date, .data$gest_value
    ) |>
    dplyr::filter(.data$event_id == min(.data$event_id, na.rm = TRUE)) |>
    dplyr::select("person_id", "event_id", "category", "event_date", "gest_value") |>
    dplyr::ungroup() |>
    dplyr::compute(name = omopgenerics::uniqueTableName(prefix = prefix))

  # first outcome
  firstOutcomeEvent <- getFirstOutcomeEvent(
    events = events, outcomeCategory = "ECT", prefix = prefix
  )

  while (!omopgenerics::isTableEmpty(table = firstOutcomeEvent)) {
    # ECT has surgery with "ECT_SURG1", "ECT_SURG2", "MTX"
    firstOutcomeEventSurg1 <- firstOutcomeEvent |>
      dplyr::select("person_id", "event_id", "event_date") |>
      dplyr::inner_join(
        events |>
          dplyr::filter(.data$category %in% c("ECT_SURG1", "ECT_SURG2", "MTX")) |>
          dplyr::select("person_id", "episode_end_date_revised" = "event_date"),
        by = "person_id"
      ) |>
      dplyr::mutate(date_difference = clock::date_count_between(
        start = .data$event_date,
        end = .data$episode_end_date_revised,
        precision = "day"
      )) |>
      dplyr::filter(.data$date_difference >= 0 & .data$date_difference <= 14) |>
      dplyr::group_by(.data$person_id, .data$event_id) |>
      dplyr::arrange(dplyr::desc(.data$episode_end_date_revised)) |>
      dplyr::filter(dplyr::row_number() == 1) |>
      dplyr::ungroup() |>
      dplyr::select(!"event_date") |>
      dplyr::compute(name = omopgenerics::uniqueTableName(prefix = prefix))

    # ECT has surgery with "ECT_SURG1", "MTX"
    firstOutcomeEventSurg2 <- firstOutcomeEvent |>
      dplyr::select("person_id", "event_id", "event_date") |>
      dplyr::inner_join(
        events |>
          dplyr::filter(.data$category %in% c("ECT_SURG1", "MTX")) |>
          dplyr::select("person_id", "episode_end_date_revised" = "event_date"),
        by = "person_id"
      ) |>
      dplyr::mutate(date_difference = clock::date_count_between(
        start = .data$event_date,
        end = .data$episode_end_date_revised,
        precision = "day"
      )) |>
      dplyr::filter(.data$date_difference >= 0 & .data$date_difference <= 14) |>
      dplyr::group_by(.data$person_id, .data$event_id) |>
      dplyr::arrange(dplyr::desc(.data$episode_end_date_revised)) |>
      dplyr::filter(dplyr::row_number() == 1) |>
      dplyr::ungroup() |>
      dplyr::select(!"event_date") |>
      dplyr::compute(name = omopgenerics::uniqueTableName(prefix = prefix))

    cdm[[nms$pregnancy_events]] <- cdm[[nms$pregnancy_events]] |>
      dplyr::left_join(
        firstOutcomeEventSurg2 |>
          dplyr::select(
            "person_id", "event_id",
            "episode_end_date_revised"
          ),
        by = c("person_id", "event_id")
      ) |>
      dplyr::mutate(event_date = dplyr::coalesce(
        .data$episode_end_date_revised, .data$event_date
      )) |>
      dplyr::select(!"episode_end_date_revised") |>
      dplyr::compute(name = nms$pregnancy_events)

    events <- events |>
      dplyr::left_join(
        firstOutcomeEventSurg2 |>
          dplyr::select(
            "person_id", "event_id",
            "episode_end_date_revised"
          ),
        by = c("person_id", "event_id")
      ) |>
      dplyr::mutate(event_date = dplyr::coalesce(
        .data$episode_end_date_revised, .data$event_date
      )) |>
      dplyr::select(!"episode_end_date_revised") |>
      dplyr::compute(name = omopgenerics::uniqueTableName(prefix = prefix))

    firstOutcomeEventInv <- firstOutcomeEvent |>
      dplyr::select("person_id", "event_id", "outcome_date" = "event_date") |>
      dplyr::inner_join(
        cdm[[nms$pregnancy_events]] |>
          dplyr::filter(.data$category %in% c("AGP", "PCONF")) |>
          dplyr::select("person_id", "event_date"),
        by = "person_id"
      ) |>
      dplyr::mutate(days = clock::date_count_between(
        start = .data$outcome_date,
        end = .data$event_date,
        precision = "day"
      )) |>
      dplyr::filter(.data$days > 0 & .data$days <= 42) |>
      dplyr::distinct(.data$person_id, .data$event_id) |>
      dplyr::compute(name = omopgenerics::uniqueTableName(prefix = prefix))

    invalidOutcomes <- getTemporalInvalidOutcomes(
      cdm = cdm, nms = nms, firstOutcomeEvent = firstOutcomeEvent, prefix = prefix
    )

    tempValidOutcomes <- firstOutcomeEvent |>
      dplyr::anti_join(invalidOutcomes, by = c("person_id", "event_id")) |>
      dplyr::anti_join(firstOutcomeEventInv, by = c("person_id", "event_id")) |>
      dplyr::semi_join(firstOutcomeEventSurg1, by = c("person_id", "event_id")) |>
      dplyr::compute(name = omopgenerics::uniqueTableName(prefix = prefix))

    cdm <- appendValidOutcomes(cdm = cdm, nms = nms, x = tempValidOutcomes)

    events <- dropCurrentOutcomeEvents(
      events = events,
      firstOutcomeEvent = firstOutcomeEvent,
      outcomeCategory = "ECT",
      prefix = prefix
    )

    firstOutcomeEvent <- getFirstOutcomeEvent(
      events = events, outcomeCategory = "ECT", prefix = prefix
    )
  }

  omopgenerics::dropSourceTable(cdm = cdm, name = dplyr::starts_with(prefix))
}
