
#' Build the `pregnancy_episodes` extension table
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

  # populate induced abortion events
  cdm <- populateAbortion(cdm = cdm, nms = nms)

  # populate spontaneous abortion events
  cdm <- populateSpontaneousAbortion(cdm = cdm, nms = nms)

  # populate delivery events
  cdm <- populateDelivery(cdm = cdm, nms = nms)

  # build final pregnancy episodes table
  cdm <- buildPregnancyEpisodesTable(cdm = cdm, nms = nms)

  # drop extra tables
  cdm <- omopgenerics::dropSourceTable(cdm = cdm, name = dplyr::starts_with(prefix))

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
updateEventDates <- function(x, revisions, name) {
  x |>
    dplyr::left_join(
      revisions |>
        dplyr::select("person_id", "event_id", "episode_end_date_revised"),
      by = c("person_id", "event_id")
    ) |>
    dplyr::mutate(event_date = dplyr::coalesce(
      .data$episode_end_date_revised, .data$event_date
    )) |>
    dplyr::select(!"episode_end_date_revised") |>
    dplyr::compute(name = name)
}
getOutcomeInvalidationEvents <- function(events, firstOutcomeEvent, prefix) {
  firstOutcomeEvent |>
    dplyr::select("person_id", "event_id", "outcome_date" = "event_date") |>
    dplyr::inner_join(
      events |>
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
          has_diab = any(.data$category == "DIAB", na.rm = TRUE)
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
getOutcomeEvents <- function(cdm, nms, outcomeCategory, extraCategories = character()) {
  prefix <- omopgenerics::tmpPrefix()
  categories <- unique(c(
    extraCategories,
    outcome_limit$first_preg_category,
    outcome_limit$outcome_preg_category
  ))
  outcomePeople <- cdm[[nms$pregnancy_events]] |>
    dplyr::filter(.data$category == .env$outcomeCategory) |>
    dplyr::distinct(.data$person_id)

  cdm[[nms$pregnancy_events]] |>
    dplyr::filter(.data$category %in% .env$categories) |>
    dplyr::inner_join(outcomePeople, by = "person_id") |>
    dplyr::group_by(
      .data$person_id, .data$category, .data$event_date, .data$gest_value
    ) |>
    dplyr::filter(.data$event_id == min(.data$event_id, na.rm = TRUE)) |>
    dplyr::select("person_id", "event_id", "category", "event_date", "gest_value") |>
    dplyr::ungroup() |>
    dplyr::compute(name = omopgenerics::uniqueTableName(prefix = prefix))
}
populateLifeBirth <- function(cdm, nms) {
  prefix <- omopgenerics::tmpPrefix()
  events <- getOutcomeEvents(cdm = cdm, nms = nms, outcomeCategory = "LB")

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

    events <- events |>
      dplyr::semi_join(
        events |>
          dplyr::filter(.data$category == "LB") |>
          dplyr::distinct(.data$person_id),
        by = "person_id"
      ) |>
      dplyr::compute(name = omopgenerics::uniqueTableName(prefix = prefix))

    # calculate new first outcome event
    firstOutcomeEvent <- getFirstOutcomeEvent(
      events = events, outcomeCategory = "LB", prefix = prefix
    )
  }

  omopgenerics::dropSourceTable(cdm = cdm, name = dplyr::starts_with(prefix))
  cdm
}
populateStillbirth <- function(cdm, nms) {
  populateOutcomeLoop(cdm = cdm, nms = nms, outcomeCategory = "SB")
}
populateEctropic <- function(cdm, nms) {
  populateOutcomeLoop(
    cdm = cdm,
    nms = nms,
    outcomeCategory = "ECT",
    extraCategories = c("ECT_SURG1", "ECT_SURG2", "MTX"),
    revisionCategories = c("ECT_SURG1", "MTX"),
    validationCategories = c("ECT_SURG1", "ECT_SURG2", "MTX"),
    requireValidationCategory = TRUE
  )
}
populateOutcomeLoop <- function(cdm,
                                nms,
                                outcomeCategory,
                                extraCategories = character(),
                                revisionCategories = character(),
                                validationCategories = character(),
                                requireValidationCategory = FALSE) {
  prefix <- omopgenerics::tmpPrefix()

  events <- getOutcomeEvents(
    cdm = cdm,
    nms = nms,
    outcomeCategory = outcomeCategory,
    extraCategories = c("AGP", "PCONF", extraCategories)
  )

  firstOutcomeEvent <- getFirstOutcomeEvent(
    events = events, outcomeCategory = outcomeCategory, prefix = prefix
  )

  while (!omopgenerics::isTableEmpty(table = firstOutcomeEvent)) {
    validatedEvents <- firstOutcomeEvent

    if (length(revisionCategories) > 0) {
      revisedOutcomeDates <- firstOutcomeEvent |>
        dplyr::select("person_id", "event_id", "event_date") |>
        dplyr::inner_join(
          events |>
            dplyr::filter(.data$category %in% .env$revisionCategories) |>
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

      cdm[[nms$pregnancy_events]] <- updateEventDates(
        x = cdm[[nms$pregnancy_events]],
        revisions = revisedOutcomeDates,
        name = nms$pregnancy_events
      )
      events <- updateEventDates(
        x = events,
        revisions = revisedOutcomeDates,
        name = omopgenerics::uniqueTableName(prefix = prefix)
      )
      firstOutcomeEvent <- updateEventDates(
        x = firstOutcomeEvent,
        revisions = revisedOutcomeDates,
        name = omopgenerics::uniqueTableName(prefix = prefix)
      )
    }

    firstOutcomeEventInv <- getOutcomeInvalidationEvents(
      events = cdm[[nms$pregnancy_events]],
      firstOutcomeEvent = firstOutcomeEvent,
      prefix = prefix
    )

    invalidOutcomes <- getTemporalInvalidOutcomes(
      cdm = cdm, nms = nms, firstOutcomeEvent = firstOutcomeEvent, prefix = prefix
    )

    if (length(validationCategories) > 0) {
      validatedEvents <- firstOutcomeEvent |>
        dplyr::select("person_id", "event_id", "event_date") |>
        dplyr::inner_join(
          events |>
            dplyr::filter(.data$category %in% .env$validationCategories) |>
            dplyr::select("person_id", "validation_event_date" = "event_date"),
          by = "person_id"
        ) |>
        dplyr::mutate(days = clock::date_count_between(
          start = .data$event_date,
          end = .data$validation_event_date,
          precision = "day"
        )) |>
        dplyr::filter(.data$days >= 0 & .data$days <= 14) |>
        dplyr::group_by(.data$person_id, .data$event_id) |>
        dplyr::arrange(dplyr::desc(.data$validation_event_date)) |>
        dplyr::filter(dplyr::row_number() == 1) |>
        dplyr::ungroup() |>
        dplyr::select("person_id", "event_id") |>
        dplyr::compute(name = omopgenerics::uniqueTableName(prefix = prefix))
    }

    tempValidOutcomes <- firstOutcomeEvent |>
      dplyr::anti_join(invalidOutcomes, by = c("person_id", "event_id")) |>
      dplyr::anti_join(firstOutcomeEventInv, by = c("person_id", "event_id"))

    if (requireValidationCategory) {
      tempValidOutcomes <- tempValidOutcomes |>
        dplyr::semi_join(validatedEvents, by = c("person_id", "event_id"))
    }

    tempValidOutcomes <- tempValidOutcomes |>
      dplyr::compute(name = omopgenerics::uniqueTableName(prefix = prefix))

    cdm <- appendValidOutcomes(cdm = cdm, nms = nms, x = tempValidOutcomes)

    events <- dropCurrentOutcomeEvents(
      events = events,
      firstOutcomeEvent = firstOutcomeEvent,
      outcomeCategory = outcomeCategory,
      prefix = prefix
    )

    firstOutcomeEvent <- getFirstOutcomeEvent(
      events = events, outcomeCategory = outcomeCategory, prefix = prefix
    )
  }

  omopgenerics::dropSourceTable(cdm = cdm, name = dplyr::starts_with(prefix))
  cdm
}
populateAbortion <- function(cdm, nms) {
  populateOutcomeLoop(
    cdm = cdm,
    nms = nms,
    outcomeCategory = "AB",
    revisionCategories = c("SA", "AB")
  )
}
populateSpontaneousAbortion <- function(cdm, nms) {
  populateOutcomeLoop(
    cdm = cdm,
    nms = nms,
    outcomeCategory = "SA",
    revisionCategories = c("SA", "AB")
  )
}
populateDelivery <- function(cdm, nms) {
  populateOutcomeLoop(
    cdm = cdm,
    nms = nms,
    outcomeCategory = "DELIV"
  )
}
collectStartCandidates <- function(...) {
  tables <- list(...)
  tables <- purrr::compact(tables)
  if (length(tables) == 0) {
    return(NULL)
  }
  purrr::reduce(tables, dplyr::union_all)
}
buildOutcomeWindows <- function(cdm, nms, prefix) {
  outcomeEvents <- cdm[[nms$pregnancy_events]] |>
    dplyr::inner_join(cdm[[nms$valid_outcomes]], by = c("person_id", "event_id")) |>
    dplyr::select(
      "person_id", "event_id",
      "outcome_category" = "category",
      "episode_end_date" = "event_date"
    ) |>
    dplyr::group_by(.data$person_id) |>
    dplyr::arrange(.data$episode_end_date, .data$event_id) |>
    dplyr::mutate(
      prior_outcome_date = dplyr::lag(.data$episode_end_date),
      prior_category = dplyr::lag(.data$outcome_category)
    ) |>
    dplyr::ungroup() |>
    dplyr::inner_join(
      cdm[[nms$term_durations]],
      by = c("outcome_category" = "category")
    ) |>
    dplyr::mutate(
      min_start_date = as.Date(clock::add_days(
        .data$episode_end_date, -.data$max_term
      )),
      retry_start_date = as.Date(clock::add_days(
        .data$prior_outcome_date, .data$retry
      )),
      max_start_date = as.Date(clock::add_days(
        .data$episode_end_date, -.data$min_term
      )),
      lower_bound = dplyr::case_when(
        is.na(.data$prior_outcome_date) ~ .data$min_start_date,
        .data$retry_start_date > .data$min_start_date ~ .data$retry_start_date,
        .default = .data$min_start_date
      )
    ) |>
    dplyr::compute(name = omopgenerics::uniqueTableName(prefix = prefix))
  outcomeEvents
}
pickCandidateStarts <- function(outcomeWindows,
                                pregnancyEvents,
                                categories,
                                startExpr,
                                startCategoryExpr,
                                rank,
                                ascending = FALSE,
                                prefix) {
  if (!rlang::is_quosure(startExpr)) {
    startExpr <- rlang::new_quosure(startExpr, env = parent.frame())
  }
  if (!rlang::is_quosure(startCategoryExpr)) {
    startCategoryExpr <- rlang::new_quosure(startCategoryExpr, env = parent.frame())
  }

  candidates <- outcomeWindows |>
    dplyr::inner_join(
      pregnancyEvents |>
        dplyr::filter(.data$category %in% .env$categories) |>
        dplyr::select(
          "person_id",
          "candidate_category" = "category",
          "candidate_event_date" = "event_date",
          "candidate_gest_value" = "gest_value"
        ),
      by = "person_id"
    ) |>
    dplyr::mutate(
      episode_start_date = !!startExpr,
      start_category = !!startCategoryExpr,
      date_rank = as.integer(.env$rank)
    ) |>
    dplyr::filter(
      !is.na(.data$episode_start_date) &
        .data$episode_start_date >= .data$lower_bound &
        .data$episode_start_date <= .data$max_start_date
    ) |>
    dplyr::compute(name = omopgenerics::uniqueTableName(prefix = prefix))

  chosenDates <- candidates |>
    dplyr::group_by(.data$person_id, .data$event_id) |>
    dplyr::summarise(
      chosen_candidate_event_date = if (ascending) {
        min(.data$candidate_event_date, na.rm = TRUE)
      } else {
        max(.data$candidate_event_date, na.rm = TRUE)
      },
      .groups = "drop"
    ) |>
    dplyr::compute(name = omopgenerics::uniqueTableName(prefix = prefix))

  candidates |>
    dplyr::inner_join(
      chosenDates,
      by = c("person_id", "event_id")
    ) |>
    dplyr::filter(.data$candidate_event_date == .data$chosen_candidate_event_date) |>
    dplyr::group_by(.data$person_id, .data$event_id) |>
    dplyr::summarise(
      episode_start_date = min(.data$episode_start_date, na.rm = TRUE),
      start_category = min(.data$start_category, na.rm = TRUE),
      date_rank = min(.data$date_rank, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::select(
      "person_id", "event_id",
      "episode_start_date", "start_category", "date_rank"
    ) |>
    dplyr::compute(name = omopgenerics::uniqueTableName(prefix = prefix))
}
buildDefaultStarts <- function(outcomeWindows, pregnancyEvents, cdm, nms, prefix) {
  outcomeWindows |>
    dplyr::inner_join(
      pregnancyEvents |>
        dplyr::select(
          "person_id",
          "candidate_category" = "category",
          "candidate_event_date" = "event_date"
        ),
      by = "person_id"
    ) |>
    dplyr::mutate(
      evidence_lower_bound = dplyr::case_when(
        is.na(.data$prior_outcome_date) ~ .data$min_start_date,
        .data$prior_outcome_date > .data$min_start_date ~ .data$prior_outcome_date,
        .default = .data$min_start_date
      ),
      evidence_upper_bound = as.Date(clock::add_days(.data$episode_end_date, 30L))
    ) |>
    dplyr::filter(
      .data$candidate_event_date >= .data$evidence_lower_bound &
        .data$candidate_event_date <= .data$evidence_upper_bound
    ) |>
    dplyr::group_by(.data$person_id, .data$event_id, .data$outcome_category,
                    .data$episode_end_date, .data$prior_outcome_date, .data$retry,
                    .data$min_start_date) |>
    dplyr::summarise(
      pre_count = as.integer(sum(.data$candidate_category == "PREM", na.rm = TRUE)),
      full_count = as.integer(sum(.data$candidate_category == "FT", na.rm = TRUE)),
      .groups = "drop"
    ) |>
    dplyr::inner_join(
      cdm[[nms$gest_est]] |>
        dplyr::select(
          "outcome_category" = "category",
          preterm, fullterm, nodata
        ),
      by = "outcome_category"
    ) |>
    dplyr::mutate(
      gest_days = dplyr::case_when(
        .data$pre_count > 0L ~ .data$preterm,
        .data$full_count > 0L ~ .data$fullterm,
        .default = .data$nodata
      ),
      raw_start_date = as.Date(clock::add_days(.data$episode_end_date, -.data$gest_days)),
      retry_start_date = as.Date(clock::add_days(.data$prior_outcome_date, .data$retry)),
      episode_start_date = dplyr::case_when(
        is.na(.data$prior_outcome_date) ~ .data$raw_start_date,
        .data$retry_start_date > .data$raw_start_date ~ .data$retry_start_date,
        .default = .data$raw_start_date
      ),
      start_category = dplyr::case_when(
        .data$pre_count > 0L ~ "PREM",
        .default = "DEFAULT"
      ),
      date_rank = 99L
    ) |>
    dplyr::select(
      "person_id", "event_id", "episode_start_date", "start_category", "date_rank"
    ) |>
    dplyr::compute(name = omopgenerics::uniqueTableName(prefix = prefix))
}
buildPregnancyEpisodesTable <- function(cdm, nms) {
  prefix <- omopgenerics::tmpPrefix()
  outcomeWindows <- buildOutcomeWindows(cdm = cdm, nms = nms, prefix = prefix)
  events <- cdm[[nms$pregnancy_events]]

  lmpStarts <- pickCandidateStarts(
    outcomeWindows = outcomeWindows,
    pregnancyEvents = events,
    categories = "LMP",
    startExpr = rlang::expr(.data$candidate_event_date),
    startCategoryExpr = rlang::expr(.data$candidate_category),
    rank = 1L,
    ascending = FALSE,
    prefix = prefix
  )
  gestStarts <- pickCandidateStarts(
    outcomeWindows = outcomeWindows,
    pregnancyEvents = events,
    categories = "GEST",
    startExpr = rlang::expr(as.Date(clock::add_days(
      .data$candidate_event_date, -.data$candidate_gest_value + 1L
    ))),
    startCategoryExpr = rlang::expr(.data$candidate_category),
    rank = 2L,
    ascending = FALSE,
    prefix = prefix
  )
  ovulStarts <- pickCandidateStarts(
    outcomeWindows = outcomeWindows,
    pregnancyEvents = events,
    categories = "OVUL",
    startExpr = rlang::expr(as.Date(clock::add_days(.data$candidate_event_date, -13L))),
    startCategoryExpr = rlang::expr(.data$candidate_category),
    rank = 3L,
    ascending = TRUE,
    prefix = prefix
  )
  ovul2Starts <- pickCandidateStarts(
    outcomeWindows = outcomeWindows,
    pregnancyEvents = events,
    categories = "OVUL2",
    startExpr = rlang::expr(as.Date(clock::add_days(.data$candidate_event_date, -13L))),
    startCategoryExpr = rlang::expr(.data$candidate_category),
    rank = 4L,
    ascending = TRUE,
    prefix = prefix
  )
  nulsStarts <- pickCandidateStarts(
    outcomeWindows = outcomeWindows,
    pregnancyEvents = events,
    categories = "NULS",
    startExpr = rlang::expr(as.Date(clock::add_days(.data$candidate_event_date, -89L))),
    startCategoryExpr = rlang::expr(.data$candidate_category),
    rank = 6L,
    ascending = TRUE,
    prefix = prefix
  )
  afpStarts <- pickCandidateStarts(
    outcomeWindows = outcomeWindows,
    pregnancyEvents = events,
    categories = "AFP",
    startExpr = rlang::expr(as.Date(clock::add_days(.data$candidate_event_date, -123L))),
    startCategoryExpr = rlang::expr(.data$candidate_category),
    rank = 7L,
    ascending = TRUE,
    prefix = prefix
  )
  amenStarts <- pickCandidateStarts(
    outcomeWindows = outcomeWindows,
    pregnancyEvents = events,
    categories = "AMEN",
    startExpr = rlang::expr(as.Date(clock::add_days(.data$candidate_event_date, -55L))),
    startCategoryExpr = rlang::expr(.data$candidate_category),
    rank = 80L,
    ascending = TRUE,
    prefix = prefix
  )
  upStarts <- pickCandidateStarts(
    outcomeWindows = outcomeWindows,
    pregnancyEvents = events,
    categories = "UP",
    startExpr = rlang::expr(as.Date(clock::add_days(.data$candidate_event_date, -55L))),
    startCategoryExpr = rlang::expr(.data$candidate_category),
    rank = 90L,
    ascending = TRUE,
    prefix = prefix
  )
  defaultStarts <- buildDefaultStarts(
    outcomeWindows = outcomeWindows,
    pregnancyEvents = events,
    cdm = cdm,
    nms = nms,
    prefix = prefix
  )
  pconfStarts <- pickCandidateStarts(
    outcomeWindows = outcomeWindows,
    pregnancyEvents = events,
    categories = c("PCONF", "AGP", "PCOMP", "TA"),
    startExpr = rlang::expr(as.Date(clock::add_days(.data$candidate_event_date, -55L))),
    startCategoryExpr = rlang::expr("PUSHBACK"),
    rank = 79L,
    ascending = TRUE,
    prefix = prefix
  )
  contraStarts <- pickCandidateStarts(
    outcomeWindows = outcomeWindows,
    pregnancyEvents = events,
    categories = "CONTRA",
    startExpr = rlang::expr(.data$candidate_event_date),
    startCategoryExpr = rlang::expr("CONTRA"),
    rank = 98L,
    ascending = FALSE,
    prefix = prefix
  )

  allStarts <- collectStartCandidates(
    lmpStarts, gestStarts, ovulStarts, ovul2Starts, nulsStarts,
    afpStarts, amenStarts, upStarts, defaultStarts
  ) |>
    dplyr::compute(name = omopgenerics::uniqueTableName(prefix = prefix))

  bestStarts <- allStarts |>
    dplyr::group_by(.data$person_id, .data$event_id) |>
    dplyr::arrange(.data$date_rank) |>
    dplyr::filter(dplyr::row_number() == 1) |>
    dplyr::ungroup() |>
    dplyr::compute(name = omopgenerics::uniqueTableName(prefix = prefix))

  newStarts <- bestStarts |>
    dplyr::filter(.data$date_rank > 7L) |>
    dplyr::left_join(
      pconfStarts |>
        dplyr::rename(
          "pconf_start_date" = "episode_start_date",
          "pconf_category" = "start_category"
        ),
      by = c("person_id", "event_id")
    ) |>
    dplyr::left_join(
      contraStarts |>
        dplyr::rename(
          "contra_start_date" = "episode_start_date",
          "contra_category" = "start_category"
        ),
      by = c("person_id", "event_id")
    ) |>
    dplyr::mutate(
      episode_start_date = dplyr::case_when(
        is.na(.data$pconf_start_date) & is.na(.data$contra_start_date) ~ .data$episode_start_date,
        is.na(.data$contra_start_date) & .data$pconf_start_date < .data$episode_start_date ~ .data$pconf_start_date,
        is.na(.data$pconf_start_date) & .data$contra_start_date > .data$episode_start_date ~ .data$contra_start_date,
        !is.na(.data$pconf_start_date) & !is.na(.data$contra_start_date) &
          .data$pconf_start_date < .data$episode_start_date &
          .data$contra_start_date < .data$pconf_start_date ~ .data$pconf_start_date,
        !is.na(.data$pconf_start_date) & !is.na(.data$contra_start_date) &
          .data$pconf_start_date < .data$episode_start_date &
          .data$contra_start_date > .data$pconf_start_date ~ .data$contra_start_date,
        !is.na(.data$pconf_start_date) & !is.na(.data$contra_start_date) &
          .data$contra_start_date > .data$episode_start_date &
          .data$pconf_start_date > .data$episode_start_date ~ .data$contra_start_date,
        .default = .data$episode_start_date
      ),
      start_category = paste0(.data$start_category, ":REFINED"),
      date_rank = .data$date_rank - 1L
    ) |>
    dplyr::select("person_id", "event_id", "episode_start_date", "start_category", "date_rank") |>
    dplyr::compute(name = omopgenerics::uniqueTableName(prefix = prefix))

  pregnancyEpisodes <- collectStartCandidates(allStarts, newStarts) |>
    dplyr::inner_join(
      outcomeWindows |>
        dplyr::select("person_id", "event_id", "episode_end_date", "outcome_category"),
      by = c("person_id", "event_id")
    ) |>
    dplyr::compute(name = omopgenerics::uniqueTableName(prefix = prefix))

  pregnancyEpisodesObs <- pregnancyEpisodes |>
    dplyr::group_by(.data$person_id, .data$episode_end_date) |>
    dplyr::arrange(.data$date_rank) |>
    dplyr::filter(dplyr::row_number() == 1) |>
    dplyr::ungroup() |>
    dplyr::inner_join(
      cdm$observation_period |>
        dplyr::select(
          "person_id",
          "observation_period_start_date",
          "observation_period_end_date"
        ),
      by = "person_id"
    ) |>
    dplyr::filter(
      .data$episode_end_date >= .data$observation_period_start_date &
        .data$episode_end_date <= .data$observation_period_end_date &
        .data$episode_start_date >= .data$observation_period_start_date
    ) |>
    PatientProfiles::addDemographicsQuery(
      age = TRUE,
      sex = TRUE,
      priorObservation = FALSE,
      futureObservation = FALSE,
      indexDate = "episode_start_date"
    ) |>
    dplyr::filter(.data$sex == "Female", .data$age >= 12, .data$age <= 55) |>
    dplyr::select(
      "person_id", "episode_start_date", "episode_end_date",
      "start_category", "outcome_category"
    ) |>
    dplyr::group_by(.data$person_id) |>
    dplyr::arrange(.data$episode_start_date) |>
    dplyr::mutate(rn = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::compute(name = omopgenerics::uniqueTableName(prefix = prefix))

  pregnancyEpisodesTwoRec <- pregnancyEpisodesObs |>
    dplyr::inner_join(
      cdm[[nms$term_durations]] |>
        dplyr::select(
          "outcome_category" = "category",
          "max_term"
        ),
      by = "outcome_category"
    ) |>
    dplyr::inner_join(
      cdm[[nms$pregnancy_events]] |>
        dplyr::select("person_id", "event_id", "event_date"),
      by = "person_id"
    ) |>
    dplyr::mutate(min_event_date = as.Date(clock::add_days(
      .data$episode_end_date, -.data$max_term + 1L
    ))) |>
    dplyr::filter(
      .data$event_date >= .data$min_event_date &
        .data$event_date <= .data$episode_end_date
    ) |>
    dplyr::group_by(
      .data$person_id, .data$episode_start_date, .data$episode_end_date,
      .data$start_category, .data$outcome_category, .data$rn
    ) |>
    dplyr::summarise(tot_events = dplyr::n_distinct(.data$event_id), .groups = "drop") |>
    dplyr::filter(.data$tot_events >= 2L) |>
    dplyr::compute(name = omopgenerics::uniqueTableName(prefix = prefix))

  cdm$pregnancy_episodes <- pregnancyEpisodesTwoRec |>
    dplyr::mutate(
      start_method = .data$start_category,
      original_outcome = .data$outcome_category,
      episode = as.integer(.data$rn),
      outcome = dplyr::case_when(
        .data$outcome_category %in% c("AB", "SA") ~ "SA/AB",
        .data$outcome_category %in% c("DELIV", "LB") ~ "LB/DELIV",
        .data$outcome_category == "SB" &
          (as.integer(clock::date_count_between(
            .data$episode_start_date, .data$episode_end_date, precision = "day"
          )) + 1L) < 140L ~ "SA/AB",
        .default = .data$outcome_category
      ),
      episode_length = as.integer(clock::date_count_between(
        .data$episode_start_date, .data$episode_end_date, precision = "day"
      )) + 1L
    ) |>
    dplyr::select(
      "person_id", "episode_start_date", "episode_end_date",
      "start_method", "original_outcome", "episode", "outcome",
      "episode_length"
    ) |>
    dplyr::compute(name = "pregnancy_episodes")

  omopgenerics::dropSourceTable(cdm = cdm, name = dplyr::starts_with(prefix))
  cdm
}
