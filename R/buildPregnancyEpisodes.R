
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
      i = "If you want to overwrite it please run {.code cdm$pregnancy_episode <- NULL} and try again."
    ))
  }

  # temp prefix
  prefix <- omopgenerics::tmpPrefix()

  # get raw events
  raw <- omopgenerics::uniqueTableName(prefix = prefix)
  cdm <- rawEvents(cdm = cdm, name = raw)

}
rawEvents <- function(cdm, name) {
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
      value_as_number = as.integer(NA)
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
      value_as_number = as.integer(NA)
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
      "concept_id" = "observation_concept_id",
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
        cdm$observation |>
          dplyr::select(
            "person_id",
            "concept_id" = "observation_concept_id",
            "start_date" = "observation_date",
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
  cdm[[name]] <- co |>
    dplyr::union_all(po) |>
    dplyr::union_all(ob) |>
    dplyr::union_all(me) |>
    dplyr::compute(name = name)

  # delete temp tables
  omopgenerics::dropSourceTable(cdm = cdm, name = dplyr::starts_with(prefix))
}
