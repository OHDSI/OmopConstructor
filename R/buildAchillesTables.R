
#' Build achilles tables using the data from the cdm object.
#'
#' @param cdm A cdm_reference object.
#' @param achillesId A vector of achilles ids. If NULL default analysis will be
#' used. Check the different analysis in `achillesAnalisisDetails`.
#'
#' @return The cdm_reference object with the achilles tables populated.
#' @export
#'
#' @examples
#' \donttest{
#' dbName <- "GiBleed"
#' CDMConnector::requireEunomia(dbName)
#' con <- duckdb::dbConnect(duckdb::duckdb(), CDMConnector::eunomiaDir(dbName))
#' cdm <- CDMConnector::cdmFromCon(
#'   con = con, cdmSchema = "main", writeSchema = "main"
#' )
#'
#' cdm <- buildAchillesTables(cdm = cdm)
#'
#' }
buildAchillesTables <- function(cdm,
                                achillesId = NULL) {
  # initial check
  cdm <- omopgenerics::validateCdmArgument(cdm = cdm)
  achillesId <- validateAchillesId(achillesId = achillesId)

  # check existent tables
  cdm <- checkExistentAchillesTables(cdm = cdm)

  # remove repeated results
  cdm <- removeRepeatedIds(cdm = cdm, achillesId = achillesId)

  # create new analysis
  len <- sprintf("%i", length(achillesId))
  for (k in seq_along(achillesId)) {
    id <- achillesId[k]
    nm <- OmopConstructor::achillesAnalisisDetails |>
      dplyr::filter(.data$analysis_id == .env$id) |>
      dplyr::pull("analysis_name")
    kk <- sprintf("%i", k) |>
      stringr::str_pad(width = nchar(len), pad = " ")
    cli::cli_inform(c("i" = "{kk} of {len}: Get achilles result for {.pkg {nm}}."))
    cdm <- appendAchillesId(cdm, id)
  }

  # append to achilles_analysis
  name <- omopgenerics::uniqueTableName()
  cdm <- omopgenerics::insertTable(
    cdm = cdm,
    name = name,
    table = OmopConstructor::achillesAnalisisDetails |>
      dplyr::filter(.data$analysis_id %in% .env$achillesId)
  )
  cdm[["achilles_analysis"]] <- cdm[["achilles_analysis"]] |>
    dplyr::union_all(cdm[[name]]) |>
    dplyr::compute(name = "achilles_analysis")
  cdm <- omopgenerics::dropSourceTable(cdm = cdm, name = name)

  return(cdm)
}

validateAchillesId <- function(achillesId, call = parent.frame()) {
  # possible ids
  possibleIds <- achillesAnalisisInternal$analysis_id
  allIds <- OmopConstructor::achillesAnalisisDetails$analysis_id

  # default analysis_id
  if (is.null(achillesId)) {
    achillesId <- OmopConstructor::achillesAnalisisDetails |>
      dplyr::filter(.data$is_default == 1L) |>
      dplyr::pull("analysis_id")
  } else {
    achillesId <- as.integer(achillesId)
    omopgenerics::assertNumeric(achillesId, integerish = TRUE, unique = TRUE, call = call)
    notIds <- achillesId[!achillesId %in% allIds]
    if (length(noIds) > 0) {
      cli::cli_inform(c("!" = "{length(notIds)} analysis id{?s} {?is/are} not valid: {notIds}."))
    }
    notConfigured <- achillesId[!achillesId %in% possibleIds & achillesId %in% allIds]
    if (length(notConfigured) > 0) {
      cli::cli_inform(c("i" = "{length(notConfigured)} analysis id{?s} {?is/are} not configured: {notConfigured}."))
    }
  }

  achillesId[achillesId %in% possibleIds]
}
checkExistentAchillesTables <- function(cdm) {
  notPresent <- omopgenerics::achillesTables() |>
    purrr::keep(\(x) !x %in% names(cdm))
  if (length(notPresent) > 0) {
    possibleToRead <- notPresent[notPresent %in% omopgenerics::listSourceTables(cdm = cdm)]
    if (length(possibleToRead) > 0) {
      cli::cli_inform(c(i = "Reading tables from source: {.pkg {possibleToRead}}"))
      cdm <- omopgenerics::readSourceTable(cdm = cdm, name = possibleToRead)
    }
    needToCreate <- purrr::keep(notPresent, \(x) !x %in% names(cdm))
    if (length(needToCreate) > 0) {
      for (nm in needToCreate) {
        cli::cli_inform(c(i = "Creating empty {.pkg {nm}} table."))
        cdm <- omopgenerics::emptyAchillesTable(cdm = cdm, name = nm)
      }
    }
  }
  return(cdm)
}
removeRepeatedIds <- function(cdm, achillesId) {
  repeatedIds <- list(
    achilles_analysis = cdm[["achilles_analysis"]] |>
      dplyr::distinct(.data$analysis_id) |>
      dplyr::pull(),
    achilles_results = cdm[["achilles_results"]] |>
      dplyr::distinct(.data$analysis_id) |>
      dplyr::pull(),
    achilles_results_dist = cdm[["achilles_results_dist"]] |>
      dplyr::distinct(.data$analysis_id) |>
      dplyr::pull()
  ) |>
    purrr::map(\(x) x[x %in% achillesId]) |>
    purrr::compact()
  if (length(repeatedIds) > 0) {
    for (nm in names(repeatedIds)) {
      ids <- repeatedIds[[nm]]
      "Removing {length(ids)} repeated analysis ids from {.pkg {nm}}." |>
        rlang::set_names("!") |>
        cli::cli_inform()
      cdm[[nm]] <- cdm[[nm]] |>
        dplyr::filter(!.data$analysis_id %in% .env$ids) |>
        dplyr::compute(name = nm)
    }
  }
  return(cdm)
}
appendAchillesId <- function(cdm, id) {

  # get analysis results
  analysis <- achillesAnalisisInternal |>
    dplyr::filter(.data$analysis_id == .env$id)
  type <- analysis$type
  if (identical(type, "count")) {
    cdm <- getCounts(cdm, analysis)
  } else if (identical(type, "person_count")) {
    cdm <- getPersonCounts(cdm, analysis)
  }

  return(cdm)
}
groupBy <- function(analysis) {
  by <- analysis |>
    dplyr::select(dplyr::starts_with("stratum_")) |>
    as.list() |>
    unlist() |>
    purrr::keep(\(x) !is.na(x))
  names(by) <- stringr::str_remove(names(by), "_name$")
  return(by)
}
mutateColumns <- function(analysis) {
  mut <- analysis |>
    dplyr::select(dplyr::starts_with("stratum_")) |>
    as.list() |>
    purrr::keep(\(x) is.na(x))
  names(mut) <- stringr::str_remove(names(mut), "_name$")
  return(mut)
}
getCounts <- function(cdm,
                      analysis) {
  tableName <- snakecase::to_snake_case(analysis$table)
  by <- groupBy(analysis)
  mut <- mutateColumns(analysis)

  nm <- omopgenerics::uniqueTableName()
  res <- cdm[[tableName]] |>
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
    dplyr::summarise(count_value = as.integer(dplyr::n())) |>
    dplyr::mutate(!!!mut, analysis_id = !!analysis$analysis_id) |>
    dplyr::compute(name = nm)

  cdm[["achilles_results"]] <- cdm[["achilles_results"]] |>
    dplyr::union_all(res) |>
    dplyr::compute(name = "achilles_results")

  omopgenerics::dropSourceTable(cdm = cdm, name = nm)

  return(cdm)
}
getPersonCounts <- function(cdm,
                            analysis) {
  tableName <- snakecase::to_snake_case(analysis$table)
  by <- groupBy(analysis)
  mut <- mutateColumns(analysis)

  nm <- omopgenerics::uniqueTableName()
  res <- cdm[[tableName]] |>
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
    dplyr::summarise(count_value = as.integer(dplyr::n_distinct(.data$person_id))) |>
    dplyr::mutate(!!!mut, analysis_id = !!analysis$analysis_id) |>
    dplyr::compute(name = nm)

  cdm[["achilles_results"]] <- cdm[["achilles_results"]] |>
    dplyr::union_all(res) |>
    dplyr::compute(name = "achilles_results")

  omopgenerics::dropSourceTable(cdm = cdm, name = nm)

  return(cdm)
}
