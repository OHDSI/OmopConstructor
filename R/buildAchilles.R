
#' Create achilles tables
#'
#' @param cdm A cdm_reference object
#' @param achillesId A vector of achilles ids. You can also use "minimal",
#' "default" or "all" to point to a subset of analyses. Or also "person",
#' "observation period", ... to run the analyses refering to a certain category.
#'
#' @return The cdm_reference object with the achilles tables populated.
#' @export
#'
buildAchilles <- function(cdm,
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
    nm <- achillesAnalisisDetails |>
      dplyr::filter(.data$analysis_id == .env$id) |>
      dplyr::pull("analysis_name")
    kk <- sprintf("%i", k) |>
      stringr::str_pad(width = nchar(len), pad = " ")
    cli::cli_inform(c("i" = "{kk} of {len}: Get achilles result for {.pkg {nm}}."))
    cdm <- appendAchillesId(cdm, id)
  }

  # append to achilles_analysis
  cdm <- appendAchillesAnalysis(cdm = cdm, achillesId = achillesId)

  return(cdm)
}

validateAchillesId <- function(achillesId, call = parent.frame()) {
  # error message
  labels <- c("minimal", "default", "all", unique(achillesAnalisisDetails$category)) |>
    tolower()
  msg <- c(x = "`achillesId` must be a valid achilles_id or a choice between {.var {labels}}.")

  # possible ids
  possibleIds <- achillesAnalisisDetails$analysis_id

  # default analysis_id
  if (is.null(achillesId)) {
    achillesId <- achillesAnalisisDetails |>
      dplyr::filter(.data$is_default) |>
      dplyr::pull("analysis_id")
  } else if (is.numeric(achillesId)) {
    achillesId <- as.integer(achillesId)
    omopgenerics::assertNumeric(achillesId, integerish = TRUE, unique = TRUE, call = call, msg = msg)
    ignored <- achillesId[!achillesId %in% possibleIds]
    if (length(ignored) > 0) {
      cli::cli_inform(c("i" = "{length(ignored)} analysis id{?s} {?is/are} not configured: {ignored}."))
    }
    achillesId <- achillesId[achillesId %in% possibleIds]
  } else if (is.character(achillesId)) {
    achillesId <- tolower(achillesId)
    omopgenerics::assertChoice(achillesId, choices = labels, length = 1, call = call, msg = msg)
    if (achillesId == "minimal") {
      achillesId <- achillesAnalisisDetails |>
        dplyr::filter(.data$is_minimal) |>
        dplyr::pull("analysis_id")
    } else if (achillesId == "default") {
      achillesId <- achillesAnalisisDetails |>
        dplyr::filter(.data$is_default) |>
        dplyr::pull("analysis_id")
    } else if (achillesId == "all") {
      achillesId <- possibleIds
    } else {
      achillesId <- achillesAnalisisDetails |>
        dplyr::filter(.data$category == .env$achillesId) |>
        dplyr::pull("analysis_id")
    }
  } else {
    cli::cli_abort(message = msg, call = call)
  }

  achillesId
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
      "Removing {length(ids)} present analysis ids from {.pkg {nm}}." |>
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
  analysis <- achillesAnalisisDetails |>
    dplyr::filter(.data$analysis_id == .env$id)

  # get table
  tableName <- analysis$table
  x <- cdm[[tableName]]

  # perform operation
  x <- operation(x = x, op = analysis$operation)

  # name of the table
  nm <- omopgenerics::uniqueTableName()

  # perform calculation
  types <- analysisType(type = analysis$type)
  if (types[[1]] == "update") {
    res <- update(cdm = cdm)
  } else if (types[[1]] == "count") {
    res <- counts(x = x, by = groupBy(analysis = analysis), count = types[[2]])
  }

  # compute table
  nm <- omopgenerics::uniqueTableName()
  res <- dplyr::compute(x = res, name = nm)

  # prepare res
  res <- prepareResult(res = res, id = id)

  # add results in the corresponding table
  name <- ifelse(analysis$distribution != 1, "achilles_results", "achilles_results_dist")
  cdm[[name]] <- cdm[[name]] |>
    dplyr::union_all(res) |>
    dplyr::compute(name = name)

  # drop table
  omopgenerics::dropSourceTable(cdm = cdm, name = nm)

  return(cdm)
}
operation <- function(x, op) {
  if (is.na(op)) {
    return(x)
  }

  op <- as.list(stringr::str_split_1(string = op, pattern = " "))
  cdm <- omopgenerics::cdmReference(table = x)

  if (op[[1]] == "remove") {
    table <- op[[2]]
    col <- op[[3]]
    x <- x |>
      dplyr::filter(!is.na(.data[[col]])) |>
      dplyr::anti_join(cdm[[table]], by = col)
  }
  x
}
analysisType <- function(type) {
  as.list(stringr::str_split_1(string = type, pattern = " "))
}
update <- function(cdm) {
  cdm$person |>
    dplyr::ungroup() |>
    dplyr::tally(name = "count_value") |>
    dplyr::mutate(
      count_value = as.integer(.data$count_value),
      stratum_1 = !!omopgenerics::cdmName(x = cdm),
      stratum_2 = "1.7.2",
      stratum_3 = !!as.character(Sys.Date())
    )
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
counts <- function(x, by, count) {
  fun <- switch(count,
                "record" = "dplyr::n()",
                "person" = "dplyr::n_distinct(.data$person_id)")
  q <- paste0("as.integer(", fun, ")") |>
    rlang::set_names("count_value") |>
    rlang::parse_exprs()
  x |>
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
    dplyr::summarise(!!!q)
}
prepareResult <- function(res, id) {
  q <- paste0("stratum_", 1:5) |>
    rlang::set_names() |>
    purrr::map_chr(\(x) {
      if (x %in% colnames(res)) {
        paste0("as.character(.data$", x, ")")
      } else {
        "as.character(NA)"
      }
    })
  q["analysis_id"] <- paste0("as.integer(", id, ")")
  q <- rlang::parse_exprs(q)
  res |>
    dplyr::mutate(!!!q)
}
appendAchillesAnalysis <- function(cdm, achillesId) {
  nm <- omopgenerics::uniqueTableName()

  # insert table
  cdm <- omopgenerics::insertTable(
    cdm = cdm,
    name = nm,
    table = achillesAnalisisDetails |>
      dplyr::filter(.data$analysis_id %in% .env$achillesId) |>
      dplyr::select(
        "analysis_id", "distribution", "distributed_field", "analysis_name",
        "stratum_1_name", "stratum_2_name", "stratum_3_name", "stratum_4_name",
        "stratum_5_name", "is_default", "category"
      )
  )

  # join
  cdm[["achilles_analysis"]] <- cdm[["achilles_analysis"]] |>
    dplyr::union_all(cdm[[nm]]) |>
    dplyr::compute(name = "achilles_analysis")

  # remove temp table
  omopgenerics::dropSourceTable(cdm = cdm, name = nm)
}
