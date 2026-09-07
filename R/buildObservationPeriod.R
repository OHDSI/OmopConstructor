
#' Build `observation_period` table from data recorded in the `cdm_reference`
#'
#' @param cdm A `cdm_reference` object.
#' @param collapseDays Distance between records to be collapsed.
#' @param persistenceDays Number of days added at the end of an observation
#' period as persistence window.
#' @param dateRange Range of dates to be considered. By default '1900-01-01' is
#' used as start date, whereas for censor date the first available of
#' `source_release_date`, `cdm_release_date`, and `Sys.Date()` will be used.
#' @param censorAge Age to censor individuals if they reach a certain age.
#' The last day in observation of the individual will be the day prior to their
#' birthday.
#' @param recordsFrom Tables to retrieve observation records from.
#' @param periodTypeConceptId Choose the observation_period_type_concept_id that
#' best represents how the period was determined.
#' [Accepted Concepts](https://athena.ohdsi.org/search-terms/terms?domain=Type+Concept&standardConcept=Standard&page=1&pageSize=15&query=).
#'
#' @return The `cdm_reference` object with a new `observation_period`.
#' @export
#'
buildObservationPeriod <- function(cdm,
                                   collapseDays = Inf,
                                   persistenceDays = Inf,
                                   dateRange = as.Date(c("1900-01-01", NA)),
                                   censorAge = 120L,
                                   recordsFrom = c("drug_exposure", "visit_occurrence"),
                                   periodTypeConceptId = 32817L) {
  # initial checks
  cdm <- omopgenerics::validateCdmArgument(cdm = cdm)
  omopgenerics::assertNumeric(collapseDays, length = 1)
  omopgenerics::assertNumeric(persistenceDays, length = 1)
  dateRange <- validateDateRange(dateRange = dateRange, cdm = cdm)
  omopgenerics::assertNumeric(censorAge, length = 1)
  omopgenerics::assertNumeric(periodTypeConceptId, integerish = TRUE, length = 1)
  periodTypeConceptId <- as.integer(periodTypeConceptId)
  omopgenerics::assertChoice(recordsFrom, choices = c(
    "drug_exposure", "condition_occurrence", "procedure_occurrence",
    "visit_occurrence", "device_exposure", "measurement", "observation", "death"
  ), null = TRUE)
  recordsFrom <- unique(recordsFrom)
  if (!is.infinite(censorAge)) censorAge <- as.integer(censorAge)
  if (!is.infinite(collapseDays)) collapseDays <- as.integer(collapseDays)
  if (!is.infinite(persistenceDays)) persistenceDays <- as.integer(persistenceDays)
  if (persistenceDays > collapseDays) {
    cli::cli_abort(c(
      x = "persistenceDays ({persistenceDays}) must be < collapseDays ({collapseDays})"
    ))
  }
  if (persistenceDays == collapseDays & !is.infinite(collapseDays)) {
    cli::cli_inform(c(i = "`persistenceDays` ({.pkg {persistenceDays}}) can not be equal to `collapseDays` ({.pkg {collapseDays}}) as back to back observation periods are not allowed, setting `collapseDays = {.pkg {collapseDays+1}}`."))
    collapseDays <- collapseDays + 1
  }

  if (length(recordsFrom) == 0) {
    cli::cli_warn(c("!" = "`recordsFrom` is empty, empty {.pkg observation_period} created."))
    # return empty
    pid <- cdm$person |>
      utils::head(1) |>
      dplyr::pull("person_id")
    op <- dplyr::tibble(
      observation_period_id = integer(),
      person_id = pid,
      observation_period_start_date = as.Date(character()),
      observation_period_end_date = as.Date(character()),
      period_type_concept_id = integer()
    )
    cdm <- omopgenerics::insertTable(
      cdm = cdm, name = "observation_period", table = op
    )
    return(cdm)
  }

  name <- omopgenerics::uniqueTableName()
  x <- getTemptativeDates(
    cdm = cdm, tables = recordsFrom, collapse = collapseDays,
    window = persistenceDays, name = name
  )

  # only dates after start date
  startDate <- dateRange[1]
  x <- x |>
    dplyr::mutate(start_date = dplyr::if_else(
      .data$start_date <= .env$startDate, .env$startDate, .data$start_date
    ))

  # censor at censor date
  censorDate <- dateRange[2]
  if (is.infinite(persistenceDays)) {
    x <- x |>
      dplyr::mutate(end_date = .env$censorDate)
  } else {
    x <- x |>
      dplyr::mutate(end_date = dplyr::if_else(
        .data$end_date <= .env$censorDate, .data$end_date, .env$censorDate
      ))
  }

  # censor at death
  if ("death" %in% names(cdm)) {
    x <- x |>
      dplyr::left_join(
        cdm$death |>
          dplyr::group_by(.data$persson_id) |>
          dplyr::summarise(death_date = min(.data$death_date, na.rm = TRUE)),
        by = "person_id"
      ) |>
      dplyr::mutate(end_date = dplyr::case_when(
        is.na(.data$death_date) ~ .data$end_date,
        .data$death_date < .data$end_date ~ .data$death_date,
        .default = .data$end_date
      )) |>
      dplyr::select(!"death_date")
  }

  # censor at censorAge
  if (!is.infinite(censorAge)) {
    x <- x |>
      PatientProfiles::addDateOfBirthQuery(dateOfBirthName = "date_of_birth") |>
      dplyr::mutate(
        age_date = as.Date(clock::add_days(
          clock::add_years(.data$date_of_birth, .env$censorAge), -1L
        )),
        end_date = dplyr::if_else(
          .data$age_date <= .data$end_date, .data$age_date, .data$end_date
        )
      )
  }

  # filter censor < start_date
  cdm$observation_period <- x |>
    dplyr::filter(.data$start_date <= .data$end_date) |>
    dplyr::arrange(.data$person_id, .data$start_date) |>
    dplyr::mutate(
      observation_period_id = as.integer(dplyr::row_number()),
      period_type_concept_id = .env$periodTypeConceptId
    ) |>
    dplyr::select(
      "observation_period_id", "person_id",
      "observation_period_start_date" = "start_date",
      "observation_period_end_date" = "end_date", "period_type_concept_id"
    ) |>
    dplyr::compute(name = "observation_period")

  # drop intermediate table
  omopgenerics::dropSourceTable(cdm = cdm, name = name)

  return(cdm)
}
getTemptativeDates <- function(cdm, tables, collapse, window, name) {
  if (is.infinite(collapse)) {
    end <- !is.infinite(window)
    q <- c(
      "min(.data$start_date, na.rm = TRUE)",
      "max(dplyr::case_when(
        is.na(.data$end_date) ~ .data$start_date,
        .data$end_date < .data$start_date ~ .data$start_date,
        .default = .data$end_date
      ), na.rm = TRUE)"
    ) |>
      rlang::parse_exprs() |>
      rlang::set_names(c("start_date", "end_date"))
    q <- q[c(TRUE, end)]
    nm <- omopgenerics::uniqueTableName()
    for (k in seq_along(tables)) {
      xk <- selectColumns(cdm, tables[k])
      if (end) {
        xk <- xk |>
          correctEndDate()
      }
      xk <- xk |>
        dplyr::group_by(.data$person_id) |>
        dplyr::summarise(!!!q, .groups = "drop")
      if (k == 1) {
        x <- xk |>
          dplyr::compute(name = name)
      } else {
        x <- x |>
          dplyr::union_all(
            xk |>
              dplyr::compute(name = nm)
          ) |>
          dplyr::group_by(.data$person_id) |>
          dplyr::summarise(!!!q, .groups = "drop") |>
          dplyr::compute(name = name)
      }
    }
    omopgenerics::dropSourceTable(cdm = cdm, name = nm)
  } else {
    for (k in seq_along(tables)) {
      xk <- selectColumns(cdm, tables[k])
      if (k == 1) {
        x <- xk
      } else {
        x <- x |>
          dplyr::union_all(xk)
      }
    }
    x <- x |>
      collapseRecords(
        startDate = "start_date", endDate = "end_date", by = "person_id",
        gap = collapse, name = name
      )
  }
  if (!is.infinite(window) & window > 0) {
    x <- x |>
      dplyr::mutate(end_date = as.Date(clock::add_days(
        .data$end_date, .env$window
      )))
  }
  x
}
selectColumns <- function(cdm, table) {
  startDate <- omopgenerics::omopColumns(table = table, field = "start_date")
  endDate <- omopgenerics::omopColumns(table = table, field = "end_date")
  sel <- c("person_id", startDate, endDate) |>
    rlang::set_names(c("person_id", "start_date", "end_date"))
  x <- cdm[[table]] |>
    dplyr::select(dplyr::all_of(sel))

  # check if casting is needed
  q <- c()
  if (!isColDate(x, "start_date")) {
    q <- c(q, "start_date" = "as.Date(.data$start_date)")
  }
  if (!isColDate(x, "end_date")) {
    q <- c(q, "end_date" = "as.Date(.data$end_date)")
  }
  if (length(q) > 0) {
    q <- rlang::parse_exprs(q)
    x <- x |>
      dplyr::mutate(!!!q)
  }

  return(x)
}
correctEndDate <- function(x) {
  x |>
    dplyr::mutate(end_date = dplyr::case_when(
      is.na(.data$end_date) ~ .data$start_date,
      .data$end_date < .data$start_date ~ .data$start_date,
      .default = .data$end_date
    ))
}
isColDate <- function(x, col) {
  x |>
    dplyr::select(dplyr::all_of(col)) |>
    utils::head(1L) |>
    dplyr::pull() |>
    dplyr::type_sum() == "date"
}
validateDateRange <- function(dateRange, cdm, call = parent.frame()) {
  if (is.null(dateRange)) {
    dateRange <- as.Date(c(NA, NA))
  } else {
    dateRange <- as.Date(dateRange)
  }

  # check dates
  omopgenerics::assertDate(dateRange, length = 2, na = TRUE, call = call)

  # start date
  if (is.na(dateRange[1])) {
    dateRange[1] <- as.Date("1900-01-01")
    cli::cli_inform(c(i = "Using start date as {.pkg 1900-01-01} please populate dateRange otherwise."))
  }

  if (is.na(dateRange[2])) {
    if ("cdm_source" %in% names(cdm)) {
      cdmSource <- cdm$cdm_source |>
        dplyr::collect()
      if (nrow(cdmSource) == 1) {
        candidate1 <- as.Date(cdmSource$source_release_date)
        if (checkDate(candidate1)) {
          dateRange[2] <- candidate1
          cli::cli_inform(c(i = "Using censor date as {.pkg {dateRange[2]}} from {.emph source_release_date}."))
        } else {
          candidate2 <- as.Date(cdmSource$cdm_release_date)
          if (checkDate(candidate2)) {
            dateRange[2] <- candidate2
            cli::cli_inform(c(i = "Using censor date as {.pkg {dateRange[2]}} from {.emph cdm_release_date}."))
          }
        }
      }
    }
    if (is.na(dateRange[2])) {
      dateRange[2] <- as.Date(Sys.Date())
      cli::cli_inform(c(i = "Using current date as censor date: {.pkg {dateRange[2]}}."))
    }
  }

  if (dateRange[2] > Sys.Date()) {
    cli::cli_warn(c("!" = "Unrealistic censor date ({dateRange[2]}) in the future provided."))
  }

  return(dateRange)
}
checkDate <- function(date) {
  check <- FALSE
  if (inherits(date, "Date")) {
    check <- isTRUE(date >= as.Date("1950-01-01") && date <= Sys.Date())
  }
  return(check)
}
