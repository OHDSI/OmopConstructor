
library(rJava)
library(Eunomia)
library(Achilles)
library(dplyr)
library(omopgenerics)
library(purrr)
library(omock)
library(OmopConstructor)

runAchilles <- function(id) {
  result <- list()

  # table to check
  distribution <- OmopConstructor:::achillesAnalisisDetails |>
    dplyr::filter(analysis_id == id) |>
    dplyr::pull("distribution")
  if (distribution == 1) {
    table <- "achilles_results_dist"
  } else {
    table <- "achilles_results"
  }

  # run analysis using achilles package
  cd <- Eunomia::getEunomiaConnectionDetails()
  Achilles::achilles(
    cdmVersion = "5.4",
    connectionDetails = cd,
    cdmDatabaseSchema = "main",
    analysisIds = id,
    defaultAnalysesOnly = FALSE
  )

  # read achilles package result
  con <- connect(connectionDetails = cd)
  result$achilles <- dplyr::tbl(con, DatabaseConnector::inDatabaseSchema("main", table)) |>
    dplyr::filter(.data$analysis_id == .env$id) |>
    dplyr::collect() |>
    dplyr::arrange(dplyr::across(dplyr::everything()))
  disconnect(connection = con)

  # run analysis using omopconstructor package
  cdm <- mockCdmFromDataset(datasetName = "GiBleed", source = "duckdb") |>
    buildAchilles(achillesId = id)

  # read omopconstructor package result
  result$omopconstructor <- cdm[[table]] |>
    dplyr::filter(.data$analysis_id == .env$id) |>
    dplyr::collect() |>
    dplyr::arrange(dplyr::across(dplyr::everything()))

  result$equal <- identical(result$achilles, result$omopconstructor)

  if (distribution == 1) {
    result$difference <- NULL
  } else {
    result$difference <- result$achilles |>
      dplyr::mutate(name = "achilles") |>
      dplyr::union_all(
        result$omopconstructor |>
          dplyr::mutate(name = "omop_constructor")
      ) |>
      tidyr::pivot_wider(values_from = "count_value")
    if (!"achilles" %in% colnames(result$difference)) {
      result$difference <- result$difference |>
        dplyr::mutate(achilles = NA_integer_)
    }
    if (!"omop_constructor" %in% colnames(result$difference)) {
      result$difference <- result$difference |>
        dplyr::mutate(omop_constructor = NA_integer_)
    }
    result$difference <- result$difference |>
      dplyr::filter(
        is.na(achilles) |
          is.na(omop_constructor) |
          achilles != omop_constructor
      )
  }

  return(result)
}

analyses <- OmopConstructor:::achillesAnalisisDetails |>
  filter(distribution != 1) |>
  select("analysis_id", "analysis_name")
differences <- analyses$analysis_id
names(differences) <- analyses$analysis_name

differences <- differences |>
  map(\(x) {
    tryCatch(
      {runAchilles(x)},
      error = function(e) NULL
    )
  })
