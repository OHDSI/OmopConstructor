## code to prepare `achillesAnalisisDetails` dataset goes here

achillesAnalisisDetails <- readr::read_csv(
  file = here::here("data-raw", "achilles_analysis_details.csv"),
  col_types = c(
    "ANALYSIS_ID" = "i", "DISTRIBUTION" = "i", "DISTRIBUTED_FIELD" = "c",
    "ANALYSIS_NAME" = "c", "STRATUM_1_NAME" = "c", "STRATUM_2_NAME" = "c",
    "STRATUM_3_NAME" = "c", "STRATUM_4_NAME" = "c", "STRATUM_5_NAME" = "c",
    "IS_DEFAULT" = "i", "CATEGORY" = "c"
  )
) |>
  dplyr::rename_all(tolower)

usethis::use_data(achillesAnalisisDetails, overwrite = TRUE)

achillesAnalisisInternal <- achillesAnalisisDetails |>
  dplyr::inner_join(
    dplyr::tribble(
      ~analysis_id, ~type, ~table,
      1, "count", "person",
      2, "count", "person",
      3, "count", "person",
      4, "count", "person",
      5, "count", "person",
      10, "count", "person",
      12, "count", "person",
      200, "person_count", "visit_occurrence",
      201, "count", "visit_occurrence",
      400, "person_count", "condition_occurrence",
      401, "count", "condition_occurrence",
      500, "person_count", "death",
      501, "count", "death",
      600, "person_count", "procedure_occurrence",
      601, "count", "procedure_occurrence",
      700, "person_count", "drug_exposure",
      701, "count", "drug_exposure",
      800, "person_count", "observation",
      801, "count", "observation",
      900, "person_count", "drug_era",
      901, "count", "drug_era",
      1000, "person_count", "condition_era",
      1001, "count", "condition_era",
      1300, "person_count", "visit_detail",
      1301, "count", "visit_detail",
      1800, "person_count", "measurement",
      1801, "count", "measurement",
      2100, "person_count", "device_exposure",
      2101, "count", "device_exposure",
      2200, "person_count", "note",
      2201, "count", "note"
    ),
    by = "analysis_id"
  ) |>
  dplyr::select("analysis_id", dplyr::starts_with("stratum_"), "type", "table")

usethis::use_data(achillesAnalisisInternal, overwrite = TRUE, internal = TRUE)
