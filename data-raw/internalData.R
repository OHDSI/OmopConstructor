
gest_est <- readr::read_csv(
  file = here::here("extras", "pregnancyConcepts", "gest_est.csv"),
  col_types = c(category = "c", gest_est_id = "i", preterm = "i", postterm = "i", fullterm = "i", nodata = "i")
)
outcome_limit <- readr::read_csv(
  file = here::here("extras", "pregnancyConcepts", "outcome_limit.csv"),
  col_types = c(first_preg_category = "c", outcome_preg_category = "c", outcome_limit_id = "i", min_days = "i")
)
pregnancy_concepts <- readr::read_csv(
  file = here::here("extras", "pregnancyConcepts", "pregnancy_concepts.csv"),
  col_types = c(category = "c", data_value = "c", concept_id = "i", gest_value = "i")
)
term_durations <- readr::read_csv(
  file = here::here("extras", "pregnancyConcepts", "term_durations.csv"),
  col_types = c(category = "c", max_term = "i", min_term = "i", retry = "i")
)

achillesAnalisisDetails <- readr::read_csv(
  file = "https://raw.githubusercontent.com/OHDSI/Achilles/refs/heads/main/inst/csv/achilles/achilles_analysis_details.csv",
  col_types = c(
    analysis_id = "i", distribution = "i", distributed_field = "c",
    analysis_name = "c", stratum_1_name = "c", stratum_2_name = "c",
    stratum_3_name = "c", stratum_4_name = "c", stratum_5_name = "c",
    is_default = "l", category = "c"
  )
) |>
  dplyr::mutate(is_minimal = .data$analysis_id %in% c(
    200, 201, 225, 400, 401, 425, 600, 601, 625, 700, 701, 725, 800, 801, 825,
    1800, 1801, 1825, 2100, 2101, 2125
  )) |>
  dplyr::relocate("is_minimal", .before = "category") |>
  # issue 790 Achilles
  dplyr::mutate(
    stratum_1_name = dplyr::case_when(
      .data$analysis_id == 226 ~ "visit_concept_id",
      .data$analysis_id == 815 ~ "observation_concept_id",
      .data$analysis_id == 827 ~ "unit_concept_id",
      .data$analysis_id %in% c(1815, 1816, 1817, 1818) ~ "measurement_concept_id",
      .data$analysis_id == 1827 ~ "unit_concept_id",
      .default = .data$stratum_1_name
    ),
    stratum_2_name = dplyr::case_when(
      .data$analysis_id %in% c(815, 1815, 1816, 1817, 1818) ~ "unit_concept_id",
      .default = .data$stratum_2_name
    ),
    stratum_3_name = dplyr::case_when(
      .data$analysis_id == 1818 ~ "measurement_range",
      .default = .data$stratum_3_name
    )
  )

extraDetails <- readr::read_csv(
  file = here::here("data-raw", "achilles_analysis_extra_details.csv"),
  col_types = c(analysis_id = "i", type = "c", table = "c", operation = "c")
)
achillesAnalisisDetails <- achillesAnalisisDetails |>
  dplyr::left_join(extraDetails, by = "analysis_id")

usethis::use_data(
  achillesAnalisisDetails,
  gest_est,
  outcome_limit,
  pregnancy_concepts,
  term_durations,
  overwrite = TRUE,
  internal = TRUE
)
