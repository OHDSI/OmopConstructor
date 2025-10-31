
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

usethis::use_data(gest_est, outcome_limit, pregnancy_concepts, term_durations, overwrite = TRUE, internal = TRUE)
