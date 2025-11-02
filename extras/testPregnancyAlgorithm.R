server_dbi <- "cdm_gold_202501"
user <- Sys.getenv("DB_USER")
password <- Sys.getenv("DB_PASSWORD")
port <- Sys.getenv("DB_PORT")
host <-Sys.getenv("DB_HOST")

targetDialect <-"postgresql"
cdmDatabaseSchema <- "public_100k"
vocabularyDatabaseSchema <- "public_100k"
resultsDatabaseSchema <- "results"

# Connect to the database via database connector to run the algorithm
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "postgresql",
  server = paste0(host, "/", server_dbi),
  user = user,
  password = password,
  port = port,
  pathToDriver = here::here("extras")
)
DatabaseConnector::downloadJdbcDrivers(dbms = "postgresql", pathToDriver = here::here("extras"))

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = server_dbi,
  port = port,
  host = host,
  user = user,
  password = password
)

cdm <- CDMConnector::cdmFromCon(
  con = con,
  cdmSchema = cdmDatabaseSchema,
  writeSchema = resultsDatabaseSchema,
  writePrefix = "mc_oc_"
)

pak::pkg_install("oxford-pharmacoepi/pregnancyAlgorithm_postgresql")

library(PregnancyAlgorithm)
init(connectionDetails, resultsDatabaseSchema, useMppBulkLoad = FALSE)

