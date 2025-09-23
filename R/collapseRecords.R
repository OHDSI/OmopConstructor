
#' Collapse records of a `cdm_table` into episodes.
#'
#' @param x A `cdm_table` object.
#' @param startDate Column in `x` that points to the start date of the record.
#' @param endDate Column in `x` that point to the end date of the record.
#' @param by Columns in `x` that aggregate the records.
#' @param gap Integer; distance allowed between two consecutive records to be
#' collapsed.
#' @param toSummarise Columns in x that we want to be keep, the different
#' columns will be added up.
#' @param name Name of the new `cdm_table` created.
#'
#' @return The `x` `cdm_table` with the records collapsed.
#' @export
#'
collapseRecords <- function(x,
                            startDate,
                            endDate,
                            by,
                            gap = 0L,
                            toSummarise = character(),
                            name = NULL) {
  # input check
  omopgenerics::validateCdmTable(table = x)
  cdm <- omopgenerics::cdmReference(x)
  omopgenerics::validateColumn(column = startDate, x = x)
  omopgenerics::validateColumn(column = endDate, x = x)
  omopgenerics::assertCharacter(by, unique = TRUE)
  omopgenerics::assertChoice(by, colnames(x))
  omopgenerics::assertCharacter(toSummarise, unique = TRUE)
  omopgenerics::assertChoice(toSummarise, colnames(x))
  omopgenerics::assertNumeric(gap, integerish = TRUE, min = 0, length = 1L)
  if (!is.infinite(gap)) {
    gap <- as.integer(gap)
  }
  name <- omopgenerics::validateNameArgument(name, cdm = cdm, null = TRUE)

  extraColumns <- colnames(x) |>
    purrr::keep(\(x) !x %in% c(startDate, endDate, by, toSummarise))
  if (length(extraColumns) > 0) {
    cli::cli_inform(c("!" = "Columns {.var {extraColumns}} will be dropped from the cdm_table."))
  }

  # eliminate missing start dates and correct end date
  q <- "dplyr::case_when(
    is.na(.data[['{endDate}']]) ~ .data[['{startDate}']],
    .data[['{endDate}']] < .data[['{startDate}']] ~ .data[['{startDate}']],
    .default = .data[['{endDate}']]
  )" |>
    glue::glue() |>
    rlang::set_names(endDate) |>
    rlang::parse_exprs()
  x <- x |>
    dplyr::filter(!is.na(.data[[startDate]])) |>
    dplyr::mutate(!!!q)

  if (omopgenerics::isTableEmpty(x)) {
    x <- x |>
      dplyr::select(dplyr::all_of(c(by, startDate, endDate))) |>
      dplyr::compute(name = name)
    return(x)
  }

  if (is.infinite(gap)) {
    x <- x |>
      dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
      dplyr::summarise(
        !!startDate := min(.data[[startDate]], na.rm = TRUE),
        !!endDate := max(.data[[endDate]], na.rm = TRUE),
        dplyr::across(
          dplyr::all_of(toSummarise), \(x) as.integer(sum(x, na.rm = TRUE))
        ),
        .groups = "drop"
      ) |>
      dplyr::compute(name = name)
    return(x)
  }

  # get unique ids
  id <- omopgenerics::uniqueId(n = 3, exclude = c(by, startDate, endDate), prefix = "xyz")
  # this is so any name of column can be used in by, startdate or endDate
  # id[1] -> date
  # id[2] -> date_id (-1 start; 1 end)
  # id[3] -> era_id (number of era)

  # to summarise
  newCols <- rep(0, length(toSummarise)) |>
    as.list() |>
    rlang::set_names(nm = toSummarise)

  # start dates
  sel <- rlang::set_names(startDate, id[1])
  start <- x |>
    dplyr::select(dplyr::all_of(c(by, sel, toSummarise))) |>
    dplyr::mutate(!!id[2] := -1L)
  # end dates
  sel <- rlang::set_names(endDate, id[1])
  end <- x |>
    dplyr::select(dplyr::all_of(c(by, sel))) |>
    dplyr::mutate(!!id[2] := 1L, !!!newCols)

  # add gap
  if (gap > 0L) {
    end <- end |>
      dplyr::mutate(!!id[1] := as.Date(clock::add_days(x = .data[[id[1]]], n = .env$gap)))
  }

  # join dates
  x <- start |>
    dplyr::union_all(end) |>
    # group
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
    # arrange by date and date_id
    dplyr::arrange(.data[[id[1]]], .data[[id[2]]]) |>
    # calculate the era_id
    dplyr::mutate(
      !!id[3] := dplyr::if_else(cumsum(.data[[id[2]]]) == -1 & .data[[id[2]]] == -1, -1L, 0L)
    ) |>
    dplyr::arrange(.data[[id[1]]], .data[[id[2]]], .data[[id[3]]]) |>
    dplyr::mutate(!!id[3] := -cumsum(.data[[id[3]]])) |>
    # summarise
    dplyr::group_by(.data[[id[3]]], .add = TRUE) |>
    dplyr::summarise(
      !!startDate := min(.data[[id[1]]], na.rm = TRUE),
      !!endDate := max(.data[[id[1]]], na.rm = TRUE),
      dplyr::across(
        dplyr::all_of(toSummarise), \(x) as.integer(sum(x, na.rm = TRUE))
      ),
      .groups = "drop"
    ) |>
    dplyr::select(dplyr::all_of(c(by, startDate, endDate, toSummarise))) |>
    dplyr::arrange()

  if (gap > 0L) {
    x <- x |>
      dplyr::mutate(!!endDate := as.Date(clock::add_days(x = .data[[endDate]], n = -gap)))
  }

  x |>
    dplyr::compute(name = name)
}
