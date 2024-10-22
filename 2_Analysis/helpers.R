createLogger <- function(results = getwd(), name = character()) {
  file <- here::here(
    results,
    paste0("log_", name, "_", format(Sys.time(), "%Y_%m_%d_%H_%M_%S"), ".txt")
  )
  file.create(file)
  options("log_file_og" = file)
  return(invisible(file))
}
log <- function(message = character(), .envir = parent.frame()) {
  message <- glue::glue(message, .envir = .envir) |>
    as.character()
  fileName <- getOption("log_file_og")
  time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cli::cli_inform("{.pkg {time}} {message}")
  con <- file(fileName, open = "a")
  writeLines(paste0("[", time, "] ", message), con = con)
  close(con)
  return(invisible(NULL))
}
cohortDateRangeCheck <- function(cdm, cohort, cohortDateRange){
  if (is.na(cohortDateRange[1])) {
    cohortDateRange[1] <- as.Date(cdm[["observation_period"]] |>
                                    dplyr::summarise(
                                      min = min(.data$observation_period_start_date,
                                                na.rm = TRUE
                                      )
                                    ) |>
                                    dplyr::collect() |>
                                    dplyr::pull("min"))
  }
  if (is.na(cohortDateRange[2])) {
    cohortDateRange[2] <- as.Date(cdm[["observation_period"]] |>
                                    dplyr::summarise(
                                      max = max(.data$observation_period_end_date,
                                                na.rm = TRUE
                                      )
                                    ) |>
                                    dplyr::collect() |>
                                    dplyr::pull("max"))
  }
  n <- cohort |>
    dplyr::group_by(.data$cohort_definition_id, .data$subject_id) |>
    dplyr::arrange(.data$cohort_start_date) |>
    dplyr::mutate(row_num = dplyr::row_number()) |>
    dplyr::filter(.data$row_num == 1) |>
    dplyr::select(-"row_num") |>
    dplyr::ungroup() |>
    dplyr::filter(
      .data$cohort_start_date <= !!cohortDateRange[[2]] &
        .data$cohort_start_date >= !!cohortDateRange[[1]]
    ) |>
    dplyr::tally() |>
    dplyr::pull("n")
  
  return(n==0)
} 
