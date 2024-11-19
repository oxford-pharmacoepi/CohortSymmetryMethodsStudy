########################
# positive controls
########################
log("- Starting positive controls")
atc_conversion <- tibble(
  name = atc_events,
  cohort_name = atc_event_name
)

oxfordRef <- oxfordRef |>
  dplyr::left_join(atc_conversion, by = c("index" = "name")) |>
  dplyr::mutate(
    index = case_when(is.na(cohort_name) ~ index,
                      T ~ cohort_name)
  ) |>
  dplyr::select(-"cohort_name") |>
  dplyr::left_join(atc_conversion, by = c("marker" = "name")) |>
  dplyr::mutate(
    marker = case_when(is.na(cohort_name) ~ marker,
                      T ~ cohort_name)
  ) |>
  dplyr::select(-"cohort_name")

oxfordRefPositive <- oxfordRef |>
  dplyr::filter(ground_truth == 1)

positive_controls_results <- list()
index_events <- oxfordRefPositive |>
  dplyr::pull("index")
marker_events <- oxfordRefPositive |>
  dplyr::pull("marker")

tryCatch({
for (i in (1:length(index_events))){
  tictoc::tic()
  if (
    cohortDateRangeCheck(cdm = cdm,
                         cdm[[index_events[[i]]]],
                         cohortDateRange = c(starting_date, ending_date))
  )
    next
  
  if (
    cohortDateRangeCheck(cdm = cdm,
                         cdm[[marker_events[[i]]]],
                         cohortDateRange = c(starting_date, ending_date))
  )
   next
  cdm <- CohortSymmetry::generateSequenceCohortSet(cdm = cdm,
                                                   name = paste0(substring(index_events[[i]],1,5), "_", substring(marker_events[[i]],1,5)),
                                                   cohortDateRange = c(starting_date, ending_date),
                                                   indexTable = index_events[[i]],
                                                   markerTable = marker_events[[i]])
 
  if (
    cdm[[paste0(substring(index_events[[i]],1,5), "_", substring(marker_events[[i]],1,5))]] |>
    dplyr::summarise(n = n_distinct(cohort_definition_id)) |>
    dplyr::pull("n") == 0 
  ) next
  
  positive_controls_results[[paste0(index_events[[i]], "_", marker_events[[i]])]] <- 
    CohortSymmetry::summariseSequenceRatios(cdm[[paste0(substring(index_events[[i]],1,5), "_", substring(marker_events[[i]],1,5))]])
  getCohortSeqtime <- tictoc::toc()$callback_msg 
 }
}, error = function(e) {
  writeLines(as.character(e),
             here(output_folder, paste0("/", db_name, "_positive_control_error.txt"
             )))
})

positive_control_res <- bind_rows(positive_controls_results) |>
  omopgenerics::newSummarisedResult()

settings <- omopgenerics::settings(positive_control_res) |>
  dplyr::mutate(ground_truth = 1)

positive_control_res <- positive_control_res |>
  omopgenerics::newSummarisedResult(
    settings = settings
  )

saveRDS(positive_control_res, file = here(output_folder, paste0("/", db_name, "_positive_control_res.rds")))
  
##############################
# negative controls
##############################
log("- Getting cohort sequence negative controls")

oxfordRefNegative <- oxfordRef |>
  dplyr::filter(ground_truth == 0)

negative_controls_results <- list()
index_events <- oxfordRefNegative |>
  dplyr::pull("index")
marker_events <- oxfordRefNegative |>
  dplyr::pull("marker")

tryCatch({
  for (i in (1:length(index_events))){
    tictoc::tic()
    if (
      (cdm[[index_events[[i]]]] |> dplyr::tally() |> dplyr::pull("n") == 0)|(cdm[[marker_events[[i]]]] |> dplyr::tally() |> dplyr::pull("n") == 0)
    ) next
    cdm <- CohortSymmetry::generateSequenceCohortSet(cdm = cdm,
                                                     name = paste0(substring(index_events[[i]],1,5), "_", substring(marker_events[[i]],1,5)),
                                                     cohortDateRange = c(starting_date, ending_date),
                                                     indexTable = index_events[[i]],
                                                     markerTable = marker_events[[i]])
    
    if (
      cdm[[paste0(substring(index_events[[i]],1,5), "_", substring(marker_events[[i]],1,5))]] |>
      dplyr::summarise(n = n_distinct(cohort_definition_id)) |>
      dplyr::pull("n") == 0 
    ) next
    
    negative_controls_results[[paste0(index_events[[i]], "_", marker_events[[i]])]] <- 
      CohortSymmetry::summariseSequenceRatios(cdm[[paste0(substring(index_events[[i]],1,5), "_", substring(marker_events[[i]],1,5))]])
    getCohortSeqtime <- tictoc::toc()$callback_msg 
  }
}, error = function(e) {
  writeLines(as.character(e),
             here(output_folder, paste0("/", db_name, "_negative_control_error.xlsx"
             )))
})

negative_control_res <- bind_rows(negative_controls_results) |>
  omopgenerics::newSummarisedResult()

settings <- omopgenerics::settings(negative_control_res) |>
  dplyr::mutate(ground_truth = 0)

negative_control_res <- negative_control_res |>
  omopgenerics::newSummarisedResult(
    settings = settings
  )

saveRDS(negative_control_res, file = here(output_folder, paste0("/", db_name, "_negative_control_res.rds")))

result <- omopgenerics::bind(positive_control_res, negative_control_res)
saveRDS(result, file = here(output_folder, paste0("/", db_name, "_result.rds")))

# ###
# log("- Lauching Shiny App")
# OmopViewer::exportStaticApp(
#   result = result,
#   directory = here::here()
# )
