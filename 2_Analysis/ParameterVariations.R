###
log("- Setting up parameters")
cohortDateRange <- list(as.Date(c(starting_date, ending_date)))

daysPriorObservation <- c(0, 365)

washoutWindow <- c(0, 365)

indexMarkerGap <- c(Inf)

combinationWindow <- list(
  c(0, 365),
  c(30, 730)
)

movingAverageRestriction <- c(548, Inf)

### 
log("- Starting positive control on varying parameters")
positive_results_varying_parameter <- omopgenerics::emptySummarisedResult()

oxfordRefPositive <- oxfordRef |>
  dplyr::filter(ground_truth == 1)
index_events <- oxfordRefPositive |>
  dplyr::pull("index")
marker_events <- oxfordRefPositive |>
  dplyr::pull("marker")

min_sequence_count_1000_positive <- c()

for (i in (1:length(index_events))){
  if (
    cohortDateRangeCheck(cdm = cdm,
                         cdm[[index_events[[i]]]],
                         cohortDateRange = as.Date(c(NA, NA)))
  )
    next
  
  if (
    cohortDateRangeCheck(cdm = cdm,
                         cdm[[marker_events[[i]]]],
                         cohortDateRange = as.Date(c(NA, NA)))
  )
    next
  cdm <- CohortSymmetry::generateSequenceCohortSet(
    cdm = cdm,
    name = paste0(substring(index_events[[i]],1,3), "_", substring(marker_events[[i]],1,3)),
    indexTable = index_events[[i]],
    markerTable = marker_events[[i]],
    combinationWindow = c(0, Inf)
  )
  
  if (
    cdm[[paste0(substring(index_events[[i]],1,3), "_", substring(marker_events[[i]],1,3))]] |>
    dplyr::tally() |>
    dplyr::pull("n") <= 1000 
  ) next
  
  min_sequence_count_1000_positive <- c(min_sequence_count_1000_positive, i)
}

for (i in min_sequence_count_1000_positive){
  for (a in cohortDateRange){
    if (
      cohortDateRangeCheck(cdm = cdm,
                           cdm[[index_events[[i]]]],
                           cohortDateRange = a)
    )
      next
    
    if (
      cohortDateRangeCheck(cdm = cdm,
                           cdm[[marker_events[[i]]]],
                           cohortDateRange = a)
    )
      next
    for (b in daysPriorObservation){
      for (c in washoutWindow){
        for (d in indexMarkerGap){
          for (e in combinationWindow){
            for (f in movingAverageRestriction){
              tictoc::tic()
              cdm <- CohortSymmetry::generateSequenceCohortSet(cdm = cdm,
                                                               name = paste0(substring(index_events[[i]],1,5), "_", substring(marker_events[[i]],1,5)),
                                                               cohortDateRange = a,
                                                               indexTable = index_events[[i]],
                                                               markerTable = marker_events[[i]],
                                                               daysPriorObservation = b,
                                                               washoutWindow = c,
                                                               indexMarkerGap = d, 
                                                               combinationWindow = e,
                                                               movingAverageRestriction = f)
              
              if (
                cdm[[paste0(substring(index_events[[i]],1,5), "_", substring(marker_events[[i]],1,5))]] |>
                dplyr::summarise(n = n_distinct(cohort_definition_id)) |>
                dplyr::pull("n") == 0 
              ) next
              
              res <- CohortSymmetry::summariseSequenceRatios(cdm[[paste0(substring(index_events[[i]],1,5), "_", substring(marker_events[[i]],1,5))]],
                                                             minCellCount = 0)
              
              positive_results_varying_parameter <- positive_results_varying_parameter |>
                omopgenerics::bind(res)
              getCohortSeqtime <- tictoc::toc()$callback_msg 
          }
        }
      }
    }
  }
 }
}

setting <- omopgenerics::settings(positive_results_varying_parameter) |>
  dplyr::mutate(ground_truth = 1)

positive_results_varying_parameter <- positive_results_varying_parameter |>
  omopgenerics::newSummarisedResult(
    settings = setting
  )

positive_results_varying_parameter <- positive_results_varying_parameter |>
  omopgenerics::suppress(minCellCount = minCellCount)

saveRDS(positive_results_varying_parameter,
        here::here(output_folder, "positive_results_varying_parameter.rds"))

### 
log("- Starting negative control on varying parameters")
negative_results_varying_parameter <- omopgenerics::emptySummarisedResult()

oxfordRefNegative <- oxfordRef |>
  dplyr::filter(ground_truth == 0)
index_events <- oxfordRefNegative |>
  dplyr::pull("index")
marker_events <- oxfordRefNegative |>
  dplyr::pull("marker")

min_sequence_count_1000_negative <- c()

for (i in (1:length(index_events))){
  if (
    cohortDateRangeCheck(cdm = cdm,
                         cdm[[index_events[[i]]]],
                         cohortDateRange = as.Date(c(NA, NA)))
  )
    next
  
  if (
    cohortDateRangeCheck(cdm = cdm,
                         cdm[[marker_events[[i]]]],
                         cohortDateRange = as.Date(c(NA, NA)))
  )
    next
  cdm <- CohortSymmetry::generateSequenceCohortSet(
    cdm = cdm,
    name = paste0(substring(index_events[[i]],1,3), "_", substring(marker_events[[i]],1,3)),
    indexTable = index_events[[i]],
    markerTable = marker_events[[i]],
    combinationWindow = c(0, Inf)
  )
  
  if (
    cdm[[paste0(substring(index_events[[i]],1,3), "_", substring(marker_events[[i]],1,3))]] |>
    dplyr::tally() |>
    dplyr::pull("n") <= 1000 
  ) next
  
  min_sequence_count_1000_negative <- c(min_sequence_count_1000_negative, i)
}

for (i in min_sequence_count_1000_negative){
  for (a in cohortDateRange){
    if (
      cohortDateRangeCheck(cdm = cdm,
                           cdm[[index_events[[i]]]],
                           cohortDateRange = a)
    )
      next
    
    if (
      cohortDateRangeCheck(cdm = cdm,
                           cdm[[marker_events[[i]]]],
                           cohortDateRange = a)
    )
      next
    for (b in daysPriorObservation){
      for (c in washoutWindow){
        for (d in indexMarkerGap){
          for (e in combinationWindow){
            for (f in movingAverageRestriction){
              tictoc::tic()
              cdm <- CohortSymmetry::generateSequenceCohortSet(cdm = cdm,
                                                               name = paste0(substring(index_events[[i]],1,5), "_", substring(marker_events[[i]],1,5)),
                                                               cohortDateRange = a,
                                                               indexTable = index_events[[i]],
                                                               markerTable = marker_events[[i]],
                                                               daysPriorObservation = b,
                                                               washoutWindow = c,
                                                               indexMarkerGap = d, 
                                                               combinationWindow = e,
                                                               movingAverageRestriction = f)
              
              if (
                cdm[[paste0(substring(index_events[[i]],1,5), "_", substring(marker_events[[i]],1,5))]] |>
                dplyr::summarise(n = n_distinct(cohort_definition_id)) |>
                dplyr::pull("n") == 0 
              ) next
              
              res <- CohortSymmetry::summariseSequenceRatios(cdm[[paste0(substring(index_events[[i]],1,5), "_", substring(marker_events[[i]],1,5))]],
                                                             minCellCount = 0)
              
              negative_results_varying_parameter <- negative_results_varying_parameter |>
                omopgenerics::bind(res)
              getCohortSeqtime <- tictoc::toc()$callback_msg 
            }
          }
        }
      }
    }
  }
}

setting_negative <- omopgenerics::settings(negative_results_varying_parameter) |>
  dplyr::mutate(ground_truth = 0)

negative_results_varying_parameter <- negative_results_varying_parameter |>
  omopgenerics::newSummarisedResult(
    settings = setting_negative
  )

negative_results_varying_parameter <- negative_results_varying_parameter |>
  omopgenerics::suppress(minCellCount = minCellCount)

saveRDS(negative_results_varying_parameter,
        here::here(output_folder, "negative_results_varying_parameter.rds"))

# ### 
# log("- Viewing results")
# result_by_parameter <- omopgenerics::bind(positive_results_varying_parameter,
#                              negative_results_varying_parameter)
# 
# OmopViewer::exportStaticApp(
#   result = result_by_parameter,
#   directory = here::here()
# )
