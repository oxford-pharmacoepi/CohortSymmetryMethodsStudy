###
log("- Setting up parameters")
cohortDateRange <- list(as.Date(c(NA, NA)), 
                        as.Date(c("2010-01-01", "2021-12-31")))

daysPriorObservation <- c(0, 365, 730)

washoutWindow <- c(0, 365, )

indexMarkerGap <- c(30)

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

for (a in cohortDateRange){
  for (b in daysPriorObservation){
    for (c in washoutWindow){
      for (d in indexMarkerGap)
        for (e in combinationWindow){
          for (f in movingAverageRestriction){
            for (i in (1:length(index_events))){
              tictoc::tic()
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
              
              positive_results_varying_parameter <- positive_results_varying_parameter |>
                omopgenerics::bind(CohortSymmetry::summariseSequenceRatios(cdm[[paste0(substring(index_events[[i]],1,5), "_", substring(marker_events[[i]],1,5))]]))
              getCohortSeqtime <- tictoc::toc()$callback_msg 
            }
          }
        }
    }
  }
}

