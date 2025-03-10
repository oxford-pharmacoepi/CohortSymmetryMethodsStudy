###
log("- Setting up parameters")
cohortDateRange <- list(as.Date(c(starting_date, ending_date)))

daysPriorObservation <- c(365)

washoutWindow <- c(0, 365)

indexMarkerGap <- c(Inf)

combinationWindow <- list(
  c(0, 365),
  c(0, 730)
)

movingAverageRestriction <- c(548, Inf)

### 
log("- Starting positive control on varying parameters")
positive_results_varying_parameter <- omopgenerics::emptySummarisedResult()

positive_controls <- results$result |>
  omopgenerics::filterSettings(ground_truth == "1") |>
  visOmopResults::tidy() |>
  dplyr::select("index_cohort_name", "marker_cohort_name") |>
  dplyr::distinct() |>
  dplyr::mutate(
    index_cohort_name = stringr::str_replace(index_cohort_name, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", ""),
    marker_cohort_name = stringr::str_replace(marker_cohort_name, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", "")
  ) |>
  dplyr::mutate(
    index_cohort_name = 
      case_when(index_cohort_name == "benzodiazepine_derivatives" ~ "combined_benzodiazepine_derivatives",
                T ~ index_cohort_name),
    marker_cohort_name = 
      case_when(marker_cohort_name == "benzodiazepine_derivatives" ~ "combined_benzodiazepine_derivatives",
                T ~ marker_cohort_name)
  ) |>
  dplyr::mutate(
    index_cohort_name = 
      case_when(index_cohort_name == "antiinflammatory_and_antirheumatic_products_non_steroids" ~ "nsaids",
                T ~ index_cohort_name),
    marker_cohort_name = 
      case_when(marker_cohort_name == "antiinflammatory_and_antirheumatic_products_non_steroids" ~ "nsaids",
                T ~ marker_cohort_name)
  ) |>
  dplyr::mutate(
    index_cohort_name = 
      case_when(index_cohort_name == "ace_inhibitors_plain" ~ "ace_inhibitors",
                T ~ index_cohort_name),
    marker_cohort_name = 
      case_when(marker_cohort_name == "ace_inhibitors_plain" ~ "ace_inhibitors",
                T ~ marker_cohort_name)
  ) |>
  dplyr::mutate(
    index_cohort_name = 
      case_when(index_cohort_name == "corticosteroids_for_systemic_use" ~ "corticosteroids",
                T ~ index_cohort_name),
    marker_cohort_name = 
      case_when(marker_cohort_name == "corticosteroids_for_systemic_use" ~ "corticosteroids",
                T ~ marker_cohort_name)
  ) |>
  dplyr::mutate(
    index_cohort_name = 
      case_when(index_cohort_name == "benzodiazepine_derivatives_n05ba_benzodiazepine_derivatives_n05cd_benzodiazepine_derivatives" ~ "benzodiazepine_derivatives",
                T ~ index_cohort_name),
    marker_cohort_name = 
      case_when(marker_cohort_name == "benzodiazepine_derivatives_n05ba_benzodiazepine_derivatives_n05cd_benzodiazepine_derivatives" ~ "benzodiazepine_derivatives",
                T ~ marker_cohort_name)
  ) |>
  dplyr::mutate(
    index_cohort_name = 
      case_when(index_cohort_name == "acetylsalicylic_acid_oral_platelet_aggregation_inhibitors_excl_heparin" ~ "acetylsalicylic_acid",
                T ~ index_cohort_name),
    marker_cohort_name = 
      case_when(marker_cohort_name == "acetylsalicylic_acid_oral_platelet_aggregation_inhibitors_excl_heparin" ~ "acetylsalicylic_acid",
                T ~ marker_cohort_name)
  ) |>
  dplyr::mutate(
    index_cohort_name = 
      case_when(index_cohort_name == "insulins_and_analogues" ~ "insulin",
                T ~ index_cohort_name),
    marker_cohort_name = 
      case_when(marker_cohort_name == "insulins_and_analogues" ~ "insulin",
                T ~ marker_cohort_name)
  ) |>
  dplyr::mutate(
    index_cohort_name = 
      case_when(index_cohort_name == "cough_suppressants_excl_combinations_with_expectorants" ~ "antitussive_agents",
                T ~ index_cohort_name),
    marker_cohort_name = 
      case_when(marker_cohort_name == "cough_suppressants_excl_combinations_with_expectorants" ~ "antitussive_agents",
                T ~ marker_cohort_name)
  ) |>
  dplyr::mutate(
    index_cohort_name = 
      case_when(index_cohort_name == "corticosteroids" ~ "steroids",
                T ~ index_cohort_name),
    marker_cohort_name = 
      case_when(marker_cohort_name == "corticosteroids" ~ "steroids",
                T ~ marker_cohort_name)
  ) |>
  dplyr::mutate(
    index_cohort_name = 
      case_when(index_cohort_name == "insulin" ~ "insulins",
                T ~ index_cohort_name),
    marker_cohort_name = 
      case_when(marker_cohort_name == "insulin" ~ "insulins",
                T ~ marker_cohort_name)
  ) |>
  dplyr::mutate(
    index_cohort_name = 
      case_when(index_cohort_name == "drugs_for_constipation" ~ "laxatives",
                T ~ index_cohort_name),
    marker_cohort_name = 
      case_when(marker_cohort_name == "drugs_for_constipation" ~ "laxatives",
                T ~ marker_cohort_name)
  )

index_names_positive <- positive_controls |>
  dplyr::pull("index_cohort_name")

marker_names_positive <- positive_controls |>
  dplyr::pull("marker_cohort_name")

for (i in (1:length(index_names_positive))){
  tictoc::tic()
  for (a in cohortDateRange){
    if (
      cohortDateRangeCheck(cdm = cdm,
                           cdm[[index_names_positive[[i]]]],
                           cohortDateRange = a)
    )
      next
    
    if (
      cohortDateRangeCheck(cdm = cdm,
                           cdm[[marker_names_positive[[i]]]],
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
                                                               name = paste0(substring(index_names_positive[[i]],1,5), "_", substring(marker_names_positive[[i]],1,5)),
                                                               cohortDateRange = a,
                                                               indexTable = index_names_positive[[i]],
                                                               markerTable = marker_names_positive[[i]],
                                                               daysPriorObservation = b,
                                                               washoutWindow = c,
                                                               indexMarkerGap = d, 
                                                               combinationWindow = e,
                                                               movingAverageRestriction = f)
              
              if (
                cdm[[paste0(substring(index_names_positive[[i]],1,5), "_", substring(marker_names_positive[[i]],1,5))]] |>
                dplyr::summarise(n = n_distinct(cohort_definition_id)) |>
                dplyr::pull("n") == 0 
              ) next
              
              res <- CohortSymmetry::summariseSequenceRatios(cdm[[paste0(substring(index_names_positive[[i]],1,5), "_", substring(marker_names_positive[[i]],1,5))]],
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

### 
log("- Starting negative control on varying parameters")
negative_results_varying_parameter <- omopgenerics::emptySummarisedResult()

negative_controls <- results$result |>
  omopgenerics::filterSettings(ground_truth == "0") |>
  visOmopResults::tidy() |>
  dplyr::select("index_cohort_name", "marker_cohort_name") |>
  dplyr::distinct() |>
  dplyr::mutate(
    index_cohort_name = stringr::str_replace(index_cohort_name, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", ""),
    marker_cohort_name = stringr::str_replace(marker_cohort_name, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", "")
  ) |>
  dplyr::mutate(
    index_cohort_name = 
      case_when(index_cohort_name == "benzodiazepine_derivatives" ~ "combined_benzodiazepine_derivatives",
                T ~ index_cohort_name),
    marker_cohort_name = 
      case_when(marker_cohort_name == "benzodiazepine_derivatives" ~ "combined_benzodiazepine_derivatives",
                T ~ marker_cohort_name)
  ) |>
  dplyr::mutate(
    index_cohort_name = 
      case_when(index_cohort_name == "antiinflammatory_and_antirheumatic_products_non_steroids" ~ "nsaids",
                T ~ index_cohort_name),
    marker_cohort_name = 
      case_when(marker_cohort_name == "antiinflammatory_and_antirheumatic_products_non_steroids" ~ "nsaids",
                T ~ marker_cohort_name)
  ) |>
  dplyr::mutate(
    index_cohort_name = 
      case_when(index_cohort_name == "ace_inhibitors_plain" ~ "ace_inhibitors",
                T ~ index_cohort_name),
    marker_cohort_name = 
      case_when(marker_cohort_name == "ace_inhibitors_plain" ~ "ace_inhibitors",
                T ~ marker_cohort_name)
  ) |>
  dplyr::mutate(
    index_cohort_name = 
      case_when(index_cohort_name == "corticosteroids_for_systemic_use" ~ "corticosteroids",
                T ~ index_cohort_name),
    marker_cohort_name = 
      case_when(marker_cohort_name == "corticosteroids_for_systemic_use" ~ "corticosteroids",
                T ~ marker_cohort_name)
  ) |>
  dplyr::mutate(
    index_cohort_name = 
      case_when(index_cohort_name == "benzodiazepine_derivatives_n05ba_benzodiazepine_derivatives_n05cd_benzodiazepine_derivatives" ~ "benzodiazepine_derivatives",
                T ~ index_cohort_name),
    marker_cohort_name = 
      case_when(marker_cohort_name == "benzodiazepine_derivatives_n05ba_benzodiazepine_derivatives_n05cd_benzodiazepine_derivatives" ~ "benzodiazepine_derivatives",
                T ~ marker_cohort_name)
  ) |>
  dplyr::mutate(
    index_cohort_name = 
      case_when(index_cohort_name == "acetylsalicylic_acid_oral_platelet_aggregation_inhibitors_excl_heparin" ~ "acetylsalicylic_acid",
                T ~ index_cohort_name),
    marker_cohort_name = 
      case_when(marker_cohort_name == "acetylsalicylic_acid_oral_platelet_aggregation_inhibitors_excl_heparin" ~ "acetylsalicylic_acid",
                T ~ marker_cohort_name)
  ) |>
  dplyr::mutate(
    index_cohort_name = 
      case_when(index_cohort_name == "insulins_and_analogues" ~ "insulin",
                T ~ index_cohort_name),
    marker_cohort_name = 
      case_when(marker_cohort_name == "insulins_and_analogues" ~ "insulin",
                T ~ marker_cohort_name)
  ) |>
  dplyr::mutate(
    index_cohort_name = 
      case_when(index_cohort_name == "cough_suppressants_excl_combinations_with_expectorants" ~ "antitussive_agents",
                T ~ index_cohort_name),
    marker_cohort_name = 
      case_when(marker_cohort_name == "cough_suppressants_excl_combinations_with_expectorants" ~ "antitussive_agents",
                T ~ marker_cohort_name)
  ) |>
  dplyr::mutate(
    index_cohort_name = 
      case_when(index_cohort_name == "corticosteroids" ~ "steroids",
                T ~ index_cohort_name),
    marker_cohort_name = 
      case_when(marker_cohort_name == "corticosteroids" ~ "steroids",
                T ~ marker_cohort_name)
  ) |> 
  dplyr::mutate(
    index_cohort_name = 
      case_when(index_cohort_name == "insulin" ~ "insulins",
                T ~ index_cohort_name),
    marker_cohort_name = 
      case_when(marker_cohort_name == "insulin" ~ "insulins",
                T ~ marker_cohort_name)
  ) |>
  dplyr::mutate(
    index_cohort_name = 
      case_when(index_cohort_name == "drugs_for_constipation" ~ "laxatives",
                T ~ index_cohort_name),
    marker_cohort_name = 
      case_when(marker_cohort_name == "drugs_for_constipation" ~ "laxatives",
                T ~ marker_cohort_name)
  )

index_names_negative <- negative_controls |>
  dplyr::pull("index_cohort_name")

marker_names_negative <- negative_controls |>
  dplyr::pull("marker_cohort_name")

for (i in (1:length(index_names_negative))){
  tictoc::tic()
  for (a in cohortDateRange){
    if (
      cohortDateRangeCheck(cdm = cdm,
                           cdm[[index_names_negative[[i]]]],
                           cohortDateRange = a)
    )
      next
    
    if (
      cohortDateRangeCheck(cdm = cdm,
                           cdm[[marker_names_negative[[i]]]],
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
                                                               name = paste0(substring(index_names_negative[[i]],1,5), "_", substring(marker_names_negative[[i]],1,5)),
                                                               cohortDateRange = a,
                                                               indexTable = index_names_negative[[i]],
                                                               markerTable = marker_names_negative[[i]],
                                                               daysPriorObservation = b,
                                                               washoutWindow = c,
                                                               indexMarkerGap = d, 
                                                               combinationWindow = e,
                                                               movingAverageRestriction = f)
              
              if (
                cdm[[paste0(substring(index_names_negative[[i]],1,5), "_", substring(marker_names_negative[[i]],1,5))]] |>
                dplyr::summarise(n = n_distinct(cohort_definition_id)) |>
                dplyr::pull("n") == 0 
              ) next
              
              res <- CohortSymmetry::summariseSequenceRatios(cdm[[paste0(substring(index_names_negative[[i]],1,5), "_", substring(marker_names_negative[[i]],1,5))]],
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

setting <- omopgenerics::settings(negative_results_varying_parameter) |>
  dplyr::mutate(ground_truth = 0)

negative_results_varying_parameter <- negative_results_varying_parameter |>
  omopgenerics::newSummarisedResult(
    settings = setting
  )

negative_results_varying_parameter <- negative_results_varying_parameter |>
  omopgenerics::suppress(minCellCount = minCellCount)

omopgenerics::bind(positive_results_varying_parameter,
                   negative_results_varying_parameter) |>
  exportSummarisedResult(fileName = here::here(output_folder, "result_by_parameter_{cdm_name}.csv"))

results[["results_by_parameter"]] <- omopgenerics::bind(positive_results_varying_parameter,
                                                        negative_results_varying_parameter)
