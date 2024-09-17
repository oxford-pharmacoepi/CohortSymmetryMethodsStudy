# Set sub output folder location -----
# the path to a folder where the results from this analysis will be saved
suboutput_folder <- here("Results", db_name, "WashoutVariation")

# output files ---- 
if (!file.exists(suboutput_folder)){
  dir.create(suboutput_folder, recursive = TRUE)}

# Positive control analysis: 
positive_control_res_365 <- positive_control_res |>
  visOmopResults::splitGroup() |>
  visOmopResults::filterSettings(result_type == "sequence_ratios") |>
  dplyr::select(-c("cdm_name", "strata_name", "strata_level", "variable_level")) |>
  visOmopResults::splitAdditional() |>
  dplyr::mutate(
    index_cohort_name = 
      case_when(index_cohort_name == "benzodiazepine_derivatives" ~ "combined_benzodiazepine_derivatives",
                T ~ index_cohort_name),
    marker_cohort_name = 
      case_when(marker_cohort_name == "benzodiazepine_derivatives" ~ "combined_benzodiazepine_derivatives",
                T ~ marker_cohort_name)
  ) |>
  dplyr::mutate(
    index_cohort_name = substring(index_cohort_name, regexpr("_", index_cohort_name) + 1, nchar(index_cohort_name)),
    marker_cohort_name = substring(marker_cohort_name, regexpr("_", marker_cohort_name) + 1, nchar(marker_cohort_name))
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
      case_when(index_cohort_name == "benzodiazepine_derivatives_n05ba_benzodiazepine_derivatives_n05cd_benzodiazepine_derivatives" ~ "benzodiazepine_derivatives",
                T ~ index_cohort_name),
    marker_cohort_name = 
      case_when(marker_cohort_name == "benzodiazepine_derivatives_n05ba_benzodiazepine_derivatives_n05cd_benzodiazepine_derivatives" ~ "benzodiazepine_derivatives",
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
  tidyr::pivot_wider(names_from = "estimate_name", values_from = "estimate_value") |>
  dplyr::mutate(group = paste0(index_cohort_name, " -> ", marker_cohort_name)) |>
  dplyr::select(-c("index_cohort_name", "marker_cohort_name")) |>
  dplyr::mutate(
    point_estimate = as.numeric(point_estimate),
    lower_CI = as.numeric(lower_CI),
    upper_CI = as.numeric(upper_CI),
    variable_name = as.factor(variable_name)
  ) |>
  dplyr::select(tidyselect::where( ~ dplyr::n_distinct(.) > 1)|group) |>
  dplyr::filter(variable_name == "adjusted") |>
  dplyr::select(
    "group",
    "asr_washout_365" = "point_estimate"
  )

oxfordRefPositive <- oxfordRef |>
  dplyr::filter(ground_truth == 1)

positive_controls_results_0 <- list()
index_events <- oxfordRefPositive |>
  dplyr::pull("index")
marker_events <- oxfordRefPositive |>
  dplyr::pull("marker")

for (i in (1:length(index_events))){
  tictoc::tic()
  cdm <- CohortSymmetry::generateSequenceCohortSet(cdm = cdm,
                                                   name = paste0(substring(index_events[[i]],1,5), "_", substring(marker_events[[i]],1,5),  "_0"),
                                                   cohortDateRange = c(starting_date, ending_date),
                                                   indexTable = index_events[[i]],
                                                   markerTable = marker_events[[i]],
                                                   daysPriorObservation = 365,
                                                   washoutWindow = 0,
                                                   indexMarkerGap = NULL, # if null it uses the second argument of the combinationWindow
                                                   combinationWindow = c(0, 365))
  
  if (
    cdm[[paste0(substring(index_events[[i]],1,5), "_", substring(marker_events[[i]],1,5), "_0")]] |>
    dplyr::summarise(n = n_distinct(cohort_definition_id)) |>
    dplyr::pull("n") == 0 
  ) next
  
  positive_controls_results_0[[paste0(index_events[[i]], "_", marker_events[[i]])]] <- 
    CohortSymmetry::summariseSequenceRatios(cdm[[paste0(substring(index_events[[i]],1,5), "_", substring(marker_events[[i]],1,5), "_0")]])
  getCohortSeqtime <- tictoc::toc()$callback_msg 
}

positive_control_res_0 <- bind_rows(positive_controls_results_0) |>
  omopgenerics::newSummarisedResult()

saveRDS(positive_control_res_0, file = paste0(suboutput_folder, "/washout_0", "_positive_control_res.rds"))

positive_control_res_0 <- positive_control_res_0 |>
  visOmopResults::splitGroup() |>
  visOmopResults::filterSettings(result_type == "sequence_ratios") |>
  dplyr::select(-c("cdm_name", "strata_name", "strata_level", "variable_level")) |>
  visOmopResults::splitAdditional() |>
  dplyr::mutate(
    index_cohort_name = 
      case_when(index_cohort_name == "benzodiazepine_derivatives" ~ "combined_benzodiazepine_derivatives",
                T ~ index_cohort_name),
    marker_cohort_name = 
      case_when(marker_cohort_name == "benzodiazepine_derivatives" ~ "combined_benzodiazepine_derivatives",
                T ~ marker_cohort_name)
  ) |>
  dplyr::mutate(
    index_cohort_name = substring(index_cohort_name, regexpr("_", index_cohort_name) + 1, nchar(index_cohort_name)),
    marker_cohort_name = substring(marker_cohort_name, regexpr("_", marker_cohort_name) + 1, nchar(marker_cohort_name))
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
      case_when(index_cohort_name == "benzodiazepine_derivatives_n05ba_benzodiazepine_derivatives_n05cd_benzodiazepine_derivatives" ~ "benzodiazepine_derivatives",
                T ~ index_cohort_name),
    marker_cohort_name = 
      case_when(marker_cohort_name == "benzodiazepine_derivatives_n05ba_benzodiazepine_derivatives_n05cd_benzodiazepine_derivatives" ~ "benzodiazepine_derivatives",
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
  tidyr::pivot_wider(names_from = "estimate_name", values_from = "estimate_value") |>
  dplyr::mutate(group = paste0(index_cohort_name, " -> ", marker_cohort_name)) |>
  dplyr::select(-c("index_cohort_name", "marker_cohort_name")) |>
  dplyr::mutate(
    point_estimate = as.numeric(point_estimate),
    lower_CI = as.numeric(lower_CI),
    upper_CI = as.numeric(upper_CI),
    variable_name = as.factor(variable_name)
  ) |>
  dplyr::select(tidyselect::where( ~ dplyr::n_distinct(.) > 1)|group) |>
  dplyr::filter(variable_name == "adjusted") |>
  dplyr::select(
    "group",
    "asr_washout_0" = "point_estimate"
  )

positive_controls_results_730 <- list()
index_events <- oxfordRefPositive |>
  dplyr::pull("index")
marker_events <- oxfordRefPositive |>
  dplyr::pull("marker")

for (i in (1:length(index_events))){
  tictoc::tic()
  cdm <- CohortSymmetry::generateSequenceCohortSet(cdm = cdm,
                                                   name = paste0(substring(index_events[[i]],1,5), "_", substring(marker_events[[i]],1,5),  "_730"),
                                                   cohortDateRange = c(starting_date, ending_date),
                                                   indexTable = index_events[[i]],
                                                   markerTable = marker_events[[i]],
                                                   daysPriorObservation = 365,
                                                   washoutWindow = 730,
                                                   indexMarkerGap = NULL, # if null it uses the second argument of the combinationWindow
                                                   combinationWindow = c(0, 365))
  
  if (
    cdm[[paste0(substring(index_events[[i]],1,5), "_", substring(marker_events[[i]],1,5), "_730")]] |>
    dplyr::summarise(n = n_distinct(cohort_definition_id)) |>
    dplyr::pull("n") == 0 
  ) next
  
  positive_controls_results_730[[paste0(index_events[[i]], "_", marker_events[[i]])]] <- 
    CohortSymmetry::summariseSequenceRatios(cdm[[paste0(substring(index_events[[i]],1,5), "_", substring(marker_events[[i]],1,5), "_730")]])
  getCohortSeqtime <- tictoc::toc()$callback_msg 
}

positive_control_res_730 <- bind_rows(positive_controls_results_730) |>
  omopgenerics::newSummarisedResult()

saveRDS(positive_control_res_730, file = paste0(suboutput_folder, "/washout_730", "_positive_control_res.rds"))

positive_control_res_730 <- positive_control_res_730 |>
  visOmopResults::splitGroup() |>
  visOmopResults::filterSettings(result_type == "sequence_ratios") |>
  dplyr::select(-c("cdm_name", "strata_name", "strata_level", "variable_level")) |>
  visOmopResults::splitAdditional() |>
  dplyr::mutate(
    index_cohort_name = 
      case_when(index_cohort_name == "benzodiazepine_derivatives" ~ "combined_benzodiazepine_derivatives",
                T ~ index_cohort_name),
    marker_cohort_name = 
      case_when(marker_cohort_name == "benzodiazepine_derivatives" ~ "combined_benzodiazepine_derivatives",
                T ~ marker_cohort_name)
  ) |>
  dplyr::mutate(
    index_cohort_name = substring(index_cohort_name, regexpr("_", index_cohort_name) + 1, nchar(index_cohort_name)),
    marker_cohort_name = substring(marker_cohort_name, regexpr("_", marker_cohort_name) + 1, nchar(marker_cohort_name))
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
      case_when(index_cohort_name == "benzodiazepine_derivatives_n05ba_benzodiazepine_derivatives_n05cd_benzodiazepine_derivatives" ~ "benzodiazepine_derivatives",
                T ~ index_cohort_name),
    marker_cohort_name = 
      case_when(marker_cohort_name == "benzodiazepine_derivatives_n05ba_benzodiazepine_derivatives_n05cd_benzodiazepine_derivatives" ~ "benzodiazepine_derivatives",
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
  tidyr::pivot_wider(names_from = "estimate_name", values_from = "estimate_value") |>
  dplyr::mutate(group = paste0(index_cohort_name, " -> ", marker_cohort_name)) |>
  dplyr::select(-c("index_cohort_name", "marker_cohort_name")) |>
  dplyr::mutate(
    point_estimate = as.numeric(point_estimate),
    lower_CI = as.numeric(lower_CI),
    upper_CI = as.numeric(upper_CI),
    variable_name = as.factor(variable_name)
  ) |>
  dplyr::select(tidyselect::where( ~ dplyr::n_distinct(.) > 1)|group) |>
  dplyr::filter(variable_name == "adjusted") |>
  dplyr::select(
    "group",
    "asr_washout_730" = "point_estimate"
  )

positive_control_washout <- positive_control_res_0 |>
  dplyr::inner_join(positive_control_res_365, by = "group") |>
  dplyr::inner_join(positive_control_res_730, by = "group") |>
  dplyr::filter(!(is.na(asr_washout_0))|!(is.na(asr_washout_365))|!(is.na(asr_washout_730)))

# Negative control analysis: 
negative_control_res_365 <- negative_control_res |>
  visOmopResults::splitGroup() |>
  visOmopResults::filterSettings(result_type == "sequence_ratios") |>
  dplyr::select(-c("cdm_name", "strata_name", "strata_level", "variable_level")) |>
  visOmopResults::splitAdditional() |>
  dplyr::mutate(
    index_cohort_name = 
      case_when(index_cohort_name == "benzodiazepine_derivatives" ~ "combined_benzodiazepine_derivatives",
                T ~ index_cohort_name),
    marker_cohort_name = 
      case_when(marker_cohort_name == "benzodiazepine_derivatives" ~ "combined_benzodiazepine_derivatives",
                T ~ marker_cohort_name)
  ) |>
  dplyr::mutate(
    index_cohort_name = substring(index_cohort_name, regexpr("_", index_cohort_name) + 1, nchar(index_cohort_name)),
    marker_cohort_name = substring(marker_cohort_name, regexpr("_", marker_cohort_name) + 1, nchar(marker_cohort_name))
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
      case_when(index_cohort_name == "benzodiazepine_derivatives_n05ba_benzodiazepine_derivatives_n05cd_benzodiazepine_derivatives" ~ "benzodiazepine_derivatives",
                T ~ index_cohort_name),
    marker_cohort_name = 
      case_when(marker_cohort_name == "benzodiazepine_derivatives_n05ba_benzodiazepine_derivatives_n05cd_benzodiazepine_derivatives" ~ "benzodiazepine_derivatives",
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
  tidyr::pivot_wider(names_from = "estimate_name", values_from = "estimate_value") |>
  dplyr::mutate(group = paste0(index_cohort_name, " -> ", marker_cohort_name)) |>
  dplyr::select(-c("index_cohort_name", "marker_cohort_name")) |>
  dplyr::mutate(
    point_estimate = as.numeric(point_estimate),
    lower_CI = as.numeric(lower_CI),
    upper_CI = as.numeric(upper_CI),
    variable_name = as.factor(variable_name)
  ) |>
  dplyr::select(tidyselect::where( ~ dplyr::n_distinct(.) > 1)|group) |>
  dplyr::filter(variable_name == "adjusted") |>
  dplyr::select(
    "group",
    "asr_washout_365" = "point_estimate"
  )

oxfordRefNegative <- oxfordRef |>
  dplyr::filter(ground_truth == 0)

negative_controls_results_0 <- list()
index_events <- oxfordRefNegative |>
  dplyr::pull("index")
marker_events <- oxfordRefNegative |>
  dplyr::pull("marker")

for (i in (1:length(index_events))){
  tictoc::tic()
  cdm <- CohortSymmetry::generateSequenceCohortSet(cdm = cdm,
                                                   name = paste0(substring(index_events[[i]],1,5), "_", substring(marker_events[[i]],1,5),  "_0"),
                                                   cohortDateRange = c(starting_date, ending_date),
                                                   indexTable = index_events[[i]],
                                                   markerTable = marker_events[[i]],
                                                   daysPriorObservation = 365,
                                                   washoutWindow = 0,
                                                   indexMarkerGap = NULL, # if null it uses the second argument of the combinationWindow
                                                   combinationWindow = c(0, 365))
  
  if (
    cdm[[paste0(substring(index_events[[i]],1,5), "_", substring(marker_events[[i]],1,5), "_0")]] |>
    dplyr::summarise(n = n_distinct(cohort_definition_id)) |>
    dplyr::pull("n") == 0 
  ) next
  
  negative_controls_results_0[[paste0(index_events[[i]], "_", marker_events[[i]])]] <- 
    CohortSymmetry::summariseSequenceRatios(cdm[[paste0(substring(index_events[[i]],1,5), "_", substring(marker_events[[i]],1,5), "_0")]])
  getCohortSeqtime <- tictoc::toc()$callback_msg 
}

negative_control_res_0 <- bind_rows(negative_controls_results_0) |>
  omopgenerics::newSummarisedResult()

saveRDS(negative_control_res_0, file = paste0(suboutput_folder, "/washout_0", "_negative_control_res.rds"))

negative_control_res_0 <- negative_control_res_0 |>
  visOmopResults::splitGroup() |>
  visOmopResults::filterSettings(result_type == "sequence_ratios") |>
  dplyr::select(-c("cdm_name", "strata_name", "strata_level", "variable_level")) |>
  visOmopResults::splitAdditional() |>
  dplyr::mutate(
    index_cohort_name = 
      case_when(index_cohort_name == "benzodiazepine_derivatives" ~ "combined_benzodiazepine_derivatives",
                T ~ index_cohort_name),
    marker_cohort_name = 
      case_when(marker_cohort_name == "benzodiazepine_derivatives" ~ "combined_benzodiazepine_derivatives",
                T ~ marker_cohort_name)
  ) |>
  dplyr::mutate(
    index_cohort_name = substring(index_cohort_name, regexpr("_", index_cohort_name) + 1, nchar(index_cohort_name)),
    marker_cohort_name = substring(marker_cohort_name, regexpr("_", marker_cohort_name) + 1, nchar(marker_cohort_name))
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
      case_when(index_cohort_name == "benzodiazepine_derivatives_n05ba_benzodiazepine_derivatives_n05cd_benzodiazepine_derivatives" ~ "benzodiazepine_derivatives",
                T ~ index_cohort_name),
    marker_cohort_name = 
      case_when(marker_cohort_name == "benzodiazepine_derivatives_n05ba_benzodiazepine_derivatives_n05cd_benzodiazepine_derivatives" ~ "benzodiazepine_derivatives",
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
  tidyr::pivot_wider(names_from = "estimate_name", values_from = "estimate_value") |>
  dplyr::mutate(group = paste0(index_cohort_name, " -> ", marker_cohort_name)) |>
  dplyr::select(-c("index_cohort_name", "marker_cohort_name")) |>
  dplyr::mutate(
    point_estimate = as.numeric(point_estimate),
    lower_CI = as.numeric(lower_CI),
    upper_CI = as.numeric(upper_CI),
    variable_name = as.factor(variable_name)
  ) |>
  dplyr::select(tidyselect::where( ~ dplyr::n_distinct(.) > 1)|group) |>
  dplyr::filter(variable_name == "adjusted") |>
  dplyr::select(
    "group",
    "asr_washout_0" = "point_estimate"
  )

negative_controls_results_730 <- list()
index_events <- oxfordRefNegative |>
  dplyr::pull("index")
marker_events <- oxfordRefNegative |>
  dplyr::pull("marker")

for (i in (1:length(index_events))){
  tictoc::tic()
  cdm <- CohortSymmetry::generateSequenceCohortSet(cdm = cdm,
                                                   name = paste0(substring(index_events[[i]],1,5), "_", substring(marker_events[[i]],1,5),  "_730"),
                                                   cohortDateRange = c(starting_date, ending_date),
                                                   indexTable = index_events[[i]],
                                                   markerTable = marker_events[[i]],
                                                   daysPriorObservation = 365,
                                                   washoutWindow = 730,
                                                   indexMarkerGap = NULL, # if null it uses the second argument of the combinationWindow
                                                   combinationWindow = c(0, 365))
  
  if (
    cdm[[paste0(substring(index_events[[i]],1,5), "_", substring(marker_events[[i]],1,5), "_730")]] |>
    dplyr::summarise(n = n_distinct(cohort_definition_id)) |>
    dplyr::pull("n") == 0 
  ) next
  
  negative_controls_results_730[[paste0(index_events[[i]], "_", marker_events[[i]])]] <- 
    CohortSymmetry::summariseSequenceRatios(cdm[[paste0(substring(index_events[[i]],1,5), "_", substring(marker_events[[i]],1,5), "_730")]])
  getCohortSeqtime <- tictoc::toc()$callback_msg 
}

negative_control_res_730 <- bind_rows(negative_controls_results_730) |>
  omopgenerics::newSummarisedResult()

saveRDS(negative_control_res_730, file = paste0(suboutput_folder, "/washout_730", "_negative_control_res.rds"))

negative_control_res_730 <- negative_control_res_730 |>
  visOmopResults::splitGroup() |>
  visOmopResults::filterSettings(result_type == "sequence_ratios") |>
  dplyr::select(-c("cdm_name", "strata_name", "strata_level", "variable_level")) |>
  visOmopResults::splitAdditional() |>
  dplyr::mutate(
    index_cohort_name = 
      case_when(index_cohort_name == "benzodiazepine_derivatives" ~ "combined_benzodiazepine_derivatives",
                T ~ index_cohort_name),
    marker_cohort_name = 
      case_when(marker_cohort_name == "benzodiazepine_derivatives" ~ "combined_benzodiazepine_derivatives",
                T ~ marker_cohort_name)
  ) |>
  dplyr::mutate(
    index_cohort_name = substring(index_cohort_name, regexpr("_", index_cohort_name) + 1, nchar(index_cohort_name)),
    marker_cohort_name = substring(marker_cohort_name, regexpr("_", marker_cohort_name) + 1, nchar(marker_cohort_name))
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
      case_when(index_cohort_name == "benzodiazepine_derivatives_n05ba_benzodiazepine_derivatives_n05cd_benzodiazepine_derivatives" ~ "benzodiazepine_derivatives",
                T ~ index_cohort_name),
    marker_cohort_name = 
      case_when(marker_cohort_name == "benzodiazepine_derivatives_n05ba_benzodiazepine_derivatives_n05cd_benzodiazepine_derivatives" ~ "benzodiazepine_derivatives",
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
  tidyr::pivot_wider(names_from = "estimate_name", values_from = "estimate_value") |>
  dplyr::mutate(group = paste0(index_cohort_name, " -> ", marker_cohort_name)) |>
  dplyr::select(-c("index_cohort_name", "marker_cohort_name")) |>
  dplyr::mutate(
    point_estimate = as.numeric(point_estimate),
    lower_CI = as.numeric(lower_CI),
    upper_CI = as.numeric(upper_CI),
    variable_name = as.factor(variable_name)
  ) |>
  dplyr::select(tidyselect::where( ~ dplyr::n_distinct(.) > 1)|group) |>
  dplyr::filter(variable_name == "adjusted") |>
  dplyr::select(
    "group",
    "asr_washout_730" = "point_estimate"
  )

negative_control_washout <- negative_control_res_0 |>
  dplyr::inner_join(negative_control_res_365, by = "group") |>
  dplyr::inner_join(negative_control_res_730, by = "group") |>
  dplyr::filter(!(is.na(asr_washout_0))|!(is.na(asr_washout_365))|!(is.na(asr_washout_730)))

### plots
custom_palette <- colorRampPalette(c("cyan", "white", "red"))(300)
controls_washout <- rbind(positive_control_washout, negative_control_washout) |>
  dplyr::mutate(asr_washout_0 = case_when(
    asr_washout_0 == 0 ~ NA,
    T ~ asr_washout_0)) |>
  dplyr::mutate(asr_washout_365 = case_when(
    asr_washout_365 == 0 ~ NA,
    T ~ asr_washout_365)) |>
  dplyr::mutate(asr_washout_730 = case_when(
    asr_washout_730 == 0 ~ NA,
    T ~ asr_washout_730))|>
  dplyr::mutate(asr_washout_0 = log(asr_washout_0),
                asr_washout_365 = log(asr_washout_365),
                asr_washout_730 = log(asr_washout_730)) |>
  dplyr::rename(`washout of 0` = "asr_washout_0",
                `washout of 365` = "asr_washout_365",
                `washout of 730` = "asr_washout_730")
rnames <- controls_washout[,1] %>% dplyr::pull()
controls_washout_mat <- data.matrix(controls_washout[,2:ncol(controls_washout)])
rownames(controls_washout_mat) <- rnames
# Create an annotation data frame
annotation_df <- data.frame(
  Group = factor(
    c(rep("Positive Controls", nrow(positive_control_washout)),
    rep("Negative Controls", nrow(negative_control_washout))))
)
annotation_colors <- list(
  Group = c("Positive Controls" = "green", "Negative Controls" = "purple")
)
rownames(annotation_df) <- rownames(controls_washout_mat)  
pheatmap(controls_washout_mat,
         cluster_rows = FALSE, 
         cluster_cols = FALSE,
         display_numbers = TRUE, 
         number_format = "%.2f",
         main = "Figure 3: log(asr), varying washout parameter in positive and negative controls",
         angle_col = 0,
         color = custom_palette,
         annotation_row = annotation_df,
         annotation_colors = annotation_colors)

