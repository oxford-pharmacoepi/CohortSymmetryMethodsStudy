res <- res |> 
  tidy()

counts <- res |> 
  dplyr::filter(variable_level == "first_pharmac") |>
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
    pair = paste0(index_cohort_name, "->", marker_cohort_name)
  ) |>
  dplyr::mutate(pair = paste0(pair, ", ground_truth = ", ground_truth)) |>
  dplyr::select("pair", "cdm_name", "count") |>
  dplyr::group_by(pair, cdm_name) |>
  dplyr::summarise(count = sum(count, na.rm = T)) |>
  dplyr::ungroup() |>
  pivot_wider(names_from = cdm_name, values_from = count) 

write_csv(counts, "rawCounts.csv")

res2 <- res |>
  dplyr::filter(variable_level == "sequence_ratio") |>
  dplyr::filter(variable_name == "adjusted") |>
  dplyr::select("index_cohort_name", "marker_cohort_name", "point_estimate", "lower_CI", "upper_CI", "ground_truth", "cdm_name") |>
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
    pair = paste0(index_cohort_name, "->", marker_cohort_name)
  ) |>
  dplyr::select("pair", "ground_truth", "cdm_name", "point_estimate", "lower_CI", "upper_CI") |>
  dplyr::filter(is.finite(.data$point_estimate)) |>
  dplyr::mutate_if(is.numeric, round, 2) |>
  dplyr::mutate(output = paste0(point_estimate, " (", lower_CI, ",", upper_CI, ")")) |>
  dplyr::select(-c("point_estimate", "lower_CI", "upper_CI")) |>
  dplyr::mutate(pair = paste0(pair, ", ground_truth = ", ground_truth)) |>
  dplyr::select(-"ground_truth") |>
  pivot_wider(names_from = cdm_name, values_from = output)

write_csv(res2, "MainResults.csv")

subsetting_pairs <- counts |>
  dplyr::filter(
    CPRD_GOLD >= 50,
    THIN_BE >= 50,
    THIN_ES >= 50,
    THIN_FR >= 50,
    THIN_IT >= 50,
    THIN_RO >= 50,
    THIN_UK >= 50
  ) |>
  dplyr::pull("pair")

res3 <- res2 |>
  dplyr::filter(pair %in% subsetting_pairs) |>
  dplyr::arrange(pair)

write_csv(res3, "SubsettedResults.csv")

CPRD_GOLD_parameter <- omopgenerics::bind(negative_results_varying_parameter, positive_results_varying_parameter)
CPRD_GOLD_parameter_obs_365 <- CPRD_GOLD_parameter |> filterSettings(result_id == 9 |result_id == 25)

THIN_BE_parameter <- omopgenerics::bind(negative_results_varying_parameter, positive_results_varying_parameter)
THIN_BE_parameter_obs_365 <- THIN_BE_parameter |> filterSettings(result_id == 9|result_id == 25)

THIN_ES_parameter <- omopgenerics::bind(negative_results_varying_parameter, positive_results_varying_parameter)
THIN_ES_parameter_obs_365 <- THIN_ES_parameter |> filterSettings(result_id == 9|result_id == 25)

THIN_IT_parameter <- omopgenerics::bind(negative_results_varying_parameter, positive_results_varying_parameter)
THIN_IT_parameter_obs_365 <- THIN_IT_parameter |> filterSettings(result_id == 9|result_id == 25)

THIN_FR_parameter <- omopgenerics::bind(negative_results_varying_parameter, positive_results_varying_parameter)
THIN_FR_parameter_obs_365 <- THIN_FR_parameter |> filterSettings(result_id == 9|result_id == 25)

THIN_RO_parameter <- omopgenerics::bind(negative_results_varying_parameter, positive_results_varying_parameter)
THIN_RO_parameter_obs_365 <- THIN_RO_parameter |> filterSettings(result_id == 9|result_id == 25)

THIN_UK_parameter <- omopgenerics::bind(negative_results_varying_parameter, positive_results_varying_parameter)
THIN_UK_parameter_obs_365 <- THIN_UK_parameter |> filterSettings(result_id == 9|result_id == 25)

res_365 <- omopgenerics::bind(
  CPRD_GOLD_parameter_obs_365,
  THIN_BE_parameter_obs_365,
  THIN_ES_parameter_obs_365,
  THIN_FR_parameter_obs_365,
  THIN_IT_parameter_obs_365,
  THIN_UK_parameter_obs_365,
  THIN_RO_parameter_obs_365
) |> tidy()

res_365_2 <- res_365 |>
  dplyr::filter(variable_level == "sequence_ratio") |>
  dplyr::filter(variable_name == "adjusted") |>
  dplyr::select("index_cohort_name", "marker_cohort_name", "point_estimate", "lower_CI", "upper_CI", "ground_truth", "cdm_name") |>
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
    pair = paste0(index_cohort_name, "->", marker_cohort_name)
  ) |>
  dplyr::select("pair", "ground_truth", "cdm_name", "point_estimate", "lower_CI", "upper_CI") |>
  dplyr::filter(is.finite(.data$point_estimate)) |>
  dplyr::mutate_if(is.numeric, round, 2) |>
  dplyr::mutate(output = paste0(point_estimate, " (", lower_CI, ",", upper_CI, ")")) |>
  dplyr::select(-c("point_estimate", "lower_CI", "upper_CI")) |>
  dplyr::mutate(pair = paste0(pair, ", ground_truth = ", ground_truth)) |>
  dplyr::select(-"ground_truth") |>
  pivot_wider(names_from = cdm_name, values_from = output) |>
  dplyr::filter(pair %in% subsetting_pairs) |>
  dplyr::arrange(pair)

write.csv(res_365_2, "SubsettedResultsPrior365.csv")

CPRD_GOLD_parameter_changed_cw <- CPRD_GOLD_parameter |> filterSettings(result_id == 3 |result_id == 19)
THIN_BE_parameter_changed_cw <- THIN_BE_parameter |> filterSettings(result_id == 3 |result_id == 19)
THIN_IT_parameter_changed_cw <- THIN_IT_parameter |> filterSettings(result_id == 3 |result_id == 19)
THIN_FR_parameter_changed_cw <- THIN_FR_parameter |> filterSettings(result_id == 3 |result_id == 19)
THIN_ES_parameter_changed_cw <- THIN_ES_parameter |> filterSettings(result_id == 3 |result_id == 19)
THIN_UK_parameter_changed_cw <- THIN_UK_parameter |> filterSettings(result_id == 3 |result_id == 19)
THIN_RO_parameter_changed_cw <- THIN_RO_parameter |> filterSettings(result_id == 3 |result_id == 19)

res_changed_cw <- omopgenerics::bind(
  CPRD_GOLD_parameter_changed_cw,
  THIN_BE_parameter_changed_cw,
  THIN_IT_parameter_changed_cw,
  THIN_FR_parameter_changed_cw,
  THIN_ES_parameter_changed_cw,
  THIN_UK_parameter_changed_cw,
  THIN_RO_parameter_changed_cw
) |> tidy()

res_changed_cw_2 <- res_changed_cw |>
dplyr::filter(variable_level == "sequence_ratio") |>
  dplyr::filter(variable_name == "adjusted") |>
  dplyr::select("index_cohort_name", "marker_cohort_name", "point_estimate", "lower_CI", "upper_CI", "ground_truth", "cdm_name") |>
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
    pair = paste0(index_cohort_name, "->", marker_cohort_name)
  ) |>
  dplyr::select("pair", "ground_truth", "cdm_name", "point_estimate", "lower_CI", "upper_CI") |>
  dplyr::filter(is.finite(.data$point_estimate)) |>
  dplyr::mutate_if(is.numeric, round, 2) |>
  dplyr::mutate(output = paste0(point_estimate, " (", lower_CI, ",", upper_CI, ")")) |>
  dplyr::select(-c("point_estimate", "lower_CI", "upper_CI")) |>
  dplyr::mutate(pair = paste0(pair, ", ground_truth = ", ground_truth)) |>
  dplyr::select(-"ground_truth") |>
  pivot_wider(names_from = cdm_name, values_from = output) |>
  dplyr::filter(pair %in% subsetting_pairs) |>
  dplyr::arrange(pair)

write_csv(res_changed_cw_2, "SubsettedResultsCombinationWindow_30_730.csv")
