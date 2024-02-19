cli::cli_alert_info("- Getting benchmarker definitions drug - drug")

# positive controls -------
cli::cli_alert_info("- Getting benchmarker definitions drug - drug positive controls")

cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(c("amiodarone", "ingredient")),
                           table_name = "amiodarone",
                           start_date = starting_date,
                           end_date = ending_date)

cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(c("levothyroxine", "ingredient")),
                           table_name = "levothyroxine",
                           start_date = starting_date,
                           end_date = ending_date)


cli::cli_alert_success("- Got benchmarker definitions drug - drug")

# negative controls -------
cli::cli_alert_info("- Getting benchmarker definitions drug - drug negative controls")

cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(c("allopurinol", "ingredient")),
                           table_name = "allopurinol",
                           start_date = starting_date,
                           end_date = ending_date)

cli::cli_alert_success("- Got benchmarker definitions drug - drug")


cli::cli_alert_info("- Getting benchmarker definitions drug - condition")
