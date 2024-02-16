#################################################
# positive control drugs
#################################################
print("Starting positive control drugs")
info(logger, "GENERATING COHORTS FOR POSITIVE CONTROLS")
print("GENERATING COHORTS FOR amiodarone")
info(logger, "GENERATING COHORTS FOR amiodarone")
# amiodarone
cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(c("amiodarone", "ingredient")),
                           table_name = "amiodarone",
                           start_date = starting_date,
                           end_date = ending_date)

print("GENERATING COHORTS FOR levothyroxine")
info(logger, "GENERATING COHORTS FOR levothyroxine")
# levothyroxine
cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(c("levothyroxine", "ingredient")),
                           table_name = "levothyroxine",
                           start_date = starting_date,
                           end_date = ending_date)

print("Finished positive control drugs")
info(logger, "GENERATED COHORTS FOR POSITIVE CONTROLS")

#################################################
## negative control drug
#################################################
print("Starting negative control drugs")
info(logger, "GENERATING COHORTS FOR NEGATIVE CONTROLS")
print("GENERATING COHORTS FOR Allopurinol")
info(logger, "GENERATING COHORTS FOR Allopurinol")
# Allopurinol
cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(c("allopurinol", "ingredient")),
                           table_name = "allopurinol",
                           start_date = starting_date,
                           end_date = ending_date)

print("Finished negative control drugs")
info(logger, "GENERATED COHORTS FOR NEGATIVE CONTROLS")

###############################################################
## index drugs 
###############################################################
print("Starting index drugs")
info(logger, "GENERATING COHORTS FOR INDEX DRUGS")

print("GENERATING COHORTS FOR ACHE INHIBITORS")
info(logger, "GENERATING COHORTS FOR ACHE INHIBITORS")
# AChE inhibitors (Donepezil, rivastigmine and galantamine)
cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(c("donepezil", "ingredient"),
                                       c("rivastigmine", "ingredient"),
                                       c("galantamine", "ingredient")),
                           table_name = "ache_inhibitors",
                           start_date = starting_date,
                           end_date = ending_date)

print("GENERATING COHORTS FOR Memantine")
info(logger, "GENERATING COHORTS FOR Memantine")
# Memantine
cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(c("memantine", "ingredient")),
                           table_name = "memantine",
                           start_date = starting_date,
                           end_date = ending_date)

print("Finished index drugs")
info(logger, "GENERATED COHORTS FOR INDEX DRUGS")
