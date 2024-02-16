# functions #################################################################
# this function takes the ingredient or ATC level and extracts the concepts using
# codelist generator then instantiates the cohorts using drug utilisation package
getSingleDrugCohort <- function(cdm, drug, table_name, start_date, end_date){
  drug_name <- list()
  
  for (i in (1: length(drug))){
    if (drug[[i]][2] == "ingredient"){
      drug_name[[i]] <- getDrugIngredientCodes(cdm = cdm, name = drug[[i]][1])
    } else {
      drug_name[[i]] <- getATCCodes(cdm = cdm, name = drug[[i]][1], level = c(drug[[i]][2]))
    }
  }
  
  conceptSet <- c()
  for (i in (1:length(drug_name))){
    conceptSet <- c(conceptSet, drug_name[[i]])
  }
  
  cdm <- generateDrugUtilisationCohortSet(
    cdm = cdm,
    name = table_name,
    conceptSet = conceptSet,
    cohortDateRange = as.Date(c(start_date, end_date))
  )
  
  return(cdm)
}