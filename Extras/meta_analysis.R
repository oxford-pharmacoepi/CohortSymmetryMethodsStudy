library(metafor)

# result <- result |> dplyr::select(-1)

df <- result %>%
  rename(
    "Pair" = "pair",
    "Control" = "control",
    "CPRD GOLD" = "CPRD_GOLD",
    "THIN Belgium" = "THIN_BE",
    "THIN Spain" = "THIN_ES",
    "THIN Italy" = "THIN_IT",
    "THIN Romania" = "THIN_RO",
    "THIN UK" = "THIN_UK"
  )

# Create a named vector for replacements
term_map <- c(
  "ace_inhibitors" = "ACE inhibitors",
  "aceclofenac" = "Aceclofenac",
  "acute_myocardial_infarction" = "AMI",
  "allopurinol" = "Allopurinol",
  "amiodarone" = "Amiodarone",
  "anaemia" = "Anaemia",
  "antitussive_agents" = "Antitussives",
  "aromatase_inhibitors" = "Aromatase inhibitors",
  "atrial_fibrillation" = "AF",
  "bisphosphonates" = "Bisphosphonates",
  "calcium_channel_blockers" = "CCBs",
  "carbamazepine" = "Carbamazepine",
  "combined_benzodiazepine_derivatives" = "Benzodiazepines",
  "constipation" = "Constipation",
  "corticosteroids" = "Corticosteroids",
  "cough" = "Cough",
  "cystitis" = "Cystitis",
  "diazepam" = "Diazepam",
  "diuretics" = "Diuretics",
  "drugs_for_constipation" = "Constipation agents",
  "epilepsy" = "Epilepsy",
  "fall" = "Falls",
  "fosinopril" = "Fosinopril",
  "fracture" = "Fracture",
  "gastrointestinal_hemorrhage" = "GI bleeding",
  "gastrointestinal_issues_reflux" = "GORD",
  "glipizide" = "Glipizide",
  "hypercholesterolaemia" = "High cholesterol",
  "hyperglycaemia" = "Hyperglycaemia",
  "hypoglycaemia" = "Hypoglycaemia",
  "insulin" = "Insulin",
  "levothyroxine" = "Levothyroxine",
  "methimazole" = "Methimazole",
  "nsaids" = "NSAIDs",
  "ondansetron" = "Ondansetron",
  "opioids" = "Opioids",
  "phenobarbital" = "Phenobarbital",
  "phenytoin" = "Phenytoin",
  "proton_pump_inhibitors" = "PPIs",
  "rosuvastatin" = "Rosuvastatin",
  "simvastatin" = "Simvastatin",
  "stroke" = "Stroke",
  "sulfasalazine" = "Sulfasalazine",
  "valproate" = "Valproate",
  "venous_thrombosis" = "VTE",
  "zopiclone" = "Zopiclone"
)

# Convert `Pair` strings
df <- df %>%
  mutate(
    Pair = str_split(Pair, "->", simplify = TRUE),
    Pair = paste(
      term_map[Pair[, 1]],
      "\u2192",  # Unicode for "→"
      term_map[Pair[, 2]]
    )
  ) %>% 
  mutate(pair_id = row_number()) %>%
  pivot_longer(
    cols = 3:8,
    names_to = "database",
    values_to = "sr_ci"
  )


# Remove non-numeric entries like "<50"
long_df <- df %>%
  filter(!grepl("<", sr_ci)) %>%
  mutate(
    sr      = as.numeric(gsub(" .*", "", sr_ci)),
    ci_low  = as.numeric(gsub(".*\\(([^,]+),.*", "\\1", sr_ci)),
    ci_high = as.numeric(gsub(".*,(.*)\\).*", "\\1", sr_ci))
  )

long_df <- long_df %>%
  mutate(
    log_sr  = log(sr),
    se_log_sr = (log(ci_high) - log(ci_low)) / (2 * 1.96)
  )

meta <- list()

pairs <- long_df |> dplyr::distinct(Pair) |> dplyr::pull("Pair")

for (x in pairs){
  sub_res <- long_df |> dplyr::filter(Pair == x) |>
    mutate(database = recode(database,
                             "CPRD_GOLD" = "CPRD GOLD",
                             "THIN_IT" = "THIN Italy",
                             "THIN_RO" = "THIN Romania",
                             "THIN_ES" = "THIN Spain",
                             "THIN_BE" = "THIN Belgium",
                             "THIN_UK" = "THIN UK"))
  
  c1 <- sub_res |> dplyr::distinct(Control) |> dplyr::pull("Control")
  
  if (nrow(sub_res) < 6) next
  meta[[x]] <- metagen(TE = log_sr,
                       seTE = se_log_sr,
                       data = sub_res,
                       studlab = database,
                       sm = "RR",       # label for relative risk, here just a name
                       method.tau = "REML")
  
  png(paste0("meta_", x, ".png"), width = 4800, height = 3600, res = 600)
  meta::forest(meta[[x]], common = FALSE)
  grid.text(paste0("Meta Analysis Results of ", x, " (",c1, " Control" ,")"), x = 0.5, y = 0.8, gp = gpar(fontsize = 14, fontface = "bold"))
  dev.off()
}