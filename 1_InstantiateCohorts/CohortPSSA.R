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

###############################################################
# marker drugs to test 
###############################################################
print("Starting test drugs")
info(logger, "GENERATING COHORTS FOR MARKER DRUGS")
##################
# instantiated
#################
# Angiotensin receptor blockers (ARB) CO9CA
print("GENERATING COHORTS FOR ARBs")
info(logger, "GENERATING COHORTS FOR ARBs")
cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(
                             c("azilsartan", "ingredient"),
                             c("candesartan", "ingredient"),
                             c("eprosartan", "ingredient"),
                             c("fimasartan potassium", "ingredient"),
                             c("irbesartan", "ingredient"),
                             c("losartan", "ingredient"),
                             c("olmesartan", "ingredient"),
                             c("telmisartan", "ingredient"),
                             c("valsartan", "ingredient") ),
                           table_name = "arbs",
                           start_date = starting_date,
                           end_date = ending_date)

info(logger, "GENERATING COHORTS FOR antipropulsives")
print("GENERATING COHORTS FOR antipropulsives")
# antipropulsives A07DA
cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(
                             c("eluxadoline", "ingredient"),
                             c("morphine", "ingredient"),
                             c("loperamide", "ingredient"),
                             c("diphenoxylate", "ingredient"),
                             c("opium", "ingredient"),
                             c("difenoxin", "ingredient")),
                           table_name = "antipropulsives",
                           start_date = starting_date,
                           end_date = ending_date)

print("GENERATING COHORTS FOR antiemetics 5HT3 antagonists")
info(logger, "GENERATING COHORTS FOR antiemetics 5HT3 antagonists")
#ANTIEMETICS A04AA 5HT3 antagonists
cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(
                             c("dolasetron", "ingredient"),
                             c("granisetron", "ingredient"),
                             c("ondansetron", "ingredient"),
                             c("palonosetron", "ingredient"),
                             c("tropisetron", "ingredient")
                           ),
                           table_name = "antiemetics_5ht3",
                           start_date = starting_date,
                           end_date = ending_date)


print("GENERATING COHORTS FOR Thioxanthene derivatives antipsychotics")
info(logger, "GENERATING COHORTS FOR Thioxanthene derivatives antipsychotics")
#N05AF Thioxanthene derivatives
cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(
                             c("thiothixene", "ingredient"),
                             c("flupenthixol", "ingredient"),
                             c("zuclopenthixol", "ingredient"),
                             c("chlorprothixene", "ingredient") ),
                           table_name = "antipsych_thioxanthene",
                           start_date = starting_date,
                           end_date = ending_date)

print("GENERATING COHORTS FOR Diphenylbutylpiperidine derivatives antipsychotics")
info(logger, "GENERATING COHORTS FOR Diphenylbutylpiperidine derivatives antipsychotics")
#N05AG Diphenylbutylpiperidine derivatives
cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(
                             c("pimozide", "ingredient"),
                             c("penfluridol", "ingredient"),
                             c("fluspirilene", "ingredient")),
                           table_name = "antipsych_diphenylbutylpipe",
                           start_date = starting_date,
                           end_date = ending_date)

print("GENERATING COHORTS FOR Diazepines, oxazepines, thiazepines and oxepines antipsychotics")
info(logger, "GENERATING COHORTS FOR Diazepines, oxazepines, thiazepines and oxepines antipsychotics")
#N05AH Diazepines, oxazepines, thiazepines and oxepines
cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(
                             c("asenapine", "ingredient"),
                             c("loxapine", "ingredient"),
                             c("quetiapine", "ingredient"),
                             c("clozapine", "ingredient"),
                             c("clothiapine", "ingredient"),
                             c("olanzapine", "ingredient")),
                           table_name = "antipsych_diazepines",
                           start_date = starting_date,
                           end_date = ending_date)

print("GENERATING COHORTS FOR Benzamides antipsychotics")
info(logger, "GENERATING COHORTS FOR Benzamides antipsychotics")
#N05AL Benzamides
cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(
                             c("sulpiride", "ingredient"),
                             c("levosulpiride", "ingredient"),
                             c("amisulpride", "ingredient"),
                             c("tiapride", "ingredient"),
                             c("sultopride", "ingredient"),
                             c("veralipride", "ingredient"),
                             c("remoxipride", "ingredient") ),
                           table_name = "antipsych_benzamides",
                           start_date = starting_date,
                           end_date = ending_date)

print("GENERATING COHORTS FOR Lithium antipsychotics")
info(logger, "GENERATING COHORTS FOR Lithium antipsychotics")
#N05AN Lithium
cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(
                             c("lithium", "ingredient"),
                             c("lithium citrate", "ingredient"),
                             c("lithium carbonate", "ingredient")),
                           table_name = "antipsych_lithium",
                           start_date = starting_date,
                           end_date = ending_date)


print("GENERATING COHORTS FOR antiepileptics")
info(logger, "GENERATING COHORTS FOR antiepileptics")
print("GENERATING COHORTS FOR Barbiturates and derivatives antiepileptics")
info(logger, "GENERATING COHORTS FOR Barbiturates and derivatives antiepileptics")
# N03AA Barbiturates and derivatives
cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(
                             c("barbexaclone", "ingredient"),
                             c("phenobarbital", "ingredient"),
                             c("primidone", "ingredient"),
                             c("mephobarbital", "ingredient"),
                             c("metharbital", "ingredient")),
                           table_name = "antiepileptics_barbiturates",
                           start_date = starting_date,
                           end_date = ending_date)

print("GENERATING COHORTS FOR Hydantoin derivatives antiepileptics")
info(logger, "GENERATING COHORTS FOR Hydantoin derivatives antiepileptics")
# N03AB Hydantoin derivatives
cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(
                             c("mephenytoin", "ingredient"),
                             c("fosphenytoin", "ingredient"),
                             c("ethotoin", "ingredient"),
                             c("phenytoin", "ingredient")),
                           table_name = "antiepileptics_hydantoin",
                           start_date = starting_date,
                           end_date = ending_date)



# N03AC Oxazolidine derivatives (no cases)

print("GENERATING COHORTS FOR Succinimide derivatives antiepileptics")
info(logger, "GENERATING COHORTS FOR Succinimide derivatives antiepileptics")
# N03AD Succinimide derivatives
cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(
                             c("ethosuximide", "ingredient"),
                             c("methsuximide", "ingredient"),
                             c("phensuximide", "ingredient")),
                           table_name = "antiepileptics_succinimide",
                           start_date = starting_date,
                           end_date = ending_date)


print("GENERATING COHORTS FOR Benzodiazepine derivatives antiepileptics")
info(logger, "GENERATING COHORTS FOR Benzodiazepine derivatives antiepileptics")
# N03AE Benzodiazepine derivatives
cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(
                             c("clonazepam", "ingredient")),
                           table_name = "antiepileptics_bzds",
                           start_date = starting_date,
                           end_date = ending_date)

print("GENERATING COHORTS FOR Carboxamide derivatives antiepileptics")
info(logger, "GENERATING COHORTS FOR Carboxamide derivatives antiepileptics")
# N03AF Carboxamide derivatives
cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(
                             c("eslicarbazepine", "ingredient"),
                             c("rufinamide", "ingredient"),
                             c("carbamazepine", "ingredient"),
                             c("oxcarbazepine", "ingredient")),
                           table_name = "antiepileptics_carboxamides",
                           start_date = starting_date,
                           end_date = ending_date)

print("GENERATING COHORTS FOR anti depressants Monoamine oxidase A inhibitors")
info(logger, "GENERATING COHORTS FOR anti depressants Monoamine oxidase A inhibitors")
# N06AG Monoamine oxidase A inhibitors
cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(
                             c("toloxatone", "ingredient"),
                             c("moclobemide", "ingredient")),
                           table_name = "antidep_moai",
                           start_date = starting_date,
                           end_date = ending_date)

print("GENERATING COHORTS FOR anti depressants Monoamine oxidase inhibitors, non-selective")
info(logger, "GENERATING COHORTS FOR anti depressants Monoamine oxidase inhibitors, non-selective")
# N06AF Monoamine oxidase inhibitors, non-selective
cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(
                             c("phenelzine", "ingredient"),
                             c("iproniazid", "ingredient"),
                             c("isocarboxazid", "ingredient"),
                             c("nialamide", "ingredient"),
                             c("tranylcypromine", "ingredient")),
                           table_name = "antidep_moins",
                           start_date = starting_date,
                           end_date = ending_date)

print("GENERATING COHORTS FOR other antiemetics")
info(logger, "GENERATING COHORTS FOR other antiemetics")
#ANTIEMETICS A04AD
cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(
                             c("rolapitant", "ingredient"),
                             c("nabilone", "ingredient"),
                             c("cerium oxalate", "ingredient"),
                             c("chlorobutanol", "ingredient"),
                             c("dronabinol", "ingredient"),
                             c("metopimazine", "ingredient"),
                             c("aprepitant", "ingredient"),
                             c("scopolamine", "ingredient") ),
                           table_name = "antiemetics_other",
                           start_date = starting_date,
                           end_date = ending_date)


print("GENERATING COHORTS FOR bladder anticholinergics")
info(logger, "GENERATING COHORTS FOR bladder anticholinergics")
#overactive bladder bladder anticholinergics G04BD
cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(
                             c("darifenacin", "ingredient"),
                             c("Desfesoterodine", "ingredient"),
                             c("emepronium", "ingredient"),
                             c("fesoterodine", "ingredient"),
                             c("flavoxate", "ingredient"),
                             c("mirabegron", "ingredient"),
                             c("oxybutynin", "ingredient"),
                             c("propiverine", "ingredient"),
                             c("solifenacin", "ingredient"),
                             c("terodiline", "ingredient"),
                             c("tolterodine", "ingredient"),
                             c("trospium", "ingredient") ),
                           table_name = "bladder_anticholinergics",
                           start_date = starting_date,
                           end_date = ending_date)


###############################
# currently being instantiated
################################


##########################
# not instantiated
#########################
print("GENERATING COHORTS FOR Indoles antipsychotics")
info(logger, "GENERATING COHORTS FOR Indoles antipsychotics")
# ANTIPSYCHOTICS
cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(
                             c("lurasidone", "ingredient"),
                             c("oxypertine", "ingredient"),
                             c("sertindole", "ingredient"),
                             c("molindone", "ingredient"),
                             c("ziprasidone", "ingredient")),
                           table_name = "antipsych_indoles",
                           start_date = starting_date,
                           end_date = ending_date)

print("GENERATING COHORTS FOR Butyrophenones antipsychotics")
info(logger, "GENERATING COHORTS FOR Butyrophenones antipsychotics")
# ANTIPSYCHOTICS
cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(
                             c("benperidol", "ingredient"),
                             c("bromperidol", "ingredient"),
                             c("droperidol", "ingredient"),
                             c("Fluanisone", "ingredient"),
                             c("haloperidol", "ingredient"),
                             c("Lumateperone", "ingredient"),
                             c("metylperon", "ingredient"),
                             c("Moperone", "ingredient"),
                             c("pipamperone", "ingredient"),
                             c("trifluperidol", "ingredient")),
                           table_name = "antipsych_butyrophenones",
                           start_date = starting_date,
                           end_date = ending_date)

print("GENERATING COHORTS FOR Fatty acid derivatives antiepileptics")
info(logger, "GENERATING COHORTS FOR Fatty acid derivatives antiepileptics")
# N03AG Fatty acid derivatives
cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(
                             c("aminobutyrate", "ingredient"),
                             c("dipropylacetamide", "ingredient"),
                             c("progabide", "ingredient"),
                             c("tiagabine", "ingredient"),
                             c("Valpromide", "ingredient"),
                             c("vigabatrin", "ingredient"),
                             c("valproate", "ingredient")),
                           table_name = "antiepileptics_fatty_acid",
                           start_date = starting_date,
                           end_date = ending_date)


print("GENERATING COHORTS FOR anti depressants Selective serotonin reuptake inhibitors")
info(logger, "GENERATING COHORTS FOR anti depressants Selective serotonin reuptake inhibitors")
# N06AB Selective serotonin reuptake inhibitors
cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(
                             c("Zimeldine", "ingredient"),
                             c("paroxetine", "ingredient"),
                             c("fluoxetine", "ingredient"),
                             c("citalopram", "ingredient"),
                             c("sertraline", "ingredient"),
                             c("escitalopram", "ingredient"),
                             c("fluvoxamine", "ingredient")),
                           table_name = "antidep_ssri",
                           start_date = starting_date,
                           end_date = ending_date)


print("GENERATING COHORTS FOR other antipsychotics")
info(logger, "GENERATING COHORTS FOR other antipsychotics")
#N05AX Other antipsychotics
cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(
                             c("pimavanserin", "ingredient"),
                             c("cariprazine", "ingredient"),
                             c("brexpiprazole", "ingredient"),
                             c("iloperidone", "ingredient"),
                             c("paliperidone", "ingredient"),
                             c("aripiprazole", "ingredient"),
                             c("mosapramine hydrochloride", "ingredient"),
                             c("prothipendyl", "ingredient"),
                             c("risperidone", "ingredient"),
                             c("zotepine", "ingredient")),
                           table_name = "other_antipsych",
                           start_date = starting_date,
                           end_date = ending_date)



print("GENERATING COHORTS FOR beta blockers")
info(logger, "GENERATING COHORTS FOR beta blockers")
#beta blockers C07AA, C07AB, C07AG
cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(c("acebutolol", "ingredient"),
                                       c("alprenolol", "ingredient"),
                                       c("atenolol", "ingredient"),
                                       c("sotalol", "ingredient"),
                                       c("betaxolol", "ingredient"),
                                       c("bevantolol", "ingredient"),
                                       c("bisoprolol", "ingredient"),
                                       c("bopindolol", "ingredient"),
                                       c("bupranolol", "ingredient"),
                                       c("carteolol", "ingredient"),
                                       c("carvedilol", "ingredient"),
                                       c("celiprolol", "ingredient"),
                                       c("esmolol", "ingredient"),
                                       c("labetalol", "ingredient"),
                                       c("landiolol", "ingredient"),
                                       c("mepindolol", "ingredient"),
                                       c("metoprolol", "ingredient"),
                                       c("nadolol", "ingredient"),
                                       c("nebivolol", "ingredient"),
                                       c("oxprenolol", "ingredient"),
                                       c("pindolol", "ingredient"),
                                       c("timolol", "ingredient"),
                                       c("tertatolol", "ingredient"),
                                       c("penbutolol", "ingredient"),
                                       c("talinolol", "ingredient"),
                                       c("practolol", "ingredient"),
                                       c("propranolol", "ingredient"),
                                         c("sotalol", "ingredient")),
                           table_name = "beta_blockers",
                           start_date = starting_date,
                           end_date = ending_date)


info(logger, "GENERATING COHORTS FOR anti psychotics")
info(logger, "GENERATING COHORTS FOR Phenothiazines antipsychotics")
print("GENERATING COHORTS FOR anti psychotics")
print("GENERATING COHORTS FOR Phenothiazines antipsychotics")
# ANTIPSYCHOTICS
cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(
                             c("acepromazine", "ingredient"),
                             c("acetophenazine", "ingredient"),
                             c("Butaperazine", "ingredient"),
                             c("chlorproethazine", "ingredient"),
                             c("chlorpromazine", "ingredient"),
                             c("cyamemazine", "ingredient"),
                             c("Dixyrazine", "ingredient"),
                             c("fluphenazine", "ingredient"),
                             c("mesoridazine", "ingredient"),
                             c("methotrimeprazine", "ingredient"),
                             c("perazine", "ingredient"),
                             c("periciazine", "ingredient"),
                             c("perphenazine", "ingredient"),
                             c("pipothiazine", "ingredient"),
                             c("prochlorperazine", "ingredient"),
                             c("promazine", "ingredient"),
                             c("thiopropazate", "ingredient"),
                             c("thioproperazine", "ingredient"),
                             c("thioridazine", "ingredient"),
                             c("trifluoperazine", "ingredient"),
                             c("triflupromazine", "ingredient")),
                           table_name = "antipsych_phenothiazines",
                           start_date = starting_date,
                           end_date = ending_date)


print("GENERATING COHORTS FOR other antiepileptics")
info(logger, "GENERATING COHORTS FOR other antiepileptics")
# N03AX Other antiepileptics
cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(
                             c("cenobamate", "ingredient"),
                             c("stiripentol", "ingredient"),
                             c("cannabidiol", "ingredient"),
                             c("brivaracetam", "ingredient"),
                             c("perampanel", "ingredient"),
                             c("ezogabine", "ingredient"),
                             c("lacosamide", "ingredient"),
                             c("pregabalin", "ingredient"),
                             c("felbamate", "ingredient"),
                             c("gabapentin", "ingredient"),
                             c("beclamide", "ingredient"),
                             c("phenacemide", "ingredient"),
                             c("topiramate", "ingredient"),
                             c("zonisamide", "ingredient"),
                             c("fenfluramine", "ingredient"),
                             c("lamotrigine", "ingredient"),
                             c("levetiracetam", "ingredient"),
                             c("sulthiame", "ingredient")),
                           table_name = "other_antiepileptics",
                           start_date = starting_date,
                           end_date = ending_date)

print("GENERATING COHORTS FOR antidepressants")
info(logger, "GENERATING COHORTS FOR antidepressants")
print("GENERATING COHORTS FOR anti depressants Non-selective monoamine reuptake inhibitors")
info(logger, "GENERATING COHORTS FOR anti depressants Non-selective monoamine reuptake inhibitors")
#ANTIDEPRESSANTS N06A ANTIDEPRESSANTS
# N06AA Non-selective monoamine reuptake inhibitors
cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(
                             c("Dimetacrine", "ingredient"),
                             c("protriptyline", "ingredient"),
                             c("nortriptyline", "ingredient"),
                             c("doxepin", "ingredient"),
                             c("trimipramine", "ingredient"),
                             c("dibenzepin", "ingredient"),
                             c("butriptyline", "ingredient"),
                             c("iprindole", "ingredient"),
                             c("lofepramine", "ingredient"),
                             c("amitriptyline", "ingredient"),
                             c("melitracen", "ingredient"),
                             c("maprotiline", "ingredient"),
                             c("imipramine", "ingredient"),
                             c("amoxapine", "ingredient"),
                             c("opipramol", "ingredient"),
                             c("amineptin", "ingredient"),
                             c("quinupramine", "ingredient"),
                             c("clomipramine", "ingredient"),
                             c("desipramine", "ingredient"),
                             c("dothiepin", "ingredient")),
                           table_name = "antidep_nsmri",
                           start_date = starting_date,
                           end_date = ending_date)


print("GENERATING COHORTS FOR other anti depressants")
info(logger, "GENERATING COHORTS FOR other anti depressants")
# N06AX Other antidepressants
cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(
                             c("esketamine", "ingredient"),
                             c("Agomelatine", "ingredient"),
                             c("vortioxetine", "ingredient"),
                             c("Hypericum perforatum leaf extract", "ingredient"),
                             c("Hypericum extract", "ingredient"),
                             c("vilazodone", "ingredient"),
                             c("milnacipran", "ingredient"),
                             c("desvenlafaxine", "ingredient"),
                             c("tryptophan", "ingredient"),
                             c("medifoxamine", "ingredient"),
                             c("minaprine", "ingredient"),
                             c("mianserin", "ingredient"),
                             c("viloxazine", "ingredient"),
                             c("tianeptine", "ingredient"),
                             c("mirtazapine", "ingredient"),
                             c("reboxetine", "ingredient"),
                             c("trazodone", "ingredient"),
                             c("bifemelane", "ingredient"),
                             c("5-hydroxytryptophan", "ingredient"),
                             c("St. John's wort extract", "ingredient"),
                             c("venlafaxine", "ingredient"),
                             c("nomifensine", "ingredient"),
                             c("nefazodone", "ingredient"),
                             c("duloxetine", "ingredient"),
                             c("bupropion", "ingredient")),
                           table_name = "other_antidep",
                           start_date = starting_date,
                           end_date = ending_date)



print("GENERATING COHORTS FOR calcium channel blockers")
info(logger, "GENERATING COHORTS FOR calcium channel blockers")
#CCB C08D, C08C
cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(c("amlodipine", "ingredient"),
                                       c("benidipine hydrochloride", "ingredient"),
                                       c("cilnidipine", "ingredient"),
                                       c("clevidipine", "ingredient"),
                                       c("diltiazem", "ingredient"),
                                       c("felodipine", "ingredient"),
                                       c("gallopamil", "ingredient"),
                                       c("isradipine", "ingredient"),
                                       c("lacidipine", "ingredient"),
                                       c("lercanidipine", "ingredient"),
                                       c("levamlodipine", "ingredient"),
                                       c("manidipine", "ingredient"),
                                       c("mepirodipine", "ingredient"),
                                       c("mibefradil", "ingredient"),
                                       c("nifedipine", "ingredient"),
                                       c("nilvadipine", "ingredient"),
                                       c("nimodipine", "ingredient"),
                                       c("nisoldipine", "ingredient"),
                                       c("nitrendipine", "ingredient"),
                                       c("verapamil", "ingredient")),
                            table_name = "ccb",
                           start_date = starting_date,
                           end_date = ending_date)


# Angiotensin-converting enzyme (ACE) inhibitors CO9A
print("GENERATING COHORTS FOR ACE inhibitors")
info(logger, "GENERATING COHORTS FOR ACE inhibitors")
cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(
                             c("benazepril", "ingredient"),
                             c("captopril", "ingredient"),
                             c("cilazapril", "ingredient"),
                             c("delapril hydrochloride", "ingredient"),
                             c("enalapril", "ingredient"),
                             c("enalaprilat", "ingredient"),
                             c("fosinopril", "ingredient"),
                             c("imidapril", "ingredient"),
                             c("lisinopril", "ingredient"),
                             c("moexipril", "ingredient"),
                             c("perindopril", "ingredient"),
                             c("quinapril", "ingredient"),
                             c("ramipril", "ingredient"),
                             c("spirapril", "ingredient"),
                             c("temocapril hydrochloride", "ingredient"),
                             c("trandolapril", "ingredient"),
                             c("zofenopril", "ingredient")),
                           table_name = "ace_inhibitors",
                           start_date = starting_date,
                           end_date = ending_date)

# diuretics
print("GENERATING COHORTS FOR diuretics")
info(logger, "GENERATING COHORTS FOR diuretics")
cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(
                             c("Mebutizide", "ingredient"),
                             c("methyclothiazide", "ingredient"),
                             c("hydroflumethiazide", "ingredient"),
                             c("cyclopenthiazide", "ingredient"),
                             c("polythiazide", "ingredient"),
                             c("chlorothiazide", "ingredient"),
                             c("hydrochlorothiazide", "ingredient"),
                             c("trichlormethiazide", "ingredient"),
                             c("bendroflumethiazide", "ingredient"),
                             c("cyclothiazide", "ingredient"),
                             c("chlorthalidone", "ingredient"),
                             c("cicletanine", "ingredient"),
                             c("Clofenamide", "ingredient"),
                             c("clopamide", "ingredient"),
                             c("Clorexolone", "ingredient"),
                             c("indapamide", "ingredient"),
                             c("mefruside", "ingredient"),
                             c("mersalyl", "ingredient"),
                             c("meticrane", "ingredient"),
                             c("metolazone", "ingredient"),
                             c("quinethazone", "ingredient"),
                             c("theobromine", "ingredient"),
                             c("xipamide", "ingredient"),
                             c("bumetanide", "ingredient"),
                             c("ethacrynate", "ingredient"),
                             c("Etozolin", "ingredient"),
                             c("furosemide", "ingredient"),
                             c("Muzolimine", "ingredient"),
                             c("piretanide", "ingredient"),
                             c("Tienilic Acid", "ingredient"),
                             c("torsemide", "ingredient"),
                             c("amiloride", "ingredient"),
                             c("canrenoate", "ingredient"),
                             c("canrenone", "ingredient"),
                             c("eplerenone", "ingredient"),
                             c("spironolactone", "ingredient"),
                             c("triamterene", "ingredient"),
                             c("conivaptan", "ingredient"),
                             c("tolvaptan", "ingredient")),
                           table_name = "diuretics",
                           start_date = starting_date,
                           end_date = ending_date)


print("GENERATING COHORTS FOR Benzodiazepines hypnotics and sedatives")
info(logger, "GENERATING COHORTS FOR Benzodiazepines hypnotics and sedatives")
# Benzodiazepine derivatives N05CD
cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(
                             c("brotizolam", "ingredient"),
                             c("estazolam", "ingredient"),
                             c("flunitrazepam", "ingredient"),
                             c("flurazepam", "ingredient"),
                             c("lormetazepam", "ingredient"),
                             c("midazolam", "ingredient"),
                             c("nimetazepam", "ingredient"),
                             c("nitrazepam", "ingredient"),
                             c("quazepam", "ingredient"),
                             c("remimazolam", "ingredient"),
                             c("temazepam", "ingredient"),
                             c("triazolam", "ingredient"),
                             c("triazulenone", "ingredient")
                           ),
                           table_name = "bzd_hs",
                           start_date = starting_date,
                           end_date = ending_date)

print("GENERATING COHORTS FOR Benzodiazepines anxiolytics")
info(logger, "GENERATING COHORTS FOR Benzodiazepines anxiolytics")
# Benzodiazepine derivatives anxiolytics N05BA
cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(
                             c("alprazolam", "ingredient"),
                             c("bromazepam", "ingredient"),
                             c("Camazepam", "ingredient"),
                             c("chlordiazepoxide", "ingredient"),
                             c("clobazam", "ingredient"),
                             c("clorazepate", "ingredient"),
                             c("clotiazepam", "ingredient"),
                             c("cloxazolam", "ingredient"),
                             c("diazepam", "ingredient"),
                             c("ethyl loflazepate", "ingredient"),
                             c("etizolam", "ingredient"),
                             c("fludiazepam", "ingredient"),
                             c("halazepam", "ingredient"),
                             c("ketazolam", "ingredient"),
                             c("lorazepam", "ingredient"),
                             c("medazepam", "ingredient"),
                             c("nordazepam", "ingredient"),
                             c("oxazepam", "ingredient"),
                             c("pinazepam", "ingredient"),
                             c("prazepam", "ingredient"),
                             c("tofisopam", "ingredient")
                           ),
                           table_name = "bzd_anxiolytics",
                           start_date = starting_date,
                           end_date = ending_date)


print("Finished test drugs")
info(logger, "GENERATED COHORTS FOR MARKER DRUGS")
