## ============================================================================
## author:      Merel Postema
## date:        29-07-2025
## Rversion:    4.4.3
## project:     TREAT-NL
## description: create harmonized dataset (hds) file for comorbidities
## ============================================================================
rm(list=ls()[grep("export_date|export_date_full|hds_visit|ids2include", ls(), invert = TRUE)])

cat("--------------- hds.comorbidities ---------------\n")

# Load data ---------------------------------------------------------------
df_visit <-
  read.csv(paste0("../data/", export_date, "/castor-export/TREAT_NL_register_2.0_export_", export_date, ".csv"),
           sep = ";"
  )

files <- c(paste0("TREAT_NL_register_2.0_CoMorbIllnesses_export_", export_date, ".csv"),
           paste0("TREAT_NL_register_2.0_CoMorbInfections_export_", export_date, ".csv"),
           paste0("TREAT_NL_register_2.0_CoMorbMalignancies_export_", export_date, ".csv")
)
df_merged <- files |> 
  lapply(function(f) read.csv(file.path(paste0("../data/", export_date, "/castor-export/", f)), sep = ";")) |> 
  bind_rows()

df_merged <- full_join(df_visit, df_merged, by = intersect(names(df_visit),names(df_merged)))

# HDS - diagnosis ---------------------------------------------------------
hds_comorb <- df_merged  |>
  filter(Participant.Id %in% ids2include) |> 
  select(
    c(
      Participant.Id,
      bas_comorb_asthma,
      bas_comorb_rhino,
      bas_comorb_eye,
      bas_comorb_oesophag,
      bas_food_allergies_yn,
      bas_illnesses_yn,
      bas_infections_yn,
      bas_malignancies_yn,
      bas_contact_allergies_yn,
      bas_skin_infections_yn ,
      illness_outcome,
      infect_outcome,
      malign_outcome
    )
  )
# Constraint: no info about anxiety and depression

hds_comorb <- hds_comorb |>
  rename(anonymisedID = Participant.Id) |>
  mutate(
    diagnosis = case_when(
      bas_comorb_asthma == 1 ~ 1,
      bas_comorb_rhino == 1 ~ 2,
      bas_food_allergies_yn == 1 ~ 3,
      (
        bas_comorb_eye == 1 |
          bas_comorb_oesophag == 1 |
          bas_illnesses_yn == 1 |
          bas_infections_yn == 1 |
          bas_malignancies_yn == 1 |
          bas_contact_allergies_yn == 1 |
          bas_skin_infections_yn == 1
      ) ~ 98,
      TRUE ~ 99
    )
  )

# HDS - ongoing -----------------------------------------------------------
# Constraint: 
# information about the status of the comorbidity (ongoing yes/no)
# is not available. Illness, malign and infect outcome are only filled in 
# when adverse event, so not possible to derive ongoing status.

hds_comorb <- hds_comorb |>
  mutate( ongoing = NA)

# final adjustments -------------------------------------------------------
hds_comorb <- hds_comorb |> 
  select(c(anonymisedID, diagnosis, ongoing)) |> 
  mutate( diagnosis = as.integer(diagnosis),
          ongoing = as.integer(ongoing))

# save HDS ----------------------------------------------------------------
write.csv(hds_comorb,
          paste0("../data/", export_date, "/hds/hds.comorbidities.csv"),
          row.names = FALSE
)
