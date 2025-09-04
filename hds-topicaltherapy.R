## ============================================================================
## author:      Merel Postema
## date:        21-08-2025
## Rversion:    4.4.3
## project:     TREAT-NL
## description: create harmonized dataset (hds) file for topicaltherapy
## ============================================================================
rm(list=ls()[grep("export_date|ids2include|hds_visit", ls(), invert = TRUE)])

cat("--------------- hds.topicaltherapy ---------------\n")

# Load data ---------------------------------------------------------------
df <- read.csv(paste0("../data/", export_date, "/castor-export/TREAT_NL_register_2.0_Current_topical_AE_treatment_export_",
                      export_date, ".csv"), sep = ";")

# Create HDS --------------------------------------------------------------
hds_top <- df |> 
  filter(Participant.Id %in% ids2include) |> 
  select(c(Participant.Id, top_treat_type, Repeating.Data.Creation.Date)) |> 
  mutate(Repeating.Data.Creation.Date = as.Date(Repeating.Data.Creation.Date, format = "%d-%m-%Y"))

# HDS - treatment ---------------------------------------------------------
hds_top <- hds_top |> mutate(
  treatment = case_when(
    top_treat_type == 1 ~ 1,
    top_treat_type == 2 ~ 2,
    top_treat_type == 3 ~ 98,
    top_treat_type == 4 ~ 98,
    top_treat_type == 9 ~ 98,
    TRUE ~ 99
  )
)


# HDS - startdate and enddate ---------------------------------------------
# Constraint: startdate and enddate are not collected in TREAT-NL.

# We define visitdate as the end date, and the preceding visit date as the start date. --> NO
# NOTE: ER ZIJN HEEL VEEL VISITDATES PER PATIENT.
# ONDUIDELIJK WELKE VISIT DATE WE PRECIES WILLEN GEBRUIKEN VOOR TOPICAL TREATMENT.

# We define repeating data creation date as enddate, and the preceding repeatind data creation date as the start date.
# for the first startdate, we use the creation date minus 14 days.

hds_top <- hds_top |> 
  rename(enddate = Repeating.Data.Creation.Date) |> 
  group_by(Participant.Id) |> 
  arrange(enddate) |> 
  mutate(
    enddate = as.Date(enddate),
    startdate = lag(enddate),
    startdate = if_else(is.na(startdate), enddate - 14, startdate)  # 14 days before
  ) |> 
  ungroup() |> distinct()

# HDS - ongonig -----------------------------------------------------------
# at time of data extract, topical therapy has not ended.

# Constraint: there is always an enddate defined, 
# so by default all topical therapy is not ongoing.
hds_top <- hds_top |> 
  mutate(ongoing = case_when(
    is.na(enddate) ~ 1,
    TRUE ~ 0
  ))


# final adjustments -------------------------------------------------------
hds_top <- hds_top |>
  mutate(treatment = as.integer(treatment),
         startdate = as.character(startdate),
         enddate = as.character(enddate)) |>
  rename(anonymisedID = Participant.Id) |> 
  select(!top_treat_type) |> distinct() |> 
  # move enddate to after ongoing
  relocate(enddate, .after = ongoing)

# save HDS ----------------------------------------------------------------
write.csv(hds_top,
          paste0("../data/", export_date, "/hds/hds.topicaltherapy.csv"),
          row.names = FALSE
)
