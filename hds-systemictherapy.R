## ============================================================================
## author:      Merel Postema
## date:        29-07-2025
## Rversion:    4.4.3
## project:     TREAT-NL
## description: create harmonized dataset (hds) file for systemictherapy
## ============================================================================
rm(list=ls()[grep("export_date|export_date_full|ids2include|hds_visit", ls(), invert = TRUE)])

cat("--------------- hds.systemictherapy ---------------\n")

# Load data ---------------------------------------------------------------
df <- read.csv(paste0("../data/", export_date, "/castor-export/TREAT_NL_register_2.0_Current_systemic_AE_treatment_export_", export_date, ".csv"),
           sep = ";"
  )

df_merged <- full_join(hds_visit, df, join_by(Participant.Id), relationship = "many-to-many") |>
  filter(!is.na(enrolment_date)) |> 
  # since there are more ids in the hds_visit than in the treatment df, 
  # only keep those ids that are in treatment df
  # this is to prevent missings for treatment startdate 
  # (gives error in ADS conversion code 3.1)
  filter(Participant.Id %in% df$Participant.Id)


# HDS - startdate and enddate ---------------------------------------------
hds_syst <- df_merged |>
  filter(Participant.Id %in% ids2include) |>
  mutate(
    syst_ther_start_date  = na_if(syst_ther_start_date , ""),
    syst_ther_start_date  = as.Date(syst_ther_start_date, format = "%d-%m-%Y"),
    syst_ther_stop_date   = na_if(syst_ther_stop_date, ""),
    syst_ther_stop_date   = as.Date(syst_ther_stop_date, format = "%d-%m-%Y"),
    #enrolment_date        = as.Date(enrolment_date, format = "%d-%m-%Y"),
  )  |>
  rename(
    startdate = syst_ther_start_date,
    enddate = syst_ther_stop_date
  ) 

hds_syst_diff <- hds_syst |>
   mutate(
    diff  = as.numeric(difftime(startdate, enrolment_date, units = "weeks")),
    diff2 = as.numeric(difftime(enddate, startdate, units = "weeks")),
    diff3 = as.numeric(difftime(export_date_full, enddate, units = "weeks"))
  )
hds_check <- hds_syst_diff |>
  filter(diff < 0 | 
           diff2 < 0 | 
           diff3 < 0)

if(nrow(hds_check) != 0){
  cat(paste0(
    "NOTE: startdate should be greater than or equal to the earliest visitdate, ",
    "and enddate should be greater than or equal to the startdate, ",
    "as well as less than or equal to the date of data extract. ",
    "The following data are removed accordingly:\n"
  ))
  hds_check |>
    as_tibble() |>
    select(Participant.Id, startdate, enrolment_date, enddate) |>
    distinct() |> 
    print(n = nrow(hds_check))
  
  hds_syst <- hds_syst_diff |>
    # Constraint startdate:
    # startdate should be larger or equal to the enrolment_date
    filter(diff >= 0 | is.na(diff)) |> 
    # Constraint enddate:
    # must satisfy BOTH:
    # - enddate >= startdate (or missing if ongoing)
    # - enddate <= export_date_full (or missing)
    filter((diff2 >= 0 | is.na(diff2)) & 
             (diff3 >= 0 | is.na(diff3)))
  
  }
rm(hds_check, hds_syst_diff)


# HDS - treatment ---------------------------------------------------------
hds_syst <- hds_syst |> rename(treatment = syst_ther_type) |>
  mutate(
    treatment = case_when(
      treatment == 1 ~ 1,
      treatment == 2 ~ 3,
      treatment == 3 ~ 2,
      treatment == 4 ~ 4,
      treatment == 5 ~ 5,
      treatment == 6 ~ 6,
      treatment == 7 ~ 11,
      treatment == 8 ~ 9,
      treatment == 9 ~ 14,
      treatment == 10 ~ 15,
      treatment == 11 ~ 13,
      treatment == 12 ~ 8,
      treatment == 99 ~ 98,
      treatment == 999 ~ 98,
      is.na(treatment) ~ 99,
      TRUE ~ treatment
    )
  )

# HDS - mainad ------------------------------------------------------------
# Constraint: difficutl to implement for all sites.
# set all to main (see e-mail Bola)
hds_syst <- hds_syst |> mutate(mainad = 1)

# derive overview of most common combinations of therapies.
# str(hds_syst)
# test <- hds_syst |> select(c(anonymisedID, treatment, startdate, enddate, visitdate))
# subset(test, anonymisedID == "AUMCC18")
# # check for overlapping time windows within participants
# df_flagged <- test %>%
#   group_by(anonymisedID) %>%
#   mutate(
#     overlap = case_when(
#       sapply(seq_along(startdate), function(i) {
#         any(
#           !is.na(startdate[i]) & !is.na(enddate[i]) & 
#           !is.na(startdate[-i]) & !is.na(enddate[-i]) & # maybe: if enddate =NA, fill in with current date (to be able to compute range)
#           startdate[i] <= enddate[-i] &
#             enddate[i]   >= startdate[-i]
#         )
#       }) ~ "combi",
#       TRUE ~ "mono"
#     )
#   ) %>%
#   ungroup()
# subset(df_flagged, anonymisedID == "AUMCC18")


# HDS - concomitant and stopreason ----------------------------------------

for(v in c("concomitant", "stopreason")) {
  if (v == "concomitant") {
    matchpat <-
      "_immu_sup_yn|_immu_sup_which|_immu_sup_start_date|_immu_sup_stop_date"
  } else {
    matchpat <- "therapy_discontinuation"
  }
  
  if (v == "concomitant") {
      hds_syst <- hds_syst |>
      rowwise() |>
      mutate(concomitant = any(c_across(matches("_immu_sup_yn")) == 1, na.rm = TRUE)) |>
      ungroup() |>
      mutate(concomitant = case_when(concomitant == TRUE ~ 1,
                                     concomitant == FALSE ~ 0,
                                     TRUE ~ NA))
    
  } else{
    # Constraint: NL register allows multiple answers for therapy discontinuation.
    # In case of multiple answers, the first match (order specified below) is used!
      hds_syst <- hds_syst |>
      mutate(
        stopreason = case_when(
          fu_therapy_discontinuation_reason.Adverse.event.s. == 1 ~ 1,
          swi_w4_therapy_discontinuation_reason.Adverse.event.s. == 1 ~ 1,
          swi_therapy_discontinuation_reason.Adverse.event.s. == 1 ~ 1,
          w4_therapy_discontinuation_reason.Adverse.event.s. == 1 ~ 1,

          fu_therapy_discontinuation_reason.Efficacy == 1 ~ 2,
          swi_w4_therapy_discontinuation_reason.Efficacy == 1 ~ 2,
          swi_therapy_discontinuation_reason.Efficacy == 1 ~ 2,
          w4_therapy_discontinuation_reason.Efficacy == 1 ~ 2,

          fu_therapy_discontinuation_reason.Inefficacy == 1 ~ 3,
          swi_w4_therapy_discontinuation_reason.Inefficacy == 1 ~ 3,
          swi_therapy_discontinuation_reason.Inefficacy == 1 ~ 3,
          w4_therapy_discontinuation_reason.Inefficacy == 1 ~ 3,

          fu_therapy_discontinuation_reason.Interaction.with.other.medication == 1 ~ 6,
          swi_w4_therapy_discontinuation_reason.Interaction.with.other.medication == 1 ~ 6,
          swi_therapy_discontinuation_reason.Interaction.with.other.medication == 1 ~ 6,
          w4_therapy_discontinuation_reason.Interaction.with.other.medication == 1 ~ 6,

          fu_therapy_discontinuation_reason.Child.wish == 1 ~ 6,
          swi_w4_therapy_discontinuation_reason.Child.wish == 1 ~ 6,
          swi_therapy_discontinuation_reason.Child.wish == 1 ~ 6,
          w4_therapy_discontinuation_reason.Child.wish == 1 ~ 6,

          fu_therapy_discontinuation_reason.Patient.request == 1 ~ 6,
          swi_w4_therapy_discontinuation_reason.Patient.request == 1 ~ 6,
          swi_therapy_discontinuation_reason.Patient.request == 1 ~ 6,
          w4_therapy_discontinuation_reason.Patient.request == 1 ~ 6,

          fu_therapy_discontinuation_reason.Other == 1 ~ 6,
          swi_w4_therapy_discontinuation_reason.Other == 1 ~ 6,
          swi_therapy_discontinuation_reason.Other == 1 ~ 6,
          w4_therapy_discontinuation_reason.Other == 1 ~ 6,

          fu_therapy_discontinuation_reason.Not.applicable == 1 ~ NA,
          swi_w4_therapy_discontinuation_reason.Not.applicable == 1 ~ NA,
          swi_therapy_discontinuation_reason.Not.applicable == 1 ~ NA,
          w4_therapy_discontinuation_reason.Not.applicable == 1 ~ NA,

          TRUE ~ 7
        )
      )
  }
}
  
# HDS - dosage and dosage_other -------------------------------------------
hds_syst <- hds_syst |>
  mutate(
    dosage_other = case_when(
      nchar(syst_ther_start_dose) > 20 ~ syst_ther_start_dose,
      TRUE ~ NA_character_
    ),
    dosage = case_when(
      nchar(syst_ther_start_dose) > 20 ~ NA_character_,
      TRUE ~ syst_ther_start_dose
    ),
    dosage = sub("mg.*|MG.*", "", dosage),
    dosage = sub(".*?(d|\\:)", "", dosage),
    dosage = gsub("\\s", "", dosage),
    dosage = sub(",", ".", dosage),
    dosage = sub("ay", "", dosage),
    dosage = case_when(
      dosage == "" ~ NA,
      TRUE ~ dosage
    )
  )

## move data with remaining characters to dosage_other
hds_syst <- hds_syst |>
  mutate(
    dosage_other = case_when(
      grepl("[a-zA-Z]", dosage) ~ syst_ther_start_dose,
      TRUE ~ dosage_other
    ),
    dosage = case_when(grepl("[a-zA-Z]", dosage) ~ NA,
                       TRUE ~ dosage)
  )

# Internal agreement: 
# For monoclonal antibodies with a specific current dose, 
# "syst_ther_currdose_*" is primary; for all others, start dose is primary. 

# --- Start dose leading: ---
# Ciclosporine – daily
# Azathioprine – daily
# Methotrexate – weekly
# Mycophenolate acid – daily
# Systemic corticosteroids – daily
# Baricitinib – daily
# Abrocitinib – daily

# --- Current dose leading: ---
# Dupilumab – as recorded in "syst_ther_currdose_*"
# Tralokinumab – as recorded in "syst_ther_currdose_*"
# Lebrikizumab – as recorded in "syst_ther_currdose_*"

hds_syst <- hds_syst |> 
  mutate(
    dosage = case_when(
      syst_ther_currdose_dupl %in% 1:2 ~ "200",
      syst_ther_currdose_dupl %in% 3:6 ~ "300",
      syst_ther_currdose_tralok %in% 1:4 ~ "300",
      syst_ther_currdose_lebrik %in% 1:3 ~ "250",
      is.na(syst_ther_currdose_dupl) & is.na(syst_ther_currdose_tralok) & is.na(syst_ther_currdose_lebrik) ~ dosage,
      TRUE ~ NA_character_
    ),
    dosage_other = case_when(
      syst_ther_currdose_dupl == 9 ~ syst_ther_currdose_dupl_other,
      syst_ther_currdose_tralok == 9 ~ syst_ther_currdose_tralok_other,
      # note: no other option for lebrik 
      is.na(syst_ther_currdose_dupl) & is.na(syst_ther_currdose_tralok) & is.na(syst_ther_currdose_lebrik) ~ dosage_other,
      TRUE ~ NA_character_
    )
  )

# HDS - frequency and frequency_other -------------------------------------

# see comment lines 300 - 313 for frequency.

hds_syst <- hds_syst |>
  mutate(frequency = case_when(
    # frequency in weeks
    treatment == 1 ~ 1*7, # cicl
    treatment == 2 ~ 1*7, # aza
    treatment == 3 ~ 1,   # meth
    treatment == 4 ~ 1*7, # myco
    treatment == 5 ~ 1*7, # other conv syst
    treatment == 6 ~ NA,  # dupi, current dosage leading
    treatment == 7 ~ NA,  # nemo, not collected
    treatment == 8 ~ NA,  # lebri, current dosage leading
    treatment == 9 ~ NA,  # tralok, current dosage leading
    treatment == 10 ~ NA, # roca, not collected
    treatment == 11 ~ NA, # oma, not collected
    treatment == 12 ~ NA, # other biol, not collected
    treatment == 13 ~ 1*7, # abro 
    treatment == 14 ~ 1*7, # bari
    treatment == 15 ~ 1*7, # upa,
    treatment == 16 ~ NA,  # other jak, not collected
    treatment == 98 ~ NA, # other syst 
    treatment == 99 ~ NA, # unknown
  ))

hds_syst <- hds_syst |> 
  mutate(
    frequency = case_when(
      syst_ther_currdose_dupl == 1 ~ 1/2,        # every other week
      syst_ther_currdose_dupl == 2 ~ 1/(10/7),   # every 10 days
      syst_ther_currdose_dupl == 3 ~ 1/2,        # every other week
      syst_ther_currdose_dupl == 4 ~ 1/3,        # every 3 weeks
      syst_ther_currdose_dupl == 5 ~ 1/4,        # every 4 weeks
      
      syst_ther_currdose_tralok == 1 ~ 1/(10/7), # every 10 days
      syst_ther_currdose_tralok == 2 ~ 1/2,      # every other week
      syst_ther_currdose_tralok == 3 ~ 1/3,      # every 3 weeks
      syst_ther_currdose_tralok == 4 ~ 1/4,      # every 4 weeks
      
      syst_ther_currdose_lebrik == 1 ~ 1/2,      # every other week
      syst_ther_currdose_lebrik == 2 ~ 1/3,      # every 3 weeks
      syst_ther_currdose_lebrik == 3 ~ 1/4,      # every 4 weeks

      is.na(syst_ther_currdose_dupl) & is.na(syst_ther_currdose_tralok) & is.na(syst_ther_currdose_lebrik) ~ frequency,
      TRUE ~ NA_real_
    ),
    frequency_other = case_when(
      syst_ther_currdose_dupl == 9 ~ syst_ther_currdose_dupl_other,
      syst_ther_currdose_tralok == 9 ~ syst_ther_currdose_tralok_other,
      # note: no other option for lebrik
      TRUE ~ NA_character_
    )
  )

# TO DO MAYBE LATER: 
# WRITE CODE TO ALSO INCLUDE FREQUENCY OTHER / DOSAGE OTHER IN THE MAIN COLUMNS.

cat(paste0("Note: the number of dosages specified as other is: ",nrow(hds_syst[!is.na(hds_syst$dosage_other),]),"\n"))
cat(paste0("Note: the number of frequencies specified as other is: ",nrow(hds_syst[!is.na(hds_syst$frequency_other),]),"\n"))
# note: 
# numbers are not equal, because treatment 5 (=other) sometimes has frequency of 7 with dosage other.
# test <- hds_syst |> filter(!is.na(dosage_other)) |> select(c(treatment, dosage, dosage_other, frequency, frequency_other))

# HDS - ongoing -----------------------------------------------------------
# at time of data extract, systemic AD therapy has not ended.

hds_syst <- hds_syst |> 
  mutate(ongoing = case_when(
    is.na(enddate) ~ 1,
    TRUE ~ 0
  ))

# final adjustments -------------------------------------------------------
df_checked <- hds_syst %>%
  mutate(
    dosage_numeric = as.numeric(dosage),  
    coercion_problem = is.na(dosage_numeric) & !is.na(dosage)
  )

w <- which(df_checked$coercion_problem == TRUE)
if(length(w)!= 0){
  df_checked2  <- subset(df_checked, coercion_problem == TRUE) |> 
    select(c(Participant.Id, syst_ther_start_dose, dosage, dosage_other)) |> 
    distinct()
  cat(paste0("Note: dosage coercion problem for id(s): ", unique(df_checked2$Participant.Id)),". Below dosage data are converted to NA.")
  print(df_checked2)
  hds_syst[w,]$dosage <- NA
}

hds_syst <- hds_syst |>
  select(
    c(
      Participant.Id,
      treatment,
      mainad,
      concomitant,
      dosage,
      dosage_other,
      frequency,
      frequency_other,
      startdate,
      ongoing,
      enddate,
      stopreason
    )
  ) |> 
  mutate(
    treatment = as.integer(treatment),
    mainad = as.integer(mainad),
    concomitant = as.integer(concomitant),
    dosage = as.numeric(dosage),
    startdate = as.character(startdate),
    ongoing = as.integer(ongoing),
    enddate = as.character(enddate),
    stopreason = as.integer(stopreason)
  ) |> 
  rename(
    anonymisedID = Participant.Id
  ) |> 
  distinct()


# save HDS ----------------------------------------------------------------
write.csv(hds_syst,
          paste0("../data/", export_date, "/hds/hds.systemictherapy.csv"),
          row.names = FALSE
)