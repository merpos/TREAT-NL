## ============================================================================
## author:      Merel Postema
## date:        22-07-2025
## Rversion:    4.4.3
## project:     TREAT-NL
## description: create harmonized dataset (hds) file for demographics
## ============================================================================
rm(list=ls()[grep("export_date|export_date_full|ids2include|hds_visit", ls(), invert = TRUE)])

cat("--------------- hds.demographics ---------------\n")

# Load data ---------------------------------------------------------------
df_visit <-
  read.csv(paste0("../data/", export_date, "/castor-export/TREAT_NL_register_2.0_export_", export_date, ".csv"),
    sep = ";"
  )
df_educ <-
  read.csv(paste0("../data/", export_date, "/castor-export/TREAT_NL_register_2.0_Educational_status_export_", export_date, ".csv"),
    sep = ";"
  )
df_occ <-
  read.csv(paste0("../data/", export_date, "/castor-export/TREAT_NL_register_2.0_Occupation_export_", export_date, ".csv"),
    sep = ";"
  )

# Create HDS --------------------------------------------------------------
hds_dem <- df_visit |>
  filter(Participant.Id %in% ids2include) |> 
  select(c(
    Participant.Id,
    dob,
    ae_onset_year,
    gender,
    matches("ethnicity")
  )) |>
  select(!ethnicity_other) |>
  rename(
    anonymisedID = Participant.Id,
    dateofbirth = dob,
    dateofadonset = ae_onset_year,
    sex = gender
  )

hds_dem <- hds_dem |>
  mutate(
    dateofbirth = as.character(as.Date(dateofbirth, format = "%d-%m-%Y")),
    dateofadonset = as.character(as.Date(dateofadonset, format = "%d-%m-%Y")),
    sex = case_when(
      sex == 9 ~ 98,
      is.na(sex) ~ 99,
      TRUE ~ sex
    )
  )


hds_dem <- hds_dem |>
  rowwise() |>
  mutate(ethnicity = paste0(c(
    if (ethnicity.White..Europe..Russia..Middle.East..North.Africa..USA..Canada..Australia. == 1) 1 else NULL,
    if (ethnicity.Black.African..Afro.Caribbean == 1) 2 else NULL,
    if (ethnicity.African.American == 1) 3 else NULL,
    if (ethnicity.Asian.Chinese == 1) 4 else NULL,
    if (ethnicity.South.Asian..India..Pakistan..Sri.Lanka..Nepal..Bhutan..Bangladesh. == 1) 5 else NULL,
    if (ethnicity.Asian...other..Korea..China.north.of.Huai.River. == 1) 6 else NULL,
    if (ethnicity.Japanese == 1) 7 else NULL,
    if (ethnicity.Hispanic.or.Latino == 1) 8 else NULL,
    if (ethnicity.Mixed...please.specify == 1) 9 else NULL,
    if (ethnicity.Other...please.specify == 1) 98 else NULL
  ), collapse = ",")) |>
  ungroup() |>
  # Constraint:
  # When multiple ethnicities are checked, the participant will be specified as mixed.
  mutate(ethnicity_multi = case_when(grepl(",", ethnicity) ~ 1, TRUE ~ 0)) |>
  mutate(ethnicity = if_else(ethnicity_multi == 1, "9", ethnicity)) |>
  select(!matches("ethnicity\\.")) |>
  mutate(ethnicity = as.integer(ethnicity)) |>
  mutate(ethnicity = if_else(is.na(ethnicity), 99, ethnicity))


# Add education and occupation to HDS -------------------------------------
for (rep_data in c("education", "occupation")) {
  
  message(paste0("--------------- ", rep_data, " ---------------"))
  
  if (rep_data == "education") {
    df_tmp <- df_educ
    cols <- c("educ_visit_dt", "educ_status")
  } else {
    df_tmp <- df_occ
    cols <- c("occ_visit_dt", "occupation_type")
  }

  df_tmp <- df_tmp |>
    select(Participant.Id, all_of(cols)) |>
    mutate(
      !!cols[1] := as.character(as.Date(.data[[cols[1]]], format = "%d-%m-%Y"))
    ) |>  
    filter(Participant.Id %in% ids2include) 

  df_tmp <- df_tmp |> 
    full_join(df_visit, join_by(Participant.Id == Participant.Id )) |> 
    filter(Site.Abbreviation != "TES") |> 
    distinct() |> 
    rename(anonymisedID = Participant.Id)
  
  # Constraint:
  # since there can be multiple measures per participant,
  # use the measure where the date is closest to enrolment_date,
  # that is, either earlier or later than enrolment date.
  df_tmp <- df_tmp |>
    group_by(anonymisedID) |>
    mutate(
      enrolment_date = as.Date(enrolment_date, format = "%d-%m-%Y"),
      diff = as.numeric(abs(difftime(.data[[cols[1]]], enrolment_date, units = "weeks")))
    ) |>
    slice_min(diff) |>
    ungroup() 

  # Constraint:
  # Some duplicated measures might remain after selecting those closest to date of enrollment,
  # because they have the same time difference.
  # For edcuation, choose the highest level. 
  # For occupation, randomly select one, unless employes and self-employed were checked,
  # in that case employed is used.
  
  if ((nrow(df_tmp) > nrow(hds_dem)) & (rep_data == "education")) {
    
    cat(paste0(
      "Note: Out of the below duplicates for education (i.e., with the same time difference), ",
      "the highest education will be selected:\n"
    ))
    
    df_tmp |>
      filter(anonymisedID %in% anonymisedID[duplicated(anonymisedID)]) |>
      distinct() |>
      select(c(anonymisedID, educ_visit_dt, educ_status, enrolment_date, diff)) |>
      distinct() |>
      print(nrow = Inf)
    
    df_tmp <- df_tmp |>
      select(c(anonymisedID, cols[2])) |> 
      group_by(anonymisedID) |>
      slice_max(order_by = !!sym(cols[2]), with_ties = FALSE)
    
    df_educ2 <- df_tmp
    
  } else if ((nrow(df_tmp) == nrow(hds_dem)) & (rep_data == "education")) {
    
    df_educ2 <- df_tmp
    
  } else if ((nrow(df_tmp) > nrow(hds_dem)) & (rep_data == "occupation")) {
    
    cat(paste0(
      "Note 1: Out of the below duplicates for occupation (i.e., with the same time difference), ",
      "one will be randomly selected.\n",
      "Note 2: Unless employed + self-employed, in which case employed will be used.\n"
    ))
    
    df_tmp_sub <- df_tmp |>
      filter(anonymisedID %in% anonymisedID[duplicated(anonymisedID)]) |>
      distinct() |>
      group_by(anonymisedID) |>
      mutate(
        check_12 = if_else(all(c(1, 2) %in% occupation_type), 1, 0),
        occupation_type = case_when(check_12 == 1 ~ 1, TRUE ~ occupation_type)
      ) |>
      filter(check_12 != 1) |>
      select(c(
        anonymisedID,
        occ_visit_dt,
        occupation_type,
        enrolment_date,
        diff
      ))
      
    print(df_tmp_sub, n = nrow(df_tmp_sub))
    rm(df_tmp_sub)
    
    df_tmp <- df_tmp |>
      group_by(anonymisedID) |>
      mutate(
        check_12 = if_else(all(c(1, 2) %in% occupation_type), 1, 0),
        occupation_type = case_when(
          check_12 == 1 ~ 1,
          TRUE ~ occupation_type
        )
      ) |>
      slice_sample(n = 1) |>
      ungroup() |>
      select(anonymisedID, occupation_type)
    
    df_occ2 <- df_tmp
    
  } else if ((nrow(df_tmp) == nrow(hds_dem)) & (rep_data == "occupation")) {
    
    df_occ2 <- df_tmp
    
  } else {
    
    warning("Unknown `rep_data` value: ", rep_data, ".")
  }
} 
  
if ((nrow(df_educ2) != nrow(hds_dem)) | (nrow(df_occ2) != nrow(hds_dem)) ) {
  warning(" Number of participants in hds.demographics is not the same for education or occupation.
            Please check lines 11-124 of this script.")
}

df_educ2 <- df_educ2 |>
  mutate(educ_status = educ_status + 1) |>
  mutate(educ_status = if_else(is.na(educ_status), 99, educ_status))

hds_dem <- left_join(hds_dem, df_educ2, join_by(anonymisedID == anonymisedID))

df_occ2 <- df_occ2 |>
  mutate(occupation_type = case_when(
    occupation_type == 9 ~ 98,
    is.na(occupation_type) ~ 99,
    TRUE ~ occupation_type
  ))

hds_dem <- left_join(hds_dem, df_occ2, join_by(anonymisedID == anonymisedID))

hds_dem <- hds_dem |> rename(
  education = educ_status,
  occupation = occupation_type
)

# final adjustments -------------------------------------------------------
hds_dem <- hds_dem |> 
  mutate(sex = as.integer(sex),
         ethnicity = as.integer(ethnicity),
         education = as.integer(education),
         occupation = as.integer(occupation)) |> 
  select(!ethnicity_multi) |> 
  distinct()

# save HDS ----------------------------------------------------------------
write.csv(hds_dem,
  paste0("../data/", export_date, "/hds/hds.demographics.csv"),
  row.names = FALSE
)