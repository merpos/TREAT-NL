## ============================================================================
## author:      Merel Postema
## date:        07-08-2025
## Rversion:    4.4.3
## project:     TREAT-NL
## description: create harmonized dataset (hds) file for phototherapy
## ============================================================================
rm(list=ls()[grep("export_date|hds_visit|ids2include", ls(), invert = TRUE)])

export_date_full <- paste(c(substring(export_date, 1,4),
                            substring(export_date, 5,6),
                            substring(export_date, 7,8)), collapse="-", sep="")

cat("--------------- hds.phototherapy ---------------\n")

# Load data ---------------------------------------------------------------
df_visit <-
  read.csv(paste0("../data/", export_date, "/castor-export/TREAT_NL_register_2.0_export_", export_date, ".csv"),
           sep = ";"
  )

files <- c(paste0("TREAT_NL_register_2.0_Current_phototherapy_AE_treatment_export_", export_date, ".csv"),
           paste0("TREAT_NL_register_2.0_Past_phototherapy_AE_treatment_export_", export_date, ".csv")
)
df_pt <- NULL 
for (f in files) {
  df_tmp <- read.csv(paste0("../data/", export_date, "/castor-export/", f), sep = ";") 
  df_pt <- bind_rows(df_pt, df_tmp)  
}

# HDS - treatment ---------------------------------------------------------
hds_pt <- df_pt |>
  mutate(treatment = case_when((curr_photother_type == 1 | past_photother_type == 1) ~ 3,
                               (curr_photother_type == 2 | past_photother_type == 2) ~ 4,
                               (curr_photother_type == 3 | past_photother_type == 3) ~ 6,
                               (curr_photother_type == 4 | past_photother_type == 4) ~ 1,
                               (curr_photother_type == 5 | past_photother_type == 5) ~ 2,
                               (curr_photother_type == 6 | past_photother_type == 6) ~ 7,
                               (curr_photother_type == 7 | past_photother_type == 7) ~ 5,
                               (curr_photother_type == 8 | past_photother_type == 8) ~ 98,
                               TRUE ~ 99))


# HDS - startdate ---------------------------------------------------------
hds_pt <- hds_pt |>
  mutate(
    curr_photother_start_date = as.character(as.Date(curr_photother_start_date, format = "%d-%m-%Y")),
    # Constraint:
    # For past phototherapy, only the year has been documented.
    # transform to real dates, by adding 01-01 for day and month.
    past_photother_year2date = case_when(
      (!is.na(past_photother_year) &
         past_photother_year > 0) ~ paste0(past_photother_year, '-01-01'),
      TRUE ~ NA_character_
    ),
    startdate = coalesce(curr_photother_start_date, past_photother_year2date)
  )

# HDS - enddate -----------------------------------------------------------
hds_pt <- hds_pt |> rename(enddate = curr_photother_stop_date) |> 
  mutate(enddate = as.character(as.Date(enddate, format = "%d-%m-%Y")))

# HDS - ongoing -----------------------------------------------------------
# At time of data extract phototherapy has not ended.
hds_pt <- hds_pt |>
  mutate(
    ongoing = case_when(
      is.na(enddate) ~ 1,
      as.Date(enddate, format = "%Y-%m-%d") > as.Date(export_date_full, format = "%Y-%m-%d") ~ 1,
      TRUE ~ 0
  )
)

# Final adjustments -------------------------------------------------------
hds_pt <- hds_pt |> 
  filter(Participant.Id %in% ids2include) |> 
  select(c(Participant.Id, treatment, startdate, ongoing, enddate)) |> 
  mutate(treatment = as.integer(treatment),
         ongoing = as.integer(ongoing)
         ) |> 
  rename(anonymisedID = Participant.Id) |> 
  mutate(
    startdate = if_else(as.Date(startdate) > as.Date("2900-01-01"),
                        NA_character_,
                        startdate),
    enddate   = if_else(as.Date(enddate) > as.Date("2900-01-01"),
                        NA_character_,
                        enddate)
  )

# save HDS ----------------------------------------------------------------
write.csv(hds_pt,
          paste0("../data/", export_date, "/hds/hds.phototherapy.csv"),
          row.names = FALSE
)