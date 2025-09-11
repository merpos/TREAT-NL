## ============================================================================
## author:      Merel Postema
## date:        22-07-2025
## Rversion:    4.4.3
## project:     TREAT-NL
## description: create harmonized dataset (hds) file for visits
## ============================================================================
cat("--------------- hds.visits ---------------\n")

# load data ---------------------------------------------------------------
df_visit <-
  read.csv(paste0("../data/", export_date, "/castor-export/TREAT_NL_register_2.0_export_", export_date, ".csv"),
           sep = ";"
  )  

files <- c( paste0("TREAT_NL_register_2.0_Follow-up_visit_export_", export_date, ".csv"),
            paste0("TREAT_NL_register_2.0_Visit_Switch_Re_start_at_4_weeks_export_", export_date, ".csv"),
            paste0("TREAT_NL_register_2.0_Switch_DO_NOT_CREATE_ANY_NEW_FORM_export_", export_date, ".csv"),
            paste0("TREAT_NL_register_2.0_UnscheduledVisit_export_", export_date, ".csv")
            
)
df_merged <- files |> 
  lapply(function(f) read.csv(file.path(paste0("../data/", export_date, "/castor-export/", f)), sep = ";")) |> 
  bind_rows()

df_merged <- bind_rows(df_visit, df_merged)

ids2include <- df_visit |> filter(Site.Abbreviation != "TES") |> pull(Participant.Id)

# create HDS --------------------------------------------------------------
hds_visit <- df_merged |>
  filter(Participant.Id %in% ids2include) |> 
  mutate(
    enrolment_date    = na_if(enrolment_date, ""),
    w4_visit_date     = na_if(w4_visit_date, ""),
    fu_visit_date     = na_if(fu_visit_date, ""),
    swi_visit_date    = na_if(swi_visit_date, ""),
    swi_w4_visit_date = na_if(swi_w4_visit_date, ""),
    usv_visit_date    = na_if(usv_visit_date, ""),
  ) |>
  mutate(
    # note enrolment_date and w4_visit date are within the same file, 
    # so not coalesce for w4
    visitdate = coalesce(
      fu_visit_date,
      swi_visit_date,
      swi_w4_visit_date,
      usv_visit_date,
      enrolment_date
    )
  ) |>
  mutate(visitdate = as.character(as.Date(visitdate, format = "%d-%m-%Y")),
  ) 

# add additional rows for w4_visit_date (was not included in coalesce)
hds_visit <- hds_visit |> mutate(
  w4_visit_date = as.character(as.Date(w4_visit_date, format = "%d-%m-%Y"))
)
hds_visit <-
  pivot_longer(
    hds_visit,
    c("visitdate", "w4_visit_date"),
    names_to = "visitdate_colname",
    values_to = "visitdate"
  ) |> filter(!is.na(visitdate)) |>
  distinct()

w <- which(hds_visit$visitdate > as.Date("2900-01-01"))
if(length(w)!=0){
  cat("The following dates are defined as missing:")
  hds_visit[which(hds_visit$visitdate > as.Date("2900-01-01")),c("Participant.Id","visitdate")] |> print(n = length(w))
  hds_visit[which(hds_visit$visitdate > as.Date("2900-01-01")),]$visitdate <- NA
}

# final adjustments --------------------------------------------------------
hds_visit_final <- hds_visit |> select(c(Participant.Id, visitdate)) |> 
  rename(anonymisedID = Participant.Id) |> distinct()

# save HDS ----------------------------------------------------------------
write.csv(hds_visit_final,
  paste0("../data/", export_date, "/hds/without-proms-data/hds.visits.csv"),
  row.names = FALSE
)