## ============================================================================
## author:      Merel Postema
## date:        21-08-2025
## Rversion:    4.4.3
## project:     TREAT-NL
## description: create harmonized dataset (hds) file for easi
## ============================================================================
rm(list=ls()[grep("export_date|export_date_full|ids2include|hds_visit", ls(), invert = TRUE)])

cat("--------------- hds.easi ---------------\n")

# HDS - easiscore ---------------------------------------------------------
hds_easi <- hds_visit |> mutate(
  # note: do not include w4 here (in same row as baseline)
  easiscore = coalesce(
    fu_easi_total_score,
    swi_easi_total_score,
    swi_w4_easi_total_score,
    usv_easi_total_score,
    bas_easi_total_score
  )
) |> 
  rename(
    easidate = visitdate
  )

# igascore for w4 was filled in with the baseline igascore, so replace
hds_easi[which(hds_easi$visitdate_colname == "w4_visit_date"),]$easiscore <- hds_easi[which(hds_easi$visitdate_colname == "w4_visit_date"),]$w4_easi_total_score

hds_easi <-hds_easi |> 
  select(c(Participant.Id, easidate, easiscore)) |> 
  distinct() |> 
  mutate(easiscore = case_when( easiscore < 0 ~NA,
                          TRUE ~ easiscore)) |> 
  rename(anonymisedID = Participant.Id)

# save HDS ----------------------------------------------------------------
write.csv(hds_easi,
          paste0("../data/", export_date, "/hds/hds.easi.csv"),
          row.names = FALSE
)
