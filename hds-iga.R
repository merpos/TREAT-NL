## ============================================================================
## author:      Merel Postema
## date:        21-08-2025
## Rversion:    4.4.3
## project:     TREAT-NL
## description: create harmonized dataset (hds) file for iga
## ============================================================================
rm(list=ls()[grep("export_date|ids2include|hds_visit", ls(), invert = TRUE)])

cat("--------------- hds.iga ---------------\n")

# HDS - iga ---------------------------------------------------------------
hds_iga <- hds_visit |> mutate(
  # note: do not include w4 here (in same row as baseline)
  iga = coalesce(
    fu_iga_score,
    swi_iga_score,
    swi_w4_iga_score,
    usv_iga_score,
    bas_iga_score
  )
) |> 
  rename(
    igadate = visitdate
  )

# igascore for w4 was filled in with the baseline igascore, so replace
hds_iga[which(hds_iga$visitdate_colname == "w4_visit_date"),]$iga <- hds_iga[which(hds_iga$visitdate_colname == "w4_visit_date"),]$w4_iga_score
  
hds_iga <-hds_iga |> 
  select(c(Participant.Id, igadate, iga)) |> 
  distinct() |> 
  mutate(iga = case_when( iga < 0 ~NA,
                          TRUE ~ iga)) |> 
  rename(anonymisedID = Participant.Id)

# save HDS ----------------------------------------------------------------
write.csv(hds_iga,
          paste0("../data/", export_date, "/hds/hds.iga.csv"),
          row.names = FALSE
)
