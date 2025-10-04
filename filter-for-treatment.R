## ============================================================================
## author:      Merel Postema
## date:        12-09-2025
## Rversion:    4.4.3
## project:     TREAT-NL
## description: only keep data for the systemic therapies included in the analysis
## ============================================================================
rm(list=ls()[grep("export_date|export_date_full|ids2include|hds_visit", ls(), invert = TRUE)])

cat("--------------- filter for systemic therapy ---------------\n")

# only keep abro, cilo and mtx 
# the other treatments are not needed for analyses

# 1	ciclosporin
# 2	methotrexate
# 3	azathioprine
# 4	mycophenolate mofetil
# 5	other conventional systemic
# 6	dupilumab
# 7	nemolizumab
# 8	lebrikizumab
# 9	tralokinumab
# 10	rocatinlimab
# 11	omalizumab
# 12	other biologic
# 13	abrocitinib
# 14	baricitinib
# 15	upadacitinib
# 16	other jak inhibitors
# 98	other systemic not falling into outlined categories
# 99	unknown

# systemic therapy --------------------------------------------------------
hds_syst <- read.csv(file = paste0("../data/", export_date, "/hds/hds.systemictherapy.csv"))

ids2extract <- hds_syst |> 
  filter(treatment == 1 | treatment == 2 | treatment == 13) |> 
  pull(anonymisedID)

hds_syst_new <- hds_syst |> filter(anonymisedID %in% ids2extract)

write.csv(hds_syst_new,
          paste0("../data/", export_date, "/hds/hds.systemictherapy.csv"),
          row.names = FALSE
)

rm(ids2extract)

# systemic therapy history ------------------------------------------------

hds_sth <- read.csv(file = paste0("../data/", export_date, "/hds/hds.systemictherapyhx.csv"))

ids2extract <- hds_sth |> 
  filter(treatment == 1 | treatment == 2 | treatment == 13) |> 
  pull(anonymisedID)

hds_sth_new <- hds_sth |> filter(anonymisedID %in% ids2extract)

write.csv(hds_sth_new,
          paste0("../data/", export_date, "/hds/hds.systemictherapyhx.csv"),
          row.names = FALSE
)
