## ============================================================================
## author:      Merel Postema
## date:        29-08-2025
## Rversion:    4.4.3
## project:     TREAT-NL
## description: check HDS output
## ============================================================================



# visit -------------------------------------------------------------------
df_visit <- read.csv(file = paste0("../data/", export_date, "/hds/hds.visits.csv"))
summary(as.Date(df_visit$visitdate))
which(df_visit$visitdate == "")

# demographics ------------------------------------------------------------
df_dem <- read.csv(file = paste0("../data/", export_date, "/hds/hds.demographics.csv"))
str(df_dem)
summary(as.Date(df_dem$dateofbirth))
df_dem |> mutate(any_empty = if_any(everything(), ~.x == "")) |> pull(any_empty) |> table()
table(df_dem$sex)
table(df_dem$ethnicity) 
table(df_dem$education)
table(df_dem$occupation)

# systemic therapy --------------------------------------------------------
df_syst <- read.csv(file = paste0("../data/", export_date, "/hds/hds.systemictherapy.csv"))
str(df_syst)
df_syst |> mutate(any_empty = if_any(everything(), ~.x == "")) |> pull(any_empty) |> table()
table(df_syst$treatment)
table(df_syst$mainad)
table(df_syst$concomitant)
summary(df_syst$dosage) # is 2000 mg a realistic value?
table(df_syst$frequency)
summary(as.Date(df_syst$startdate))
table(is.na(df_syst$ongoing))
which(df_syst$ongoing == "")
summary(as.Date(df_syst$enddate))
table(df_syst$stopreason)

# dlqi --------------------------------------------------------------------
df_dlqi <- read.csv(file = paste0("../data/", export_date, "/hds/hds.dlqi.csv"))
str(df_dlqi)
df_dlqi |> mutate(any_empty = if_any(everything(), ~.x == "")) |> pull(any_empty) |> table()
summary(df_dlqi$dlqi) # 0 tot 3 --> should be 0 to 30 !!
table(df_dlqi$dlqitype)

# comorbidities -----------------------------------------------------------
df_comorb <- read.csv(file = paste0("../data/", export_date, "/hds/hds.comorbidities.csv"))
str(df_comorb)
df_comorb |> mutate(any_empty = if_any(everything(), ~.x == "")) |> pull(any_empty) |> table()
table(df_comorb$diagnosis)
table(df_comorb$ongoing)

# poem --------------------------------------------------------------------
df_poem <- read.csv(file = paste0("../data/", export_date, "/hds/hds.poem.csv"))
str(df_poem)
df_poem |> mutate(any_empty = if_any(everything(), ~.x == "")) |> pull(any_empty) |> table()
summary(df_poem$poem) 
table(df_poem$poemwho)

# nrs ---------------------------------------------------------------------
df_nrs <- read.csv(file = paste0("../data/", export_date, "/hds/hds.nrs.csv"))
str(df_nrs)
df_nrs |> mutate(any_empty = if_any(everything(), ~.x == "")) |> pull(any_empty) |> table()
summary(as.Date(df_nrs$nrsdate)) # er komt een 3023 in voor, deze moet eruit gefilterd.
df_nrs[which(df_nrs$nrsdate > "2030-01-01"),] # AUMCU18 heeft een NRS datum van 3023!


# phototherapy ------------------------------------------------------------
df_pt <- read.csv(file = paste0("../data/", export_date, "/hds/hds.phototherapy.csv"))
str(df_pt)
df_pt |> mutate(any_empty = if_any(everything(), ~.x == "")) |> pull(any_empty) |> table()
table(df_pt$treatment)
summary(as.Date(df_pt$startdate))
table(df_pt$ongoing)
summary(as.Date(df_pt$enddate)) 


# systemictherapyhx -------------------------------------------------------
df_sth <- read.csv(file = paste0("../data/", export_date, "/hds/hds.systemictherapyhx.csv"))
str(df_sth)
df_sth |> mutate(any_empty = if_any(everything(), ~.x == "")) |> pull(any_empty) |> table()
table(df_sth$treatment)

# iga ---------------------------------------------------------------------
df_iga <- read.csv(file = paste0("../data/", export_date, "/hds/hds.iga.csv"))
str(df_iga)
df_iga |> mutate(any_empty = if_any(everything(), ~.x == "")) |> pull(any_empty) |> table()
summary(df_iga$iga) # 0- 4 is OK
summary(as.Date(df_iga$igadate))


# easi --------------------------------------------------------------------
df_easi <- read.csv(file = paste0("../data/", export_date, "/hds/hds.easi.csv"))
str(df_easi)
df_easi |> mutate(any_empty = if_any(everything(), ~.x == "")) |> pull(any_empty) |> table()
summary(df_easi$easiscore) # 0- 72; OK
summary(as.Date(df_easi$easidate))

# topical therapy ---------------------------------------------------------
df_top <- read.csv(file = paste0("../data/", export_date, "/hds/hds.topicaltherapy.csv"))
str(df_top)
df_top |> mutate(any_empty = if_any(everything(), ~.x == "")) |> pull(any_empty) |> table()
table(df_top$treatment)
summary(as.Date(df_top$startdate))
summary(as.Date(df_top$enddate))
