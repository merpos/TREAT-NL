## ============================================================================
## author:      Merel Postema
## date:        21-08-2025
## Rversion:    4.4.3
## project:     TREAT-NL
## description: create harmonized dataset (hds) file for topicaltherapy
## ============================================================================
rm(list=ls()[grep("export_date|export_date_full|ids2include|hds_visit", ls(), invert = TRUE)])

cat("--------------- hds.topicaltherapy ---------------\n")

# Load data ---------------------------------------------------------------
df <- read.csv(paste0("../data/", export_date, "/castor-export/TREAT_NL_register_2.0_Current_topical_AE_treatment_export_",
                      export_date, ".csv"), sep = ";")

# Create HDS --------------------------------------------------------------
hds_top <- df |> 
  filter(Participant.Id %in% ids2include) |> 
  select(c(Participant.Id, top_treat_type, Repeating.Data.Creation.Date, Repeating.data.Parent)) |> 
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
# agreement: we use the date in the repeating.data.parent column, 
# or else the repeating.data.creation.date, to link to the closest visitdate.
# this visitdate should be **earlier** than the repeating data date, and
# will be assigned as startdate. 
# the enddate will be the earliest subsequent date where topical therapy ends,
# as indicated with *top_ther_yn.

# ----- extract repeating data date -----
hds_top <- hds_top |>
  mutate(
    repdat = regmatches(Repeating.data.Parent,
                        gregexpr("\\b\\d{2}-\\d{2}-\\d{4}\\b", Repeating.data.Parent)),
    repdat = lapply(repdat,"[",1:3),
    repdat = vapply(repdat, function(x) paste(x, collapse = "-"), character(1)),
    repdat = if_else(grepl("NA", repdat), NA_character_, repdat),
    repdat = as.Date(repdat, format = "%d-%m-%Y"),
    repdat = if_else(is.na(repdat), Repeating.Data.Creation.Date, repdat)
  )

if(nrow(hds_top[which(hds_top$repdat > export_date_full),]) >0){
  cat(paste0(
    "Note: Repeating.data.Parent date is greater than export date.\n", 
    "For the below data, Repeating.Data.Creation.Date is used instead."
  ))
  hds_top[which(hds_top$repdat > export_date_full),] |> print(nrow = Inf)
  hds_top[which(hds_top$repdat > export_date_full),]$repdat <- hds_top[which(hds_top$repdat > export_date_full),]$Repeating.Data.Creation.Date
}

hds_top <- hds_top |> 
  filter(Participant.Id %in% ids2include) |> 
  select(c(Participant.Id, treatment, repdat)) |> 
  arrange(Participant.Id, repdat)

# ----- link to the closest visitdate -----
hds_visit2 <- hds_visit |> 
  select(c(Participant.Id, visitdate, contains("top_ther_yn"))) |> 
  arrange(Participant.Id)

hds_top <- hds_top |> 
  mutate(startdate = NA,
         enddate = NA)
hds_top_empty <- hds_top[0,]

for (p in unique(hds_top$Participant.Id)) {
  hds_top_sub <- subset(hds_top, Participant.Id == p) |> distinct()
  hds_visit_sub <- subset(hds_visit2, Participant.Id == p) |> distinct()
  
  if(nrow(hds_visit_sub) == 0){
    next
  }else{
    for (i in nrow(hds_top_sub)) {
      
      # ----- startdate ----- 
      dates_diff <- difftime(hds_visit_sub$visitdate, hds_top_sub[i, ]$repdat)
      # visitdate should be earlier than repeating data date
      w1 <- which(dates_diff < 0) 
      w2 <- which.min(abs(dates_diff)[w1])
      w3 <- w1[w2]
      rm(w1, w2)
      if(length(w3)!=0){
        hds_top_sub[i, ]$startdate <- hds_visit_sub$visitdate[w3]
        
        # ----- stopdate ----- 
        hds_visit_sub2 <- hds_visit_sub |> filter(if_any(contains("top_ther_yn"), ~ . == 0))
        if(nrow(hds_visit_sub2) == 0){
          # no enddate specified
          next 
        }else{
          rm(w3)
          # select the first subsequent (i.e., positive value) date as enddate
          tmp <- difftime(hds_visit_sub2$visitdate, hds_top_sub[i, ]$startdate)
          w1 <- which(tmp > 0)
          w2 <- which.min(tmp[w1])
          w3 <- w1[w2]
          if(length(w3)!=0){
          hds_top_sub[i, ]$enddate <- hds_visit_sub2$visitdate[w3]
          }else{
          next
        }
        }
      }else{
        cat(paste0(
          "Note: for the below data, repeatingdata date is later than visitdate.\n", 
          "Hence no startdate was assigned."
        ))
        hds_top_sub[i, ] |>  print(nrow = Inf)
        next
      }
    }
    hds_top_empty <- rbind(hds_top_empty, hds_top_sub)
    rm(hds_top_sub, hds_visit_sub)
  }
}

hds_top <- hds_top_empty |> 
  filter(!is.na(startdate)) |>
  select(c(Participant.Id, treatment, startdate, enddate))

# HDS - ongonig -----------------------------------------------------------
# at time of data extract, topical therapy has not ended.
hds_top <- hds_top |> 
  mutate(ongoing = case_when(
    is.na(enddate) ~ 1,
    TRUE ~ 0
  ))

# final adjustments -------------------------------------------------------
hds_top <- hds_top |>
  mutate(treatment = as.integer(treatment),
         startdate = as.character(startdate),
         enddate = as.character(enddate),
         ongoing = as.integer(ongoing)) |>
  rename(anonymisedID = Participant.Id) |> 
  distinct() |> 
  # move enddate to after ongoing
  relocate(enddate, .after = ongoing)
  

# save HDS ----------------------------------------------------------------
write.csv(hds_top,
          paste0("../data/", export_date, "/hds/hds.topicaltherapy.csv"),
          row.names = FALSE
)
