## ============================================================================
## author:      Merel Postema
## date:        07-08-2025
## Rversion:    4.4.3
## project:     TREAT-NL
## description: create harmonized dataset (hds) file for nrs
## ============================================================================
rm(list=ls()[grep("export_date|export_date_full|hds_visit|ids2include", ls(), invert = TRUE)])

cat("--------------- hds.nrs ---------------\n")

# Load data ---------------------------------------------------------------
df_visit <-
  read.csv(paste0("../data/", export_date, "/castor-export/TREAT_NL_register_2.0_export_", export_date, ".csv"),
           sep = ";"
  )

myfiles <- list.files(paste0("../data/", export_date,"/castor-export/"))
myfiles <- myfiles[grep("Kind|Child|Volwassen|Adult",myfiles)]
df_nrs <- NULL 
for (f in myfiles) {
  df_tmp <- read.csv(paste0("../data/", export_date, "/castor-export/", f), sep = ";") |> 
    select(Castor.Participant.ID, matches("^NRS_|^currentdate_.._"))
  df_nrs <- bind_rows(df_nrs, df_tmp)  
}

# HDS - nrs and nrsdate -------------------------------------------------
currentdate_cols <- names(df_nrs)[grepl("^currentdate_", names(df_nrs))]
nrs_cols <- names(df_nrs)[grepl("^NRS_.._1_", names(df_nrs))] # TREAT uses 24 hour version.

df_nrs <- df_nrs |>
  distinct() |>
  rowwise() |>
  mutate(nrsdate = do.call(coalesce, as.list(c_across(all_of(currentdate_cols)))),
         nrs=do.call(coalesce, as.list(c_across(all_of(nrs_cols))))
  ) |>
  ungroup() |> 
  select(c(Castor.Participant.ID, nrsdate, nrs))

# final adjustments -------------------------------------------------------
# only keep ids from hds_visit (i.e., filtered out for TEST sites)
hds_nrs <- df_nrs |> full_join(hds_visit[,c("Participant.Id","visitdate")], 
                     join_by(Castor.Participant.ID == Participant.Id), 
                     relationship = "many-to-many") |> 
  filter(
    Castor.Participant.ID %in% ids2include
  ) |> 
  distinct()
  

# Constraint: nrsdate should be larger or equal to 14 days before visitdate
hds_nrs <- hds_nrs |> 
  mutate(
    visitdate = as.Date(visitdate, format = "%Y-%m-%d"),
    nrsdate  = as.Date(nrsdate,  format = "%d-%m-%Y")
  ) |> 
  group_by(Castor.Participant.ID) |> 
  mutate(
    earliest_visit = if (all(is.na(visitdate))) NA else min(visitdate, na.rm = TRUE),
    constraint_ok  = if_else(nrsdate >= earliest_visit - 14, 1, 0),  # 1 = valid, 0 = invalid
    diff_days      = as.numeric(difftime(nrsdate, earliest_visit, units = "days"))
  ) |> 
  ungroup()

if(nrow(subset(hds_nrs, constraint_ok == 0))!=0){
  cat("NOTE: nrsdate should be larger or equal to 14 days before the earliest visitdate, so the following nrs data are removed:\n")
  hds_nrs |> 
    filter(constraint_ok == 0) |> select(!c(visitdate, constraint_ok)) |> distinct() |> print(n = Inf)
  hds_nrs <- hds_nrs |> filter(constraint_ok == 1)
}

hds_nrs <- hds_nrs |>
  select(c(Castor.Participant.ID, nrsdate, nrs))|> 
  distinct() |> 
  mutate(nrsdate = as.character(nrsdate)) |> 
  distinct() |> 
  rename(anonymisedID = Castor.Participant.ID)

# save HDS ----------------------------------------------------------------
write.csv(hds_nrs,
          paste0("../data/", export_date, "/hds/without-proms-data/hds.nrs.csv"),
          row.names = FALSE
)
