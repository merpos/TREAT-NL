## ============================================================================
## author:      Merel Postema
## date:        29-07-2025
## Rversion:    4.4.3
## project:     TREAT-NL
## description: create harmonized dataset (hds) file for poem
## ============================================================================
rm(list=ls()[grep("export_date|export_date_full|hds_visit|ids2include", ls(), invert = TRUE)])

cat("--------------- hds.poem ---------------\n")

# Load data ---------------------------------------------------------------

myfiles <- list.files(paste0("../data/", export_date,"/castor-export/"))
myfiles <- myfiles[grep("Kind|Child|Volwassen|Adult",myfiles)]

df_poem <- NULL 
for (f in myfiles) {
  df_tmp <- read.csv(paste0("../data/", export_date, "/castor-export/", f), sep = ";") |> 
    select(Castor.Participant.ID, matches("^calc_POEM_.._TOT|^currentdate_.._"))
  df_poem <- bind_rows(df_poem, df_tmp)  
}

# HDS - poem and poemdate -------------------------------------------------
currentdate_cols <- names(df_poem)[grepl("^currentdate_", names(df_poem))]
poem_cols <- names(df_poem)[grepl("^calc_POEM_.._TOT", names(df_poem))]

df_poem <- df_poem |>
  distinct() |>
  rowwise() |>
  mutate(poemdate = do.call(coalesce, as.list(c_across(all_of(currentdate_cols)))),
         poem=do.call(coalesce, as.list(c_across(all_of(poem_cols))))
  ) |>
  ungroup() |> 
  select(c(Castor.Participant.ID, poemdate, poem))

# HDS - poemwho -----------------------------------------------------------
# Note: Also the child version is filled out by the patient self.
df_poem$poemwho <- 1

# final adjustments -------------------------------------------------------
hds_poem <- df_poem |> full_join(hds_visit[,c("Participant.Id","visitdate")], 
                      join_by(Castor.Participant.ID == Participant.Id), 
                      relationship = "many-to-many") |> 
  filter(
  Castor.Participant.ID %in% ids2include
) |> 
  distinct()

# Constraint: poemdate should be larger or equal to 14 days before the earliest visitdate
hds_poem <- hds_poem |> 
  mutate(
    visitdate = as.Date(visitdate, format = "%Y-%m-%d"),
    poemdate  = as.Date(poemdate,  format = "%d-%m-%Y")
  ) |> 
  group_by(Castor.Participant.ID) |> 
  mutate(
    earliest_visit = if (all(is.na(visitdate))) NA else min(visitdate, na.rm = TRUE),
    constraint_ok  = if_else(poemdate >= earliest_visit - 14, 1, 0),  # 1 = valid, 0 = invalid
    diff_days      = as.numeric(difftime(poemdate, earliest_visit, units = "days"))
  ) |> 
  ungroup()

if(nrow(subset(hds_poem, constraint_ok == 0))!=0){
  cat("NOTE: poemdate should be larger or equal to 14 days before the earliest visitdate, so the following poem data are removed:\n")
  hds_poem |> 
    filter(constraint_ok == 0) |> select(!c(visitdate, constraint_ok)) |> distinct() |> print(n = Inf)
  hds_poem <- hds_poem |> filter(constraint_ok == 1)
}

hds_poem <- hds_poem |>
  rename(anonymisedID = Castor.Participant.ID) |> 
  select(c(anonymisedID, poemdate, poem, poemwho))|> 
  distinct() |> 
  mutate(poemdate = as.character(poemdate), 
         poemwho = as.integer(poemwho)) |> 
  distinct()

# save HDS ----------------------------------------------------------------
write.csv(hds_poem,
          paste0("../data/", export_date, "/hds/without-proms-data/hds.poem.csv"),
          row.names = FALSE
)
