## ============================================================================
## author:      Merel Postema
## date:        29-07-2025
## Rversion:    4.4.3
## project:     TREAT-NL
## description: create harmonized dataset (hds) file for dlqi
## ============================================================================
rm(list=ls()[grep("export_date|hds_visit|ids2include", ls(), invert = TRUE)])

cat("--------------- hds.dlqi ---------------\n")

# Load data ---------------------------------------------------------------
files_dlqi <- list.files(paste0("../data/20250722/castor-export/"))
files_dlqi <- files_dlqi[grep('_Volwassen_|_Adult_|_Kind_|_Child_', files_dlqi)]

df_dlqi <- files_dlqi |> 
  lapply(function(f) read.csv(file.path(paste0("../data/", export_date, "/castor-export/", f)), sep = ";")) |> 
  bind_rows() |> 
  select(Castor.Participant.ID, matches('^currentdate_|DLQI_'))

# HDS - dlqidate ----------------------------------------------------------
currentdate_cols <- names(df_dlqi)[grepl("^currentdate_", names(df_dlqi))]

df_dlqi <- df_dlqi |> 
  rowwise() |> 
  mutate(dlqidate = do.call(coalesce, as.list(c_across(all_of(currentdate_cols))))) |> 
  ungroup() |> 
  select(Castor.Participant.ID, matches('^dlqidate$|DLQI_'))

rm(currentdate_cols)

# HDS - dlqi and dlqitype -------------------------------------------------
dlqi_cols <- names(df_dlqi)[grepl("^CDLQI_|^DLQI", names(df_dlqi))]
dlqi_cols_c <- dlqi_cols[grep('^DLQI', dlqi_cols)] # type = dlqi
dlqi_cols_d <- dlqi_cols[grep('^CDLQI', dlqi_cols)] # type = clqi

df_dlqi <- df_dlqi |>
  mutate(across(all_of(dlqi_cols), ~as.integer(.x))) |> 
  rowwise() |> 
  mutate(dlqi_c = do.call(coalesce, as.list(c_across(all_of(dlqi_cols_c)))),
         dlqi_d = do.call(coalesce,as.list(c_across(all_of(dlqi_cols_d)))),
  ) |> 
  ungroup()

df_dlqi <- pivot_longer(df_dlqi, c("dlqi_c","dlqi_d"), names_to = "dlqitype", values_to = "dlqi")

df_dlqi <- df_dlqi |> 
  mutate(dlqitype = case_when(dlqitype == "dlqi_d" ~ 1,
                              dlqitype == "dlqi_c" ~ 2,
                              TRUE ~ NA))

# final adjustments -------------------------------------------------------
hds_dlqi <- df_dlqi |>   
  filter(Castor.Participant.ID %in% ids2include)

hds_dlqi <- hds_dlqi |> 
  select(c(Castor.Participant.ID, dlqidate, dlqi, dlqitype)) |> 
  mutate(dlqidate = as.character(as.Date(dlqidate, format = "%d-%m-%Y")),
         dlqitype = as.integer(dlqitype))

# save HDS ----------------------------------------------------------------
write.csv(hds_dlqi,
          paste0("../data/", export_date, "/hds/hds.dlqi.csv"),
          row.names = FALSE
)