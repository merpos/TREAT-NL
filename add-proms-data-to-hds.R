## ============================================================================
## author:      Merel Postema
## date:        29-07-2025
## Rversion:    4.4.3
## project:     TREAT-NL
## description: add additional PROMS data (from Excel) to the HDS
## ============================================================================
cat("--------------- add PROMS data ---------------\n")

# load data ---------------------------------------------------------------
df <- read.csv(paste0("../data/prosexcelall.csv"), sep = "," ) 

# DLQI --------------------------------------------------------------------
hds_dlqi <- read.csv(file = paste0("../data/", export_date, "/hds/hds.dlqi.csv"))

df2 <- df |> 
  filter(questionnaire == "CDLQI" | questionnaire == "DLQI" ) |> 
  mutate(dlqitype = case_when(questionnaire == "CDLQI" ~ 2, 
                              questionnaire == "DLQI" ~ 1, )) |> 
  mutate(value = if_else(value == "m", NA, value)) |> 
  rename(dlqi = value,
         dlqidate = Date.visit,
         anonymisedID = SubjectID) |> 
  select(c(anonymisedID, dlqidate, dlqi, dlqitype ))

hds_dlqi_new <- hds_dlqi |> 
  rbind(df2) |> 
  distinct() |> 
  filter(!is.na(dlqi) & !is.na(dlqitype)) |> 
  mutate(dlqi = as.integer(dlqi),
         dlqitype = as.integer(dlqitype))

rm(df2)
write.csv(hds_dlqi_new,
          paste0("../data/", export_date, "/hds/hds.dlqi.csv"),
          row.names = FALSE
)

# NRS ---------------------------------------------------------------------
hds_nrs <- read.csv(file = paste0("../data/", export_date, "/hds/hds.nrs.csv"))

df2 <- df |> 
  filter(questionnaire == "NRS.24h.pruritis") |> 
  mutate(value = if_else(value == "m", NA, value)) |> 
  rename(nrs = value,
         nrsdate = Date.visit,
         anonymisedID = SubjectID) |> 
  select(c(anonymisedID, nrsdate, nrs ))

hds_nrs_new <- hds_nrs |> 
  rbind(df2) |> 
  distinct() |> 
  filter(!is.na(nrs) | !is.na(nrsdate)) |> 
  mutate(nrs = as.integer(nrs))

rm(df2)
write.csv(hds_nrs_new,
          paste0("../data/", export_date, "/hds/hds.nrs.csv"),
          row.names = FALSE
)


# POEM --------------------------------------------------------------------
hds_poem <- read.csv(file = paste0("../data/", export_date, "/hds/hds.poem.csv"))

df2 <- df |> 
  filter(questionnaire == "POEM") |> 
  mutate(value = if_else(value == "m", NA, value),
         # note: also child version is filled out by patient itself, so
         # poemwho is always 1
         poemwho = 1 
         ) |> 
  rename(poem = value,
         poemdate = Date.visit,
         anonymisedID = SubjectID) |> 
  select(c(anonymisedID, poemdate, poem, poemwho ))

hds_poem_new <- hds_poem |>
  rbind(df2) |>
  distinct() |>
  filter(!is.na(poem) | !is.na(poemdate)) |>
  mutate(poem    = as.integer(poem), 
         poemwho = as.integer(poemwho))

rm(df2)
write.csv(hds_poem_new,
          paste0("../data/", export_date, "/hds/hds.poem.csv"),
          row.names = FALSE
)