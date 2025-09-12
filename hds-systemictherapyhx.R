## ============================================================================
## author:      Merel Postema
## date:        12-08-2025
## Rversion:    4.4.3
## project:     TREAT-NL
## description: create harmonized dataset (hds) file for systemictherapyhx
## ============================================================================
rm(list=ls()[grep("export_date|export_date_full|hds_visit|ids2include", ls(), invert = TRUE)])

cat("--------------- hds.systhemictherapyhx ---------------\n")

# Load data ---------------------------------------------------------------
# df_visit <-
#   read.csv(paste0("../data/", export_date, "/castor-export/TREAT_NL_register_2.0_export_", export_date, ".csv"),
#            sep = ";"
#   )


# Create HDS --------------------------------------------------------------
hds_systhist <- hds_visit |> select(Participant.Id, visitdate, contains("past_syst_ther_")) |> 
  select(Participant.Id,, visitdate, matches("_type|_year_|_month_|_stopdt_|_stop_")) |> 
  select(!contains("stop_spec"))

hds_systhist <- hds_systhist |>
  mutate(
    across(
      .cols = starts_with("past_syst_ther_year"),
      .fns = ~ {
        tmp <- as.numeric(.x)
        tmp <- ifelse(tmp < 0, NA, tmp)
        as.character(tmp)
      }
    ),
    across(
      .cols = starts_with("past_syst_ther_month_start"),
      .fns = ~ {
        tmp <- as.numeric(.x)
        tmp <- ifelse(tmp < 0, NA, tmp)
        as.character(tmp)
      }
    ),
    across(
      .cols = starts_with("past_syst_ther_stopdt"),
      .fns = ~ map_chr(.x, function(val) {
        if (is.na(val) || val == "") return(NA_character_)
        parts <- unlist(strsplit(val, "-"))
        # replace unknown day ("UK") with 15
        if ("UK" %in% parts[1]) parts[1] <- "15"
        # replace unknown month ("UK") with 06
        if ("UK" %in% parts[2]) parts[2] <- "06"
        paste(parts, collapse = "-")
      })
    )
  )

df_empty <- data.frame(
  Participant.Id = character(),
  treatment    = character(),
  startdate    = character(),
  enddate      = character(),
  stopreason   = integer()
)

# Note: past_syst_ther_type is a checkbox, so multiple treatments can be selected.
# the code below loops through each treatment and binds rows in order to derive all info.
for(trt in c(
  "ciclo",
  "aza",
  "meth",
  "myco",
  "cort",
  "dupi",
  "omal",
  "tralo",
  "bari",
  "upada",
  "abro",
  "lebrik",
  "other",
  # inv = inv_med 
  "inv" 
)) {
  print(paste0("--------------", trt, "----------------------"))
 for( p in unique(hds_systhist$Participant.Id)){
  hds_tmp <- hds_systhist |> 
    select(Participant.Id, contains(trt, ignore.case = TRUE)) |> 
    filter(Participant.Id == p) |>
    filter(!if_all(-1, is.na)) |> 
    distinct()
  
  w <- grep("past_syst_ther_type", (names(hds_tmp)))
  if(isTRUE(hds_tmp[,w]==1)){
  
    # ------------- treatment -------------
    hds_tmp$treatment <- trt # later nog coderen 
    
    # ------------- startdate -------------
    myyear <- hds_tmp |> select(contains("year")) |>  pull()
    mymonth <- hds_tmp |> select(contains("month")) |>  pull()
    if(isTRUE(!is.na(mymonth) && nchar(mymonth) == 1)){ mymonth <- paste0("0", mymonth)}
    
    if (isTRUE(!is.na(myyear) && !is.na(mymonth))) {
      # Constraint: day is always unknown, fill with 15.
      hds_tmp <- hds_tmp |> mutate(startdate = paste(myyear, mymonth, "15", sep = "-"))
    } else if (isTRUE(!is.na(myyear) && is.na(mymonth))) {
      # Constraint: if month is unknown, fill with June.
      hds_tmp <- hds_tmp |> mutate(startdate = paste(myyear, "06-15", sep = "-"))
    } else{
      cat(
        paste0(
          "Unknown past treatment year for participant: ", p, ", treatment: ",
          trt,  ". Startdate is made missing.\n"
        )
      )
    }

    # ------------- enddate -------------
    w2 <- grep("past_syst_ther_stopdt", (names(hds_tmp)))
    hds_tmp <- hds_tmp |> rename(enddate = colnames(hds_tmp)[w2])
    rm(w2)
    
    # ------------- stopreason -------------
    w <- grep("past_syst_ther_reason_stop", (names(hds_tmp)))
    hds_tmp <- hds_tmp |> rename(stopreason = colnames(hds_tmp)[w])
    
    # ------------- add to dataframe -------------
    hds_tmp <- hds_tmp |> select(Participant.Id, treatment, any_of("startdate"), enddate, stopreason)
    df_empty <- df_empty |>  bind_rows(hds_tmp)
  }
  
}
}

hds_systhist_new <- df_empty
rm(df_empty)

# set extreme dates (indicating missings) to NA
hds_systhist_new <- hds_systhist_new |> mutate(
  startdate = as.Date(startdate, format = "%Y-%m-%d"),
  enddate = as.Date(enddate, format = "%d-%m-%Y"),
)

w1 <- which(hds_systhist_new$startdate > as.Date("2900-01-01"))
w2 <- which(hds_systhist_new$enddate > as.Date("2900-01-01"))
if(length(w1)!=0) {
  hds_systhist_new[w, ]$startdate <- NA
}
if(length(w2)!=0) {
  hds_systhist_new[w, ]$enddate <- NA
}

# HDS - treatment ---------------------------------------------------------
hds_systhist_new <- hds_systhist_new |> mutate(
  treatment = case_when(
    treatment == "ciclo" ~ 1,
    treatment == "aza" ~ 3,
    treatment == "meth" ~ 2,
    treatment == "myco" ~ 4,
    treatment == "cort" ~ 5,
    treatment == "dupi" ~ 6,
    treatment == "omal" ~ 11,
    treatment == "tralo" ~ 9,
    treatment == "bari" ~ 14,
    treatment == "upada" ~ 15,
    treatment == "abro" ~ 13,
    treatment == "lebrik" ~ 8,
    treatment == "other" ~ 98,
    treatment == "inv" ~ 98
  )
)
# constraint: not all treatments are collected in TREAT-NL

# Final adjustments -------------------------------------------------------

# HDS:
# 1	adverse event
# 2	remission
# 3	insufficient response
# 4	relapse
# 5	cumulative dose
# 6	other causes
# 7	don't know reason for stopping

# TREAT-NL:
# 1=Insufficient response
# 2=Loss of treatment response (after initial good response)
# 3=Side effects
# 4=Cumulative dose
# 5=Disease remission
# 9=Other

hds_systhist_new <- hds_systhist_new |> mutate(
  treatment = as.integer(treatment),
  startdate = as.character(startdate),
  enddate = as.character(enddate),
  stopreason = case_when(
    stopreason == 1 ~ 3,
    stopreason == 2 ~ 4, ## EXTRA CHECKEN!
    stopreason == 3 ~ 1,
    stopreason == 4 ~ 5,
    stopreason == 5 ~ 2,
    stopreason == 9 ~ 6,
    TRUE ~ 7
  ),
  stopreason = as.integer(stopreason)
) |> 
  rename(anonymisedID = Participant.Id)
# note: different stopreason options than for current systemic therapy

# save HDS ----------------------------------------------------------------
write.csv(hds_systhist_new,
          paste0("../data/", export_date, "/hds/without-proms-data/hds.systemictherapyhx.csv"),
          row.names = FALSE
)