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

# systemic therapy --------------------------------------------------------
df_syst <- read.csv(file = paste0("../data/", export_date, "/hds/hds.systemictherapy.csv"))
str(df_syst)

table(df_syst$treatment)
table(is.na(df_syst$treatment))
which(df_syst$treatment == "")

table(df_syst$mainad)
table(is.na(df_syst$mainad))
which(df_syst$mainad == "")

table(df_syst$concomitant)
table(is.na(df_syst$concomitant))
which(df_syst$concomitant == "")

summary(df_syst$dosage) # is 2000 mg a realistic value?
table(is.na(df_syst$dosage))
which(df_syst$dosage == "")

table(is.na(df_syst$dosage_other))
which(df_syst$dosage_other == "")

table(df_syst$frequency)
table(is.na(df_syst$frequency))
which(df_syst$frequency == "")

table(is.na(df_syst$frequency_other))
which(df_syst$frequency_other == "")

summary(as.Date(df_syst$startdate))
table(is.na(df_syst$startdate))
which(df_syst$startdate == "")

table(is.na(df_syst$ongoing))
which(df_syst$ongoing == "")

summary(as.Date(df_syst$enddate))
table(is.na(df_syst$enddate))
which(df_syst$enddate == "")

table(df_syst$stopreason)
table(is.na(df_syst$enddate))
which(df_syst$enddate == "")

table(df_syst$codosagetable(df_syst$concomitant)

table(is.na(df_syst$startdate))
subset(df_syst, anonymisedID == "AUMCD189")
tmp <- subset(hds_syst, Participant.Id == "AUMCD189") |> select(c(Participant.Id, startdate, enddate, enrolment_date, visitdate))

#startdate and enddate needs to be on the same row as the closest visitdate.
# right now it is on different rows...

tmp2 <- subset(df_merged, Participant.Id == "AUMCD189")|> select(c(Participant.Id, enrolment_date, visitdate, syst_ther_start_date, syst_ther_stop_date, contains("therapy_discontinuation")))



tmp3=subset(hds_syst_diff, Participant.Id == "AUMCD189")|> select(c(Participant.Id, enrolment_date, visitdate,startdate,enddate))

tmp3=subset(df, Participant.Id == "AUMCD189")

