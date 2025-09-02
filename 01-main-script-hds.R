## ============================================================================
## author:      Merel Postema
## date:        24-07-2025
## Rversion:    4.4.3
## project:     TREAT-NL
## description: main script to create harmonized dataset (hds) files 
## ============================================================================

# to be changed accordingly -----------------------------------------------
export_date <- "20250722"
my_lib_path <-
  paste0(
    "C:/Users/P076169/Amsterdam UMC/MyRPackages/",
    paste0(version$major, ".", version$minor),
    "/"
  )

# libraries ------------------------------------------------------------------
if (file.exists(my_lib_path) == FALSE) {
  dir.create(my_lib_path)
}
.libPaths(my_lib_path)

packages <- c("dplyr", "tidyr")
install.packages(setdiff(packages, rownames(installed.packages())))
load_pack <- lapply(packages, library, character.only = TRUE)

# capture output to log file -----------------------------------------------
log_file <- paste0("hds_output_log_", export_date, ".txt")
sink(file = log_file, append = TRUE, split = TRUE)

# source all hds scripts --------------------------------------------------

options(warn = 1)
# the below script should be run first. 
source("hds-visits.R", echo=FALSE)
# depends on ids2include (defined in hds-visits.R)
source("hds-demographics.R", echo=FALSE) 
source("hds-systemictherapy.R", echo=FALSE) 
source("hds-dlqi.R", echo=FALSE) 
source("hds-comorbidities.R", echo=FALSE) 
source("hds-phototherapy.R", echo=FALSE)
source("hds-topicaltherapy.R", echo=FALSE) 
# depends on ids2include and hds_visit (defined in hds-visits.R)
source("hds-poem.R", echo=FALSE)
source("hds-nrs.R", echo=FALSE)
# depends on hds_visit (defined in hds-visits.R)
source("hds-systemictherapyhx.R", echo=FALSE) 
source("hds-iga.R", echo=FALSE) 
source("hds-easi.R", echo=FALSE) 

cat("------------------ session info -----------------------")
sessionInfo()

options(warn = 0)
#while (sink.number() > 0) sink()
sink(type = "message")
sink(file=NULL)
## END ##