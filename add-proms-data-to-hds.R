## ============================================================================
## author:      Merel Postema
## date:        29-07-2025
## Rversion:    4.4.3
## project:     TREAT-NL
## description: add additional PROMS data (from Excel) to the HDS
## ============================================================================
cat("--------------- add PROMS data from Excel ---------------\n")

# load data ---------------------------------------------------------------
files <- c(
  "TREAT PRO abrocitinib data tm maart 2023.csv",
  "TREAT PRO baricitinib data tm maart 2023.csv",
  "TREAT PRO cellcept data tm maart 2023.csv",
  "TREAT PRO ciclosporine data tm maart 2023.csv",
  "TREAT PRO dupilumab data tm maart 2023.csv",
  "TREAT PRO lichttherapie data tm maart 2023.csv",
  "TREAT PRO methotrexaat data tm maart 2023.csv",
  "TREAT PRO systemische corticosteroiden data tm maart 2023.csv",
  "TREAT PRO tralokinumab data tm maart 2023.csv",
  "TREAT PRO upadacitinib data tm maart 2023.csv"
)

# Note: not yet stored as CSV. 

data <- NULL 
  for (f in files) {
    df_tmp <- read.csv(paste0("../data/", f), sep = ";") 
    df_pt <- bind_rows(df_pt, df_tmp)  
  }
  
