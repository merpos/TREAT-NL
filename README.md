README
================

## TREAT- NL HDS conversion code

# Description:

This repository contains all the R code needed to convert raw Castor CSV
exports from the [TREAT-NL](https://treatregister.nl/) dataset into a
harmonized dataset (HDS), and international effort by the TREAT Registry
Taskforce to compare national datasets and enable pooled analyses.

Input files: `~../data/[export_date]/castor-export/`  
Output files: `~../data/[export_date]/hds/`

## Instructions

### 1. Export Data

- Export your study data from Castor EDC into CSV format.

### 2. Prepare Folder Structure

- Create a new folder named after the export date (format:
  `yyyymmdd`).  
- Inside this folder, create the following subfolders:
  - `castor-export` → store the raw Castor CSV files here  
  - `hds` → processed output files will be saved here

### 3. Open R Project

- Open RStudio and load the project `d2t-scripts.Rproj` via File \> Open
  Project.

### 4. Open Main Script

- Open the script `01-main-script-hds.R`.

### 5. Adjust Parameters

- Update the `export_date` variable (line 10).  
- Adjust the library paths if necessary (lines 11–16).

### 6. Run the Script

- Click the Source button, or  
- Select all the code and click Run.

### 7. Output and Logs

- Processed files will be saved in the `hds` folder:
  `~../data/[export_date]/hds/`.  
- A log file will be generated in the `scripts` folder:
  `hds_output_log_[yyyymmdd].txt`.
