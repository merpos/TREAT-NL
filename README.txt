TREAT- NL HDS conversion code
Author: Merel Postema

Description:
This folder contains all the R code needed to convert raw Castor CSV exports into a harmonized dataset (HDS).

Input files: ~../data/[export_date]/castor-export/
Output files: ~../data/[export_date]/hds/

Instructions:
1) Export data
- Export your study data from Castor EDC into CSV format

2) Prepare folder structure
- Create a new folder named after the export date (format: yyyymmdd)
- Inside this folder, create:
- castor-export -> store the raw Castor CSV files here
- hds -> output files will be stored here

3) Open R project
- Open RStudio and load the project d2t-scripts.Rproj via File > Open Project

4) Open main script
- Open the script 01-main-script-hds.R

5) Adjust parameters
- Update the export_date variable (line 10)
- Adjust the library paths if necessary (lines 11-16)

6) Run the script
- Click the Source button, or
- Select all the code and click Run

7) Output and logs
- Processed files will be saved in the hds folder: ~../data/[export_date]/hds/
- A log file will be generated in the scripts folder: hds_output_log_[yyyymmdd].txt
