# Staff model analysis of NOTAMS by Service Area

- Creates optimized staffing models by Service Area for the 90th percentile busiest days across the whole NAS
- Does this for the 90th percentile day in each of season x weekend/weekday type combination, 8 days in total

## Data

- NOTAMS were downloaded from the EN2 FSS NOTAM Action Report query tool at the FNS Reports page: `https://notams.aim.faa.gov/fnsreports/main.html`.
  + Downloaded files are named as `report_2423572413724759606.csv`, with the file name being a unique identifier for the download. Files need to be concatenated together to be useful. 
  + Each file represents 7 - 10 days of NOTAM records.
  + NOTAM action reports were downloaded with `Status = All`,  
- `FNS_Regions.xlsx` was compiled manually with input from FAA staff to assign FPAs to Service Areas


## How to run

This process was developed with the statistical programming language R, using R version 4.0.2, and edited in RStudio Desktop. 

Either clone the git repository (see below) or request a .zip file containing the code, and place it in a directory named `notam-metar`. Data files should be in a folder named `Data` within this directory.

There are several scripts for this work. 

1. `NOTAM_FNS_Analysis.Rmd`
  + Compiles downloaded files into one data frame, and saves as `FNS_Reports.RData` as a single compressed file for easier reuse.
  + Prepares initial data data assessment
  + Filters out identical NOTAM IDs that occur within 10 minutes of each other, as well as NOTAM types 'Initiated' or 'Submitted for Review'. 
  + Makes histograms by Service Area and by season
  + Prepares data for detailed analysis and saves as `FNS_NOTAM_Freq_w_busy_days.RData` for next step.

2. Function scripts.
  + `Find_percentile_days.R`. This script finds which days in the study period match a certain percentile of business (in terms of total number of NOTAMs received). Currently configured to find the 90th percentile days either across the NAS by season and weekday type, or for each service area combination.
  + `NOTAM_demand_plots.R`. This generates histograms of NOTAM counts by day across service area combinations and seasons, as well as hourly and cumulative demand plots for each service area combination and season. 

3. `NOTAM_shift_algorithm.R`. For each service area combination and season, generate suitable staff allocations 
  + Identifies 90th percentile days by season and service area combination
  + Calculates staff needed using an optimization method on the function `compute_staff_model()` to find the staff needed to achieve a specified mean NOTAM processing delay target (e.g., 2 minutes)
  + Creates schedules for each season using the function `optimize_staff_allocation()` on the 90th percentile days for each season and the number of staff determined with `compute_staff_model()`
  + Writes the staffing model for each season and service area combination to .csv files
  + Plots the results in individual .png files using the function `plot_staffing_model`
    

4. Analysis
  + `Compare_Staffing_Model_Analyses.R` looks at the results of the optimized models and prepares summaries for the report.

## Code 

This code is maintained in `https://github.com/VolpeUSDOT/notam-metar/`. Contact `daniel.flynn@dot.gov` for access.

This code directory is `FNS_Reports`, within the `notam-metar` repository. The other contents of the repository include code for previous analyses of NOTAM frequency, as well as code for an analysis of METARs.

