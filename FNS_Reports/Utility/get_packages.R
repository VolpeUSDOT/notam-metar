# Get all necessary packages across data prep and analysis scripts 
# add any packages your scripts require here. Keep in alphabetical order.

loadpacks <- c(
  "dplyr",
  "DT",
  "ggplot2",
  "egg",
  "kableExtra",
  "knitr",
  "lubridate",
  "plotly",
  "readxl",
  "reshape2",
  "rmarkdown",
  "tidyr",
  "tidyverse"
)

completed_installs = 0

# Look for an exact match of each package name in the available packages
# If not already download, attempt to download and install
# Provide warnings or errors if any

for(i in loadpacks){
  
    tryCatch({
      if(!any(grepl(paste0('^', i, '$'), .packages(all.available=T)))) {
        utils::install.packages(i, dependencies = TRUE, repos = "http://cran.us.r-project.org")
        completed_installs = completed_installs + 1
      }
    },
    
    warning = function(warn) {
      print(warn)
      cat(paste('\n Install exited with warning, probably because ->', i, '<- does not exist in the CRAN repository. Check spelling.'))
      
    },
    
    error = function(error_condition) {
      cat(paste('Install exited with an error. \n Check list of packages to make sure all are expected to exist in CRAN reporistory.'))
    },
    
    finally = {
      NULL
    })
    
}

if(completed_installs > 0) {
  cat('Successfully installed', completed_installs, 'packages and dependences for R installed at', Sys.getenv('R_HOME'))
}

rm(i, loadpacks)



