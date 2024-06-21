# Basic script to install packages

# List of packages to install
packages_to_install <- c("tidyverse", "janitor", "lubridate",
                        "jsonlite", "readxl",
                        "skimr", "gtsummary", "knitr", "apyramid")

# For each package in the list, install if not already installed
for (package in packages_to_install) {
 if (!requireNamespace(package, quietly = TRUE)) {
   install.packages(package, dependencies = TRUE)
 }
}

# Remove the list
rm(packages_to_install)

# For setting up a reproducible environment
# If you want to use renv (https://rstudio.github.io/renv/articles/renv.html)

#install.packages("renv")
# - creating a snapshot of the libraries
#renv::snapshot()
# - the renv restore at the beginning of the script would redirect to the libraries saved already during the snapshot
#renv::restore() 
