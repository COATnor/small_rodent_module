## ============================================================
## Download files from the COAT Data Portal for comparison
##
## Author: Suaiba Amina Salahuddin
##
## Purpose:
## Download selected V_snowbed_shrub_abundance_intensive files
## from the COAT Data Portal before comparing them with corrected
## files stored locally from Box.
##
## Original download functions:
## Hanna Böhner
## COATnor/data_management_scripts
## download_data_from_coat_data_portal.R
## ============================================================

rm(list = ls())

if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}

remotes::install_github(
  "ropensci/ckanr",
  upgrade = "always"
)

library(ckanr)
library(tidyverse)

COAT_key <- Sys.getenv("api_COAT")

if (COAT_key == "") {
  stop(
    "No COAT API token found. Store the token locally in .Renviron ",
    "using the variable name api_COAT."
  )
}

source(
  "https://github.com/COATnor/data_management_scripts/blob/master/download_data_from_coat_data_portal.R?raw=TRUE"
)

ckanr::ckanr_setup(
  url = "https://data.coat.no",
  key = COAT_key
)

## Check that the installed ckanr sends the required Authorization header
if (!(
  "Authorization" %in%
  names(ckanr:::auth_headers(ckanr::get_default_key()))
)) {
  stop(
    "The installed ckanr does not send the Authorization header. ",
    "Reinstall ckanr and restart R."
  )
}

dataset_name <- "v_snowbed_shrub_abundance_intensive_v2"

## Select only the year or years being checked.
## Add further years when needed.
years <- c(2011, 2022, 2024)

filenames <- paste0(
  "V_snowbed_shrub_abundance_intensive_",
  years,
  ".txt"
)

## Files are downloaded locally but are not committed to GitHub.
portal_dir <- "portal_files_for_comparison/version2"

dir.create(
  portal_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

## Optional: show the files available in the dataset
list_data_files(
  name = dataset_name
)

download_coat_data(
  name = dataset_name,
  filenames = filenames,
  store = "disk",
  out.dir = portal_dir
)

message("Portal files downloaded to: ", normalizePath(portal_dir))