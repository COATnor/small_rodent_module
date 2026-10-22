## ============================================================
## Script to compare COAT Data Portal files with corrected Box data files
##
## Author: Suaiba Amina Salahuddin
##
## Purpose:
## Check that corrected annual files have been uploaded correctly
## to the COAT Data Portal.
## ============================================================

library(readr)
library(dplyr)
library(tibble)

portal_dir <- "portal files for comparison"
box_dir <- "corrected files to data portal"
results_dir <- "comparison results"

dir.create(
  results_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

years <- 2025

filenames <- paste0(
  "V_snowbed_shrub_abundance_intensive_",
  years,
  ".txt"
)

read_coat_file <- function(path) {
  readr::read_delim(
    file = path,
    delim = ";",
    na = "NA",
    show_col_types = FALSE
  )
}

compare_file <- function(filename) {
  
  portal_file <- file.path(portal_dir, filename)
  box_file <- file.path(box_dir, filename)
  
  if (!file.exists(portal_file)) {
    return(tibble(
      filename = filename,
      status = "missing from portal download",
      portal_rows = NA_integer_,
      box_rows = NA_integer_,
      same_columns = NA,
      same_data = FALSE
    ))
  }
  
  if (!file.exists(box_file)) {
    return(tibble(
      filename = filename,
      status = "missing from Box/corrected files",
      portal_rows = NA_integer_,
      box_rows = NA_integer_,
      same_columns = NA,
      same_data = FALSE
    ))
  }
  
  portal_data <- read_coat_file(portal_file)
  box_data <- read_coat_file(box_file)
  
  same_columns <- identical(
    names(portal_data),
    names(box_data)
  )
  
  if (same_columns) {
    
    portal_sorted <- portal_data %>%
      arrange(across(everything()))
    
    box_sorted <- box_data %>%
      arrange(across(everything()))
    
    same_data <- identical(
      portal_sorted,
      box_sorted
    )
    
  } else {
    same_data <- FALSE
  }
  
  tibble(
    filename = filename,
    status = ifelse(same_data, "match", "different"),
    portal_rows = nrow(portal_data),
    box_rows = nrow(box_data),
    same_columns = same_columns,
    same_data = same_data
  )
}

comparison <- bind_rows(
  lapply(filenames, compare_file)
)

write_csv(
  comparison,
  file.path(
    results_dir,
    "portal_box_comparison.csv"
  )
)

print(comparison)