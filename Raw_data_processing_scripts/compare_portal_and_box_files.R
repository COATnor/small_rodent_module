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

portal_dir <- "portal_files_for_comparison/version2"

## Replace this with your actual Box directory
box_dir <- "/Users/yourname/Library/CloudStorage/Box-Box/COAT/Small rodent module/corrected files to data portal"
results_dir <- "comparison results/version2"

dir.create(
  results_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

years <- c(2011, 2022, 2024)

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
      status = "missing from portal",
      portal_rows = NA_integer_,
      box_rows = NA_integer_,
      same_columns = NA,
      same_data = FALSE
    ))
  }
  
  if (!file.exists(box_file)) {
    return(tibble(
      filename = filename,
      status = "missing from Box",
      portal_rows = NA_integer_,
      box_rows = NA_integer_,
      same_columns = NA,
      same_data = FALSE
    ))
  }
  
  portal_data <- read_coat_file(portal_file)
  box_data <- read_coat_file(box_file)
  
  ## Check whether both files contain the same column names
  same_columns <- setequal(
    names(portal_data),
    names(box_data)
  )
  
  if (!same_columns) {
    writeLines(
      c(
        "Columns only in portal file:",
        paste(setdiff(names(portal_data), names(box_data)), collapse = ", "),
        "",
        "Columns only in Box file:",
        paste(setdiff(names(box_data), names(portal_data)), collapse = ", ")
      ),
      con = file.path(
        results_dir,
        paste0(filename, "_column_differences.txt")
      )
    )
    
    return(tibble(
      filename = filename,
      status = "different columns",
      portal_rows = nrow(portal_data),
      box_rows = nrow(box_data),
      same_columns = FALSE,
      same_data = FALSE
    ))
  }
  
  ## Put the Box columns in the same order as the portal columns
  box_data <- box_data %>%
    select(all_of(names(portal_data)))
  
  ## Sort rows so that row order does not affect the comparison
  portal_sorted <- portal_data %>%
    arrange(across(everything()))
  
  box_sorted <- box_data %>%
    arrange(across(everything()))
  
  same_data <- identical(
    portal_sorted,
    box_sorted
  )
  
  ## If different, save rows only found in each source
  if (!same_data) {
    
    portal_only <- anti_join(
      portal_data,
      box_data,
      by = names(portal_data)
    )
    
    box_only <- anti_join(
      box_data,
      portal_data,
      by = names(portal_data)
    )
    
    write_csv(
      portal_only,
      file.path(
        results_dir,
        paste0(filename, "_portal_only.csv")
      )
    )
    
    write_csv(
      box_only,
      file.path(
        results_dir,
        paste0(filename, "_box_only.csv")
      )
    )
  }
  
  tibble(
    filename = filename,
    status = ifelse(same_data, "match", "different"),
    portal_rows = nrow(portal_data),
    box_rows = nrow(box_data),
    same_columns = same_columns,
    same_data = same_data,
    portal_only_rows = ifelse(
      same_data,
      0,
      nrow(anti_join(
        portal_data,
        box_data,
        by = names(portal_data)
      ))
    ),
    box_only_rows = ifelse(
      same_data,
      0,
      nrow(anti_join(
        box_data,
        portal_data,
        by = names(portal_data)
      ))
    )
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