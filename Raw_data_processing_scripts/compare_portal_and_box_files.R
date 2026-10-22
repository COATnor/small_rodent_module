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

portal_dir <- "portal_files_for_comparison/version3"

## Replace this with your actual Box directory
box_dir <- "/Users/ssa195/Library/CloudStorage/Box-Box/COAT/Modules/Small rodent module/data/V_snowbed_shrub_abundance_intensive/corrected files to data portal"
results_dir <- "comparison results/version3"

dir.create(
  results_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

years <- c(2011, 2022, 2024, 2025)

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
    col_types = readr::cols(
      .default = readr::col_character()
    ),
    trim_ws = FALSE,
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
  
  same_data <- isTRUE(
    all.equal(
      portal_sorted,
      box_sorted,
      check.attributes = FALSE
    )
  )
  ## Find complete rows present only in each source
  ##
  ## portal_only:
  ## rows present in the portal file but not in the Box file
  ##
  ## box_only:
  ## rows present in the Box file but not in the portal file
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
  
  
  ## Print comparison result
  message("Same column names: ", same_columns)
  message("Same data: ", same_data)
  message("Rows only in portal: ", nrow(portal_only))
  message("Rows only in Box: ", nrow(box_only))
  
  ## Show the reason if the comparison still fails
  if (!same_data) {
    message("\nExplanation from all.equal():")
    print(
      all.equal(
        portal_sorted,
        box_sorted,
        check.attributes = FALSE
      )
    )
  }
  
  
  ## Print exact unmatched rows to the R console
  if (nrow(portal_only) > 0) {
    
    message("\nEntries only in the COAT Portal file:")
    print(portal_only, n = Inf)
    
  } else {
    
    message("\nNo entries occur only in the COAT Portal file.")
  }
  
  
  if (nrow(box_only) > 0) {
    
    message("\nEntries only in the Box file:")
    print(box_only, n = Inf)
    
  } else {
    
    message("\nNo entries occur only in the Box file.")
  }
  
  
  ## Create a clean filename for output files
  file_stem <- tools::file_path_sans_ext(filename)
  
  
  ## Save portal-only rows
  write_csv(
    portal_only,
    file.path(
      results_dir,
      paste0(file_stem, "_portal_only.csv")
    )
  )
  
  
  ## Save Box-only rows
  write_csv(
    box_only,
    file.path(
      results_dir,
      paste0(file_stem, "_box_only.csv")
    )
  )
  
  
  ## Save a readable text report
  report_file <- file.path(
    results_dir,
    paste0(file_stem, "_differences.txt")
  )
  
  report <- capture.output({
    
    cat("Comparison for: ", filename, "\n\n", sep = "")
    
    cat("Portal file:\n")
    cat(normalizePath(portal_file, mustWork = FALSE), "\n\n")
    
    cat("Box file:\n")
    cat(normalizePath(box_file, mustWork = FALSE), "\n\n")
    
    cat("Portal rows: ", nrow(portal_data), "\n", sep = "")
    cat("Box rows: ", nrow(box_data), "\n", sep = "")
    cat("Same columns: ", same_columns, "\n", sep = "")
    cat("Same data: ", same_data, "\n\n", sep = "")
    
    cat(
      "Rows only in the COAT Portal file: ",
      nrow(portal_only),
      "\n\n",
      sep = ""
    )
    
    if (nrow(portal_only) > 0) {
      print(portal_only, n = Inf)
    } else {
      cat("None\n")
    }
    
    cat(
      "\nRows only in the Box file: ",
      nrow(box_only),
      "\n\n",
      sep = ""
    )
    
    if (nrow(box_only) > 0) {
      print(box_only, n = Inf)
    } else {
      cat("None\n")
    }
  })
  
  writeLines(
    report,
    con = report_file
  )
  
  
  ## Return one summary row
  tibble(
    filename = filename,
    status = ifelse(
      same_data,
      "match",
      "different"
    ),
    portal_rows = nrow(portal_data),
    box_rows = nrow(box_data),
    same_columns = same_columns,
    same_data = same_data,
    portal_only_rows = nrow(portal_only),
    box_only_rows = nrow(box_only)
  )
}


## ---------------------------------- ##
## RUN COMPARISON FOR ALL FILES
## ---------------------------------- ##

comparison <- bind_rows(
  lapply(
    filenames,
    compare_file
  )
)


## ---------------------------------- ##
## SAVE SUMMARY
## ---------------------------------- ##

summary_file <- file.path(
  results_dir,
  "portal_box_comparison.csv"
)

write_csv(
  comparison,
  summary_file
)


## ---------------------------------- ##
## PRINT SUMMARY
## ---------------------------------- ##

message("\n========================================")
message("Comparison completed")
message("========================================")

print(comparison)

message(
  "\nSummary saved to:\n",
  normalizePath(summary_file, mustWork = FALSE)
)