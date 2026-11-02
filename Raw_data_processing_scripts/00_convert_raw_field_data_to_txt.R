# This script converts raw field data on shrubs in snowbeds (.csv, .xlsx, .xls or .txt) to the
# tab-delimited .txt format that 01_edit_raw_data_shrubs_in_snowbeds_COAT.R reads.
#
# Author: Suaiba Amina Salahuddin
#
# What the script does:
#  1. Reads the file (for Excel workbooks it picks the sheet with "localities" in its name)
#  2. Finds the site (VJ or KO) and the year (e.g. 2026) in the file name
#  3. Cleans the table lightly (removes empty rows and empty unnamed columns,
#     converts Excel date numbers to dd.mm.yyyy)
#  4. Checks the column names against the expected ones
#  5. Writes snowbed_shrubs_<site>_<year>.txt, e.g. snowbed_shrubs_VJ_2026.txt
#
# The original file is never changed. Open the new .txt file in a text editor
# and check it before running script 01.

# Install once if necessary:
# install.packages("readxl")

suppressPackageStartupMessages({
  library(readxl)
})

# Files to convert. Leave empty to pick a file from your computer in a dialog window.
# Example: input_files <- c("raw data from field/Snowbed shrubs VJ 2026.xlsx")
input_files <- c("/Users/username/Library/CloudStorage/Box-Box/COAT/Modules/Small rodent module/data/V_snowbed_shrub_abundance_intensive/raw data from field/Snowbed vegetation data VJ 2026.csv", "/Users/ssa195/Library/CloudStorage/Box-Box/COAT/Modules/Small rodent module/data/V_snowbed_shrub_abundance_intensive/raw data from field/snowbed_vegetation_data_KO_2026.xlsx")

# Option 2: automatically find files in this directory.
input_dir <- "/Users/username/Library/CloudStorage/Box-Box/COAT/Modules/Small rodent module/data/V_snowbed_shrub_abundance_intensive/raw data from field/"

# Where the converted .txt files should be saved
out.dir <- "/Users/username/Library/CloudStorage/Box-Box/COAT/Modules/Small rodent module/data/V_snowbed_shrub_abundance_intensive/raw data from field/"

# Overwrite an existing snowbed_shrubs_<site>_<year>.txt? (TRUE or FALSE)
overwrite <- FALSE

# If TRUE, the script stops after the first error.
# If FALSE, it reports errors and continues with the remaining files.
stop_on_error <- FALSE

# Only fill in the three settings below if the script stops and asks for them.
# They apply to every file in input_files, so use them with one file at a time.
# Examples: site_override <- "KO"
#           year_override <- "2026"
#           sheet_override <- "pre-filled localities KO 2026"
site_override  <- NULL   # site if it cannot be found in the file name ("VJ" or "KO")
year_override  <- NULL   # year if it cannot be found in the file name, e.g. "2026"
sheet_override <- NULL   # exact Excel sheet name to use instead of choosing automatically

# Optional file-specific overrides.
#
# The names must be the complete input file paths.
#
# Example:
# site_overrides <- c(
#   "/path/to/file_without_site_name.xlsx" = "VJ"
# )
site_overrides <- character(0)
year_overrides <- character(0)
sheet_overrides <- character(0)

# Allowed site codes
sites <- c("VJ", "KO")

# Excel sheets considered to be locality sheets
sheet_pattern <- "pre[- ]?filled[ _-]*localities|localities"

# Other ways the site may be written in a sheet name, besides the code itself
# (regular expressions, not case sensitive). Add more if your sheets use other names.
site_aliases <- c(VJ = "vestre[ _-]?jakobselv",
                  KO = "komagdalen")

# Allowed site codes in the file name
sites <- c("VJ", "KO")

# Column names that script 01 expects
expected_titles <- c("date", "locality", "site", "plot", "treatment", "subplot",
                     "emp_nig", "vac_myr", "vac_uli", "bet_nan", "vac_vit", "phy_cae",
                     "nar_str", "hup_sel", "sal_her", "comment", "observer")
# Supported input extensions
supported_extensions <- c("csv", "txt", "tsv", "xlsx", "xls", "xlsm")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
################# Utility functions  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

get_file_setting <- function(values, path, default = NULL) {
  if (length(values) == 0) {
    return(default)
  }
  
  if (!is.null(names(values)) && path %in% names(values)) {
    return(unname(values[[path]]))
  }
  
  default
}

get_site <- function(filename, site_setting = NULL) {
  if (!is.null(site_setting)) {
    site <- toupper(as.character(site_setting))
    
    if (!site %in% sites) {
      stop(
        "Invalid site override '", site_setting,
        "'. Expected one of: ", paste(sites, collapse = ", "),
        call. = FALSE
      )
    }
    
    return(site)
  }
  
  name <- tools::file_path_sans_ext(basename(filename))
  
  pattern <- paste0(
    "(?<![A-Za-z])(",
    paste(sites, collapse = "|"),
    ")(?![A-Za-z])"
  )
  
  hits <- regmatches(
    name,
    gregexpr(pattern, name, perl = TRUE, ignore.case = TRUE)
  )[[1]]
  
  hits <- unique(toupper(hits))
  
  if (length(hits) == 1) {
    return(hits)
  }
  
  stop(
    "Could not find exactly one site in file name '",
    basename(filename), "'.\n",
    "Expected one of: ", paste(sites, collapse = ", "), ".\n",
    "Add a file-specific entry to site_overrides, for example:\n\n",
    "site_overrides <- c(",
    "\n  \"", filename, "\" = \"VJ\"\n)",
    call. = FALSE
  )
}

get_year <- function(filename, year_setting = NULL) {
  if (!is.null(year_setting)) {
    year <- as.character(year_setting)
    
    if (!grepl("^20[0-9]{2}$", year)) {
      stop(
        "Invalid year override '", year_setting,
        "'. Use a four-digit year such as 2026.",
        call. = FALSE
      )
    }
    
    return(year)
  }
  
  name <- tools::file_path_sans_ext(basename(filename))
  
  hits <- unique(regmatches(
    name,
    gregexpr("(?<![0-9])20[0-9]{2}(?![0-9])", name, perl = TRUE)
  )[[1]])
  
  if (length(hits) == 1) {
    return(hits)
  }
  
  stop(
    "Could not find exactly one four-digit year in file name '",
    basename(filename), "'.\n",
    "Add a file-specific entry to year_overrides, for example:\n\n",
    "year_overrides <- c(",
    "\n  \"", filename, "\" = \"2026\"\n)",
    call. = FALSE
  )
}

site_in_name <- function(sheet_names, site) {
  alias <- site_aliases[[site]]
  
  pattern <- paste0(
    "(?<![A-Za-z])", site, "(?![A-Za-z])",
    "|", alias
  )
  
  grepl(pattern, sheet_names, perl = TRUE, ignore.case = TRUE)
}

choose_sheet <- function(path, site, sheet_setting = NULL) {
  sheets <- excel_sheets(path)
  
  if (length(sheets) == 0) {
    stop("The workbook contains no sheets: ", basename(path), call. = FALSE)
  }
  
  # Explicit sheet setting takes priority
  if (!is.null(sheet_setting)) {
    if (!sheet_setting %in% sheets) {
      stop(
        "The requested sheet '", sheet_setting,
        "' does not exist in ", basename(path), ".\n",
        "Available sheets: ", paste(sheets, collapse = ", "),
        call. = FALSE
      )
    }
    
    message("Using explicitly configured sheet: ", sheet_setting)
    return(sheet_setting)
  }
  
  # A one-sheet workbook is unambiguous
  if (length(sheets) == 1) {
    message("Using the only sheet: ", sheets)
    return(sheets)
  }
  
  # Find locality-related sheets
  candidates <- grep(
    sheet_pattern,
    sheets,
    ignore.case = TRUE,
    value = TRUE
  )
  
  if (length(candidates) == 0) {
    stop(
      "No locality sheet found in ", basename(path), ".\n",
      "Available sheets: ", paste(sheets, collapse = ", "), "\n",
      "Add a file-specific entry to sheet_overrides if necessary.",
      call. = FALSE
    )
  }
  
  # Find sheets belonging to the selected site
  has_selected_site <- site_in_name(candidates, site)
  
  other_sites <- setdiff(sites, site)
  
  has_other_site <- rep(FALSE, length(candidates))
  
  for (other_site in other_sites) {
    has_other_site <- has_other_site |
      site_in_name(candidates, other_site)
  }
  
  site_sheets <- candidates[has_selected_site & !has_other_site]
  
  if (length(site_sheets) == 1) {
    message("Using sheet: ", site_sheets)
    return(site_sheets)
  }
  
  if (length(site_sheets) > 1) {
    stop(
      "More than one sheet matches site ", site, " in ",
      basename(path), ":\n  ",
      paste(site_sheets, collapse = "\n  "), "\n",
      "Add a file-specific entry to sheet_overrides.",
      call. = FALSE
    )
  }
  
  # If exactly one locality sheet has no site name, use it
  no_site_sheets <- candidates[!has_selected_site & !has_other_site]
  
  if (length(no_site_sheets) == 1) {
    message(
      "No sheet explicitly mentions site ", site,
      ". Using the only unlabelled locality sheet: ",
      no_site_sheets
    )
    return(no_site_sheets)
  }
  
  stop(
    "Could not identify a unique sheet for site ", site,
    " in ", basename(path), ".\n",
    "Candidate sheets:\n  ",
    paste(candidates, collapse = "\n  "), "\n",
    "Add a file-specific entry to sheet_overrides.",
    call. = FALSE
  )
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
################# Reading functions  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

guess_delimiter <- function(path) {
  lines <- readLines(path, n = 10, warn = FALSE, encoding = "UTF-8")
  
  if (length(lines) == 0) {
    stop("The file is empty: ", basename(path), call. = FALSE)
  }
  
  delimiter_counts <- c(
    "\t" = sum(lengths(regmatches(lines, gregexpr("\t", lines, fixed = TRUE)))),
    ";"  = sum(lengths(regmatches(lines, gregexpr(";", lines, fixed = TRUE)))),
    ","  = sum(lengths(regmatches(lines, gregexpr(",", lines, fixed = TRUE))))
  )
  
  delimiter <- names(which.max(delimiter_counts))
  
  if (max(delimiter_counts) == 0) {
    stop(
      "Could not detect a delimiter in ", basename(path),
      ". Expected tab, semicolon or comma separated data.",
      call. = FALSE
    )
  }
  
  delimiter
}

read_delimited <- function(path) {
  delimiter <- guess_delimiter(path)
  
  delimiter_label <- switch(
    delimiter,
    "\t" = "tab",
    ";" = "semicolon",
    "," = "comma"
  )
  
  message(
    "Reading delimited text using ", delimiter_label,
    " separator: ", basename(path)
  )
  
  read.table(
    file = path,
    header = TRUE,
    sep = delimiter,
    quote = "\"",
    comment.char = "",
    fill = TRUE,
    colClasses = "character",
    na.strings = c("", "NA", "N/A"),
    check.names = FALSE,
    strip.white = TRUE,
    stringsAsFactors = FALSE,
    fileEncoding = "UTF-8-BOM"
  )
}

read_input_file <- function(path, site, sheet_setting = NULL) {
  extension <- tolower(tools::file_ext(path))
  
  if (extension %in% c("xlsx", "xls", "xlsm")) {
    sheet <- choose_sheet(path, site, sheet_setting)
    
    return(as.data.frame(
      read_excel(
        path = path,
        sheet = sheet,
        col_types = "text",
        .name_repair = "minimal"
      ),
      stringsAsFactors = FALSE
    ))
  }
  
  if (extension %in% c("csv", "txt", "tsv")) {
    return(read_delimited(path))
  }
  
  stop(
    "Unsupported file extension '.", extension,
    "'. Supported extensions are: ",
    paste(supported_extensions, collapse = ", "),
    call. = FALSE
  )
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
################# Cleaning functions  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

clean_data <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  
  # Clean column names lightly
  names(df) <- trimws(names(df))
  
  # Convert all cells to character, remove tabs/line breaks and trim whitespace
  df[] <- lapply(df, function(x) {
    x <- as.character(x)
    x <- gsub("[\t\r\n]+", " ", x)
    x <- trimws(x)
    x[x == ""] <- NA_character_
    x
  })
  
  # Remove completely empty rows
  if (nrow(df) > 0) {
    df <- df[rowSums(!is.na(df)) > 0, , drop = FALSE]
  }
  
  # Remove completely empty unnamed columns
  unnamed <- is.na(names(df)) |
    names(df) == "" |
    grepl("^\\.\\.\\.[0-9]+$", names(df)) |
    grepl("^X(\\.[0-9]+)?$", names(df))
  
  if (ncol(df) > 0) {
    empty_columns <- colSums(!is.na(df)) == 0
    df <- df[, !(unnamed & empty_columns), drop = FALSE]
  }
  
  df
}

fix_dates <- function(df) {
  if (!"date" %in% names(df)) {
    return(df)
  }
  
  x <- as.character(df$date)
  x <- trimws(x)
  
  # Excel serial dates, including values such as 46188 or 46188.0
  serial <- !is.na(x) &
    grepl("^[0-9]{4,6}(\\.0+)?$", x)
  
  if (any(serial)) {
    numeric_dates <- as.numeric(x[serial])
    
    valid <- numeric_dates > 10000 & numeric_dates < 100000
    
    converted <- rep(NA_character_, length(numeric_dates))
    converted[valid] <- format(
      as.Date(numeric_dates[valid], origin = "1899-12-30"),
      "%d.%m.%Y"
    )
    
    x[serial] <- converted
    
    message(sum(serial & !is.na(x)), " Excel date value(s) converted")
  }
  
  # ISO dates: 2026-07-15 or 2026-07-15 00:00:00
  iso <- !is.na(x) &
    grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", x)
  
  if (any(iso)) {
    converted <- as.Date(substr(x[iso], 1, 10), format = "%Y-%m-%d")
    x[iso] <- format(converted, "%d.%m.%Y")
    message(sum(iso), " ISO date(s) converted")
  }
  
  # Dates using slash separators, e.g. 15/07/2026
  slash <- !is.na(x) &
    grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}$", x)
  
  if (any(slash)) {
    converted <- as.Date(x[slash], format = "%d/%m/%Y")
    x[slash] <- format(converted, "%d.%m.%Y")
    message(sum(slash), " slash-format date(s) converted")
  }
  
  # Warn about remaining non-empty values that are not dd.mm.yyyy
  valid_format <- is.na(x) |
    grepl("^[0-9]{2}\\.[0-9]{2}\\.[0-9]{4}$", x)
  
  invalid <- !valid_format
  
  if (any(invalid)) {
    warning(
      sum(invalid),
      " date value(s) are not in dd.mm.yyyy format. Examples: ",
      paste(head(unique(x[invalid]), 5), collapse = ", "),
      call. = FALSE
    )
  }
  
  df$date <- x
  df
}

check_columns <- function(df) {
  actual <- names(df)
  
  missing <- setdiff(expected_titles, actual)
  extra <- setdiff(actual, expected_titles)
  
  if (length(missing) > 0) {
    warning(
      "Missing expected columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  
  if (length(extra) > 0) {
    warning(
      "Unexpected columns: ",
      paste(extra, collapse = ", "),
      call. = FALSE
    )
  }
  
  if (length(missing) == 0 &&
      length(extra) == 0 &&
      !identical(actual, expected_titles)) {
    warning(
      "All expected columns are present, but their order differs from script 01.",
      call. = FALSE
    )
  }
  
  if (identical(actual, expected_titles)) {
    message("Column names match the expected names.")
    return(TRUE)
  }
  
  FALSE
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
################# File discovery  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

find_input_files <- function() {
  if (length(input_files) > 0) {
    files <- normalizePath(
      input_files,
      winslash = "/",
      mustWork = FALSE
    )
  } else {
    if (!dir.exists(input_dir)) {
      stop(
        "input_dir does not exist:\n", input_dir, "\n",
        "Either correct input_dir or list files explicitly in input_files.",
        call. = FALSE
      )
    }
    
    files <- list.files(
      path = input_dir,
      pattern = paste0(
        "\\.(",
        paste(supported_extensions, collapse = "|"),
        ")$"
      ),
      ignore.case = TRUE,
      recursive = recursive_search,
      full.names = TRUE
    )
    
    # Prevent previously generated outputs from being reprocessed
    files <- files[
      !grepl(
        "^snowbed_shrubs_[A-Z]+_20[0-9]{2}\\.txt$",
        basename(files),
        ignore.case = TRUE
      )
    ]
    
    files <- normalizePath(
      files,
      winslash = "/",
      mustWork = FALSE
    )
  }
  
  files <- unique(files)
  
  if (length(files) == 0) {
    stop(
      "No input files were found.\n",
      "Check input_dir or specify files explicitly in input_files.",
      call. = FALSE
    )
  }
  
  missing_files <- files[!file.exists(files)]
  
  if (length(missing_files) > 0) {
    stop(
      "These input files do not exist:\n  ",
      paste(missing_files, collapse = "\n  "),
      call. = FALSE
    )
  }
  
  files
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
################# Conversion  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

convert_raw_file <- function(path) {
  message("\n--- Processing: ", basename(path))
  
  site_setting <- get_file_setting(
    site_overrides,
    path,
    default = site_override
  )
  
  year_setting <- get_file_setting(
    year_overrides,
    path,
    default = year_override
  )
  
  sheet_setting <- get_file_setting(
    sheet_overrides,
    path,
    default = sheet_override
  )
  
  site <- get_site(path, site_setting)
  year <- get_year(path, year_setting)
  
  df <- read_input_file(
    path = path,
    site = site,
    sheet_setting = sheet_setting
  )
  
  df <- clean_data(df)
  df <- fix_dates(df)
  
  check_columns(df)
  
  if (!dir.exists(out.dir)) {
    dir.create(out.dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  output_file <- file.path(
    out.dir,
    paste0("snowbed_shrubs_", site, "_", year, ".txt")
  )
  
  if (file.exists(output_file) && !overwrite) {
    stop(
      "Output file already exists: ", output_file, "\n",
      "Set overwrite <- TRUE or remove the old file.",
      call. = FALSE
    )
  }
  
  write.table(
    df,
    file = output_file,
    sep = "\t",
    quote = FALSE,
    row.names = FALSE,
    na = "",
    fileEncoding = "UTF-8"
  )
  
  message(
    "Written: ", output_file,
    " (", nrow(df), " rows, ", ncol(df), " columns)"
  )
  
  invisible(output_file)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
################# Run  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

files_to_process <- find_input_files()

message(
  "\nFound ", length(files_to_process),
  " input file", ifelse(length(files_to_process) == 1, "", "s"), "."
)

results <- list()
errors <- character(0)

for (file_path in files_to_process) {
  result <- tryCatch(
    convert_raw_file(file_path),
    
    error = function(e) {
      message(
        "\nFAILED: ", basename(file_path),
        "\nReason: ", conditionMessage(e)
      )
      
      errors <<- c(
        errors,
        paste0(basename(file_path), ": ", conditionMessage(e))
      )
      
      NULL
    }
  )
  
  results[[file_path]] <- result
  
  if (stop_on_error && length(errors) > 0) {
    stop("Stopping because stop_on_error <- TRUE.", call. = FALSE)
  }
}

message("\nConversion finished.")

if (length(errors) > 0) {
  message(
    "\nFiles with errors:\n- ",
    paste(errors, collapse = "\n- ")
  )
} else {
  message("All files converted successfully.")
}

message(
  "\nNext step: open the generated .txt file(s), check them, ",
  "and then run 01_edit_raw_data_shrubs_in_snowbeds_COAT.R."
)