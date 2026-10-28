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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
################# Settings  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Files to convert. Leave empty to pick a file in a dialog window.
# Example: input_files <- c("raw data from field/Snowbed shrubs VJ 2026.xlsx")
input_files <- character(0)

# Where the converted .txt files should be saved
out.dir <- "raw data from field/"

# Excel sheet to use: the sheet whose name contains this text (not case sensitive)
sheet_pattern <- "localities"

# Allowed site codes in the file name
sites <- c("VJ", "KO")

# Column names that script 01 expects
expected_titles <- c("date", "locality", "site", "plot", "treatment", "subplot",
                     "emp_nig", "vac_myr", "vac_uli", "bet_nan", "vac_vit", "phy_cae",
                     "nar_str", "hup_sel", "sal_her", "comment", "observer")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
################# Functions  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

library(readxl)

# ask the user a question (only possible when running interactively)
ask_text <- function(prompt) {
  if (!interactive()) stop(prompt, " (cannot ask: R is not running interactively)", call. = FALSE)
  trimws(readline(paste0(prompt, " ")))
}

# site code (VJ or KO) from the file name
get_site <- function(filename) {
  name <- tools::file_path_sans_ext(basename(filename))
  pattern <- paste0("(?<![A-Za-z])(", paste(sites, collapse = "|"), ")(?![A-Za-z])")
  hits <- regmatches(name, gregexpr(pattern, name, perl = TRUE, ignore.case = TRUE))[[1]]
  hits <- unique(toupper(hits))
  if (length(hits) == 1) return(hits)
  
  message("Could not find exactly one site (", paste(sites, collapse = " or "),
          ") in the file name: ", basename(filename))
  repeat {
    answer <- toupper(ask_text(paste0("Site (", paste(sites, collapse = "/"), "):")))
    if (answer %in% sites) return(answer)
    message("Please type one of: ", paste(sites, collapse = ", "))
  }
}

# four-digit year (20xx) from the file name
get_year <- function(filename) {
  name <- tools::file_path_sans_ext(basename(filename))
  hits <- unique(regmatches(name, gregexpr("(?<!\\d)20\\d{2}(?!\\d)", name, perl = TRUE))[[1]])
  if (length(hits) == 1) return(hits)
  
  message("Could not find exactly one year in the file name: ", basename(filename))
  repeat {
    answer <- ask_text("Year (e.g. 2026):")
    if (grepl("^20\\d{2}$", answer)) return(answer)
    message("Please type a four-digit year, e.g. 2026")
  }
}

# choose the Excel sheet
choose_sheet <- function(path) {
  sheets <- excel_sheets(path)
  if (length(sheets) == 1) return(sheets)
  
  matches <- grep(sheet_pattern, sheets, ignore.case = TRUE, value = TRUE)
  if (length(matches) == 1) {
    message("Using sheet: ", matches)
    return(matches)
  }
  
  if (length(matches) > 1) {
    message("More than one sheet matches '", sheet_pattern, "'.")
    options <- matches
  } else {
    message("No sheet name contains '", sheet_pattern, "'.")
    options <- sheets
  }
  if (!interactive()) stop("Cannot choose a sheet: R is not running interactively", call. = FALSE)
  choice <- utils::menu(options, title = "Choose the sheet to convert:")
  if (choice == 0) stop("No sheet chosen", call. = FALSE)
  options[choice]
}

# read a csv/txt file as text; guess delimiter and encoding
read_delimited <- function(path) {
  first_lines <- readLines(path, n = 5, warn = FALSE, encoding = "unknown")
  counts <- c("\t" = sum(lengths(regmatches(first_lines, gregexpr("\t", first_lines)))),
              ";"  = sum(lengths(regmatches(first_lines, gregexpr(";",  first_lines)))),
              ","  = sum(lengths(regmatches(first_lines, gregexpr(",",  first_lines)))))
  sep <- names(counts)[which.max(counts)]
  
  all_lines <- readLines(path, warn = FALSE, encoding = "unknown")
  enc <- if (all(validUTF8(all_lines))) "UTF-8-BOM" else "latin1"
  
  message("Reading as delimited text (separator: ",
          switch(sep, "\t" = "tab", ";" = "semicolon", "," = "comma"),
          ", encoding: ", enc, ")")
  read.delim(path, sep = sep, header = TRUE, colClasses = "character",
             na.strings = c("NA", ""), check.names = FALSE, strip.white = TRUE,
             fileEncoding = enc)
}

# light cleaning of the table
clean_data <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  
  # remove tabs and line breaks inside cells, trim spaces, empty text -> NA
  df[] <- lapply(df, function(x) {
    x <- trimws(gsub("[\t\r\n]+", " ", as.character(x)))
    x[!is.na(x) & x == ""] <- NA
    x
  })
  
  # remove completely empty rows
  df <- df[rowSums(!is.na(df)) > 0, , drop = FALSE]
  
  # remove completely empty columns that have no real name
  unnamed <- is.na(names(df)) | names(df) == "" | grepl("^\\.\\.\\.[0-9]+$", names(df)) |
    grepl("^X(\\.[0-9]+)?$", names(df))
  df <- df[, !(unnamed & colSums(!is.na(df)) == 0), drop = FALSE]
  df
}

# make dates dd.mm.yyyy (the format script 01 expects)
fix_dates <- function(df) {
  if (!"date" %in% names(df)) return(df)
  x <- df$date
  
  # Excel date numbers, e.g. 46188
  serial <- !is.na(x) & grepl("^[0-9]{5}$", x)
  if (any(serial)) {
    x[serial] <- format(as.Date(as.numeric(x[serial]), origin = "1899-12-30"), "%d.%m.%Y")
    message(sum(serial), " Excel date number(s) converted to dd.mm.yyyy")
  }
  
  # ISO dates, e.g. 2026-07-15 (with or without a time)
  iso <- !is.na(x) & grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", x)
  if (any(iso)) {
    x[iso] <- format(as.Date(substr(x[iso], 1, 10), "%Y-%m-%d"), "%d.%m.%Y")
    message(sum(iso), " date(s) in yyyy-mm-dd converted to dd.mm.yyyy")
  }
  
  bad <- !is.na(x) & !grepl("^[0-9]{2}\\.[0-9]{2}\\.[0-9]{4}$", x)
  if (any(bad)) {
    warning(sum(bad), " date(s) are not in dd.mm.yyyy format, e.g.: ",
            paste(head(unique(x[bad]), 5), collapse = ", "),
            ". Check them before running script 01.", call. = FALSE)
  }
  df$date <- x
  df
}

# compare column names with the expected ones
check_columns <- function(df) {
  if (identical(names(df), expected_titles)) {
    message("Column names match the expected names.")
    return(invisible(TRUE))
  }
  missing <- setdiff(expected_titles, names(df))
  extra   <- setdiff(names(df), expected_titles)
  if (length(missing)) warning("Missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  if (length(extra))   warning("Unexpected columns: ", paste(extra, collapse = ", "), call. = FALSE)
  if (!length(missing) && !length(extra))
    warning("Column names are correct but the order differs from what script 01 expects.", call. = FALSE)
  invisible(FALSE)
}

# convert one file
convert_raw_file <- function(path) {
  message("\n--- ", basename(path))
  if (!file.exists(path)) stop("File not found: ", path, call. = FALSE)
  
  site <- get_site(path)
  year <- get_year(path)
  
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("xlsx", "xls", "xlsm")) {
    sheet <- choose_sheet(path)
    df <- read_excel(path, sheet = sheet, col_types = "text")
  } else if (ext %in% c("csv", "txt", "tsv")) {
    df <- read_delimited(path)
  } else {
    stop("Unsupported file type '.", ext, "'. Use csv, xlsx, xls or txt.", call. = FALSE)
  }
  
  df <- clean_data(df)
  df <- fix_dates(df)
  check_columns(df)
  
  if (!dir.exists(out.dir)) dir.create(out.dir, recursive = TRUE)
  out_file <- file.path(out.dir, paste0("snowbed_shrubs_", site, "_", year, ".txt"))
  
  if (file.exists(out_file)) {
    if (tolower(ask_text(paste0(basename(out_file), " already exists. Overwrite? (y/n):"))) != "y")
      stop("Not overwritten: ", out_file, call. = FALSE)
  }
  
  write.table(df, out_file, sep = "\t", quote = FALSE, row.names = FALSE,
              na = "", fileEncoding = "UTF-8")
  message("Written: ", out_file, " (", nrow(df), " rows, ", ncol(df), " columns)")
  invisible(out_file)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
################# Run  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

if (length(input_files) == 0) input_files <- file.choose()

for (f in input_files) {
  tryCatch(convert_raw_file(f),
           error = function(e) message("FAILED: ", conditionMessage(e)))
}

# Next: open the new .txt file(s) in a text editor, check them, and then run
# 01_edit_raw_data_shrubs_in_snowbeds_COAT.R