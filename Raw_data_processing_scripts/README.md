# Raw data processing scripts

Scripts for processing raw field data for the COAT small rodent module and for checking the files uploaded to the COAT data portal. See the [main README](../README.md) for the overall repository and the yearly routine.

## Scripts

| Script | Purpose |
|---|---|
| `01_edit_raw_data_shrubs_in_snowbeds_COAT.R` | Yearly workflow: checks raw files, reformats and writes the portal file |
| `correct_annual_files.R` | One-off fixes to past raw data files already prepared for the portal |
| `download_portal_files_for_comparison.R` | Downloads selected files from the COAT data portal |
| `compare_portal_and_box_files.R` | Compares downloaded portal files with the corrected files in Box |

The first two scripts are described in the main README. The rest of this file covers the portal check.

## Portal check: is the corrected file on the portal?

**When to use:** Some files were corrected after preparation (see `correct_annual_files.R`), and it is unclear whether the portal holds the old or the corrected version.

This workflow only downloads and compares. It never uploads, modifies or overwrites anything on the portal. The files being compared are not stored in this repository.

### 1. API token (once per person)

Downloading private files needs a personal API token.

1. Log in to [data.coat.no](https://data.coat.no).
2. Select your name in the top-right corner.
3. Open **Manage → API tokens** and create a token.
4. In R, run `usethis::edit_r_environ()`, add the line below, save, and restart R/RStudio:

```text
api_COAT=YOUR_COAT_API_TOKEN
```

The scripts read it with `Sys.getenv("api_COAT")`. Never write the token in a script, commit it, or share it!

### 2. Run the check

Open the project via `small_rodent_module.Rproj` so that the relative folder paths in the scripts work.

1. Put the corrected files from Box in `corrected files to data portal/`.
2. In `download_portal_files_for_comparison.R`, set `years` and run it. Files are saved to `portal files for comparison/`.
3. In `compare_portal_and_box_files.R`, set `years` to match and run it.
4. Read the result in `comparison results/portal_box_comparison.csv`.

Rows are sorted before comparing, so a different row order alone is not a mismatch.

### 3. Reading the result

Example of a successful result:

```text
filename,status,portal_rows,box_rows,same_columns,same_data
V_snowbed_shrub_abundance_intensive_2025.txt,match,108,108,TRUE,TRUE
```

| `status` | Meaning |
|---|---|
| `match` | Portal file is identical to the corrected file |
| `different` | Columns or data differ; the portal probably holds a different version |
| `missing from portal download` | File was not found in the download |
| `missing from Box/corrected files` | No corrected file in `corrected files to data portal/` |

If a file is `different`, record it in a GitHub Issue before uploading anything, then follow the normal upload and metadata update steps.

### 4. Local folders

`portal files for comparison/`, `corrected files to data portal/` and `comparison results/` are local working folders. Keep them out of Git (see `.gitignore`), together with `.Renviron`.

Example `.gitignore` entries:

```gitignore
# COAT API credentials
.Renviron

# Local/private data and comparison files
portal files for comparison/
corrected files to data portal/
comparison results/
```

### Results log

| Date checked | Years | Result | Issue |
|---|---|---|---|
| 2026-10-22 | 2011, 2022, 2024 | All files matched corrected Box files for portal version 2; no differences found | None |
| 2026-10-22 | 2011, 2022, 2024, 2025 | All files matched corrected Box files for portal version 3; no differences found | None |

### Credits

The download functions are sourced from
<https://github.com/COATnor/data_management_scripts/blob/master/download_data_from_coat_data_portal.R>,
originally written by Hanna Böhner.
The portal download and comparison workflow in this repository was written
by Suaiba Amina Salahuddin.
