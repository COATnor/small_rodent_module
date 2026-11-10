# Small rodent module

This repository contains code and documentation for the **COAT small rodent module**.

The repository is used for:

- preprocessing raw field data
- checking and formatting data before upload to the COAT data portal
- calculating state variables derived from raw data
- documenting workflows and decisions
- logging changes to code and data
- tracking tasks, responsibilities and open issues

The repository will be expanded gradually as additional workflows and folders are documented.

> Raw and processed data are stored in Box or the COAT data portal, not in this repository! Only code, documentation and other version-controlled project files should be committed to Git.

## COAT data portal

Data and state variables from the module are published on the [COAT data portal](https://data.coat.no).

| Type | Name | Link |
|---|---|---|
| Dataset | `V_snowbed_shrub_abundance_intensive` | [data.coat.no/dataset/v_snowbed_shrub_abundance_intensive](https://data.coat.no/dataset/v_snowbed_shrub_abundance_intensive) |
| State variable | `V85_snowbed_dwarf_shrub_abundance_intensive` | [data.coat.no/state-variable/v85_snowbed_dwarf_shrub_abundance_intensive](https://data.coat.no/state-variable/v85_snowbed_dwarf_shrub_abundance_intensive) |

The dataset page holds the raw-derived data files prepared with the scripts in `Raw_data_processing_scripts/`. The state variable page lists all existing versions of the state variable, which are calculated and uploaded with the scripts in `state_variable_scripts/`.

## Repository structure

```text
.
├── Raw_data_processing_scripts/
│   ├── 01_edit_raw_data_shrubs_in_snowbeds_COAT.R
│   ├── correct_annual_files.R
│   ├── download_portal_files_for_comparison.R
│   └── compare_portal_and_box_files.R
├── data_preprocessing_scripts/
├── state_variable_scripts/
├── .gitignore
├── CITATION.cff
├── LICENSE
├── README.md
└── small_rodent_module.Rproj
```

## `Raw_data_processing_scripts/`

This folder contains scripts for processing raw field data. The scripts may include checks, corrections, formatting and preparation of files before they are uploaded to the COAT data portal.

## `data_preprocessing_scripts/`

This folder contains scripts for preprocessing data before further analyses or calculation of state variables.

## `state_variable_scripts/`

This folder contains scripts for calculating state variables and other derived measures for the small rodent module.

The scripts in this folder:

1. download the data they need from the COAT data portal
2. calculate the state variable
3. upload a new version of the state variable to the portal

| Script | State variable | Portal page |
|---|---|---|
| `calculate_state_variable_shrub_abundance_snowbeds.R` | `V85_snowbed_dwarf_shrub_abundance_intensive` | [Existing versions](https://data.coat.no/state-variable/v85_snowbed_dwarf_shrub_abundance_intensive) |

For example, `calculate_state_variable_shrub_abundance_snowbeds.R` downloads the `V_snowbed_shrub_abundance_intensive` dataset, calculates `V85_snowbed_dwarf_shrub_abundance_intensive` and uploads the result as a new version. All earlier versions remain available on the state variable page.

Because each upload creates a new version, run these scripts only after the underlying dataset on the portal has been checked and updated (see the yearly routine below), and document the change in the relevant GitHub Issue.

The documentation of the folders and workflows will be expanded gradually.

## Shrub abundance in snowbeds: raw data preprocessing

The `Raw_data_processing_scripts/` folder contains preprocessing code for the **`V_snowbed_shrub_abundance_intensive`** dataset, which is part of the COAT small rodent module.

The workflow prepares raw field data before it is uploaded to the COAT data portal.

## Scripts

| Script | Purpose |
|---|---|
| `Raw_data_processing_scripts/01_edit_raw_data_shrubs_in_snowbeds_COAT.R` | Yearly workflow. Reads raw field files, runs checks, reformats to COAT variable names, reshapes to long format and writes the portal file |
| `Raw_data_processing_scripts/correct_annual_files.R` | One-off fixes to raw data files that have already been prepared for the portal |
| `Raw_data_processing_scripts/download_portal_files_for_comparison.R` | Downloads selected files from the COAT data portal for comparison |
| `Raw_data_processing_scripts/compare_portal_and_box_files.R` | Compares the downloaded portal files with the corrected files stored in Box |

## `01_edit_raw_data_shrubs_in_snowbeds_COAT.R`
Based on Eeva's 2017 template (`Template snowbed vegetation data.xlsx`). Last updated by Eeva 17.10.2023.

### Raw data format

The script reads the raw field files as **tab-delimited text (`.txt`)**. Please save the raw field data in this format from the start, e.g. `snowbed_shrubs_VJ_2025.txt` and `snowbed_shrubs_KO_2025.txt`.

If the data have been saved as `.csv` or `.xlsx`, **convert them to tab-delimited `.txt` before running the script**!

### Steps to run script

1. Set `data.dir` (raw files) and `out.dir` (files for the portal).
2. Set `filename` to the Vestre Jakobselv (`_VJ_`) file, then run the checks. Repeat for Komagdalen (`_KO_`). **The two localities are checked separately.**
3. Run the combine step after both are checked.
4. Set `year` and write `V_snowbed_shrub_abundance_intensive_<year>.txt` (semicolon-separated).

Checks in the script:

- column names match the expected 17 columns
- dates parse as `dd.mm.yyyy`
- `locality` and `site` are within the allowed lists
- 4 plots per site and 12 subplots per plot (so 108 rows per plot after reshaping: 9 species x 12 subplots)
- `treatment` is `control`
- plant columns contain only 0 or 1
- observer and comment spelling

Species columns: `emp_nig`, `vac_myr`, `vac_uli`, `bet_nan`, `vac_vit`, `phy_cae`, `nar_str`, `hup_sel`, `sal_her`.

## `correct_annual_files.R`

Reads the raw data files already prepared for the portal and writes corrected versions to `corrected files to data portal/`.

| Year | Correction |
|---|---|
| 2011 | Missing dates filled from `dates_2011_filled.xlsx`, joined by `sn_site` and `sn_plot` |
| 2022 | Presence values swapped between `bet_nan` and `vac_vit` in Komagdalen |
| 2024 | Same swap in Komagdalen; wrong `t_year` values (9, 10, 11) set to 2024; dates converted from `dd.mm.yyyy` to `yyyy-mm-dd` |
| 2025 | Wrong `t_year` values (1, 2, 10, 11, 12, 31) set to 2025; dates converted to `yyyy-mm-dd` |

### Checking that corrected files are on the portal

If it is unclear whether the corrected or the old version of a file is on the COAT data portal, the portal file can be downloaded and compared with the corrected file in Box. This is a check only and does not change anything on the portal.

See [`Raw_data_processing_scripts/README.md`](Raw_data_processing_scripts/README.md) for instructions. Results can be logged in GitHub Issues.

## Yearly routine (checklist)

We can use something like this to do a check for new data collected and to be added to the COAT data portal. We can perhaps copy these points into an issue for each new field season and tick them off when addressed. This checklist can be adapted as needed!

- [ ] Raw files for Komagdalen and Vestre Jakobselv received and placed in `raw data from field/`
- [ ] VJ file checked and corrected in script 01
- [ ] KO file checked and corrected in script 01
- [ ] Files combined, reshaped and written for the portal
- [ ] Output checked: column names, 108 rows per plot, date format `yyyy-mm-dd`, `t_year` is a four-digit year
- [ ] File uploaded to the data portal
- [ ] Portal metadata updated (data version, years covered, changes)
- [ ] Changes logged below


## Working in this repository

This repository is also the project log. It tracks:

- changes to code and data
- tasks and who is responsible
- who did what and when
- open issues
- decisions about data processing and corrections

## Issues

Use GitHub Issues for:

- tasks
- data problems
- questions
- requests for review
- new field-season workflows
- corrections to existing data
- metadata updates

## Commits

Write short commit messages that describe what changed. For example:

```text
Fix t_year for 2025 shrub data
```

When relevant, reference the related issue:

```text
Fix t_year for 2025 shrub data (#2)
```

The commit history and GitHub Issues together provide a record of who did what and when.

Before making a data-related change, document the original file name and the reason for the change in the relevant GitHub Issue.

## Open tasks

- [ ] Update portal metadata according to the corrected files
