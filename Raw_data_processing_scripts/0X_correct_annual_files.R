### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
###  CORRECT SOME ANNUAL DATA FILES FOR V_SNOWBED_SHRUB_ABUNDANCE_INTENSIVE_v7  ###
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###


## Setup

library(dplyr)
library(writexl)
library(readxl)



## Import files that should be corrected

data_2011 <- read.csv("C:/Users/gusyn2253/Box/COAT/Modules/Small rodent module/data/V_snowbed_shrub_abundance_intensive/edited files to data portal/V_snowbed_shrub_abundance_intensive_2011.txt", sep = ";")
data_2022 <- read.csv("C:/Users/gusyn2253/Box/COAT/Modules/Small rodent module/data/V_snowbed_shrub_abundance_intensive/edited files to data portal/V_snowbed_shrub_abundance_intensive_2022.txt", sep = ";")
data_2024 <- read.csv("C:/Users/gusyn2253/Box/COAT/Modules/Small rodent module/data/V_snowbed_shrub_abundance_intensive/edited files to data portal/V_snowbed_shrub_abundance_intensive_2024.txt", sep = ";")
data_2025 <- read.csv("C:/Users/gusyn2253/Box/COAT/Modules/Small rodent module/data/V_snowbed_shrub_abundance_intensive/edited files to data portal/V_snowbed_shrub_abundance_intensive_2025.txt", sep = ";")



## Functions

swap_presence_komagdalen <- function(df) {
  
  # Identify problematic groups
  bad_groups <- df %>%
    filter(
      sn_locality == "komagdalen",
      v_species %in% c("bet_nan", "vac_vit")
    ) %>%
    group_by(sn_site, sn_plot, sn_subplot) %>%
    summarise(
      has_both_species = all(c("bet_nan", "vac_vit") %in% v_species),
      .groups = "drop") %>%
    filter(!has_both_species)
  
  # Stop if any group is invalid
  if (nrow(bad_groups) > 0) {
    stop(paste0("Swap stopped: ", nrow(bad_groups)," group(s) in locality",
                "'komagdalen' do not contain both 'bet_nan' and 'vac_vit'."),
         call. = FALSE
    )
  }
  
  # Perform the swap if no bad groups
  df <- df %>%
    group_by(sn_locality, sn_site, sn_plot, sn_subplot) %>%
    mutate(
      v_presence = case_when(
        sn_locality == "komagdalen" & v_species == "bet_nan" ~
          v_presence[v_species == "vac_vit"],
        sn_locality == "komagdalen" & v_species == "vac_vit" ~
          v_presence[v_species == "bet_nan"],
        TRUE ~ v_presence)) %>%
    ungroup()
  
  return(df)
}


## 2011
lapply(data_2011, unique)

# export unique combinations of sn_site and sn_plot
dates_missing_2011 <- data_2011 %>%
  distinct(sn_site,sn_plot)
#write_xlsx(dates_missing_2011, path = "C:/Users/gusyn2253/OneDrive - UiT Office 365/COAT Guro/small_rodent/snowbed_shrub_abundance/dates_2011.xlsx")

# read in correct dates
dates_2011 <- read_xlsx(path = "C:/Users/gusyn2253/OneDrive - UiT Office 365/COAT Guro/small_rodent/snowbed_shrub_abundance/dates_2011_filled.xlsx")
dates_2011 <- dates_2011 %>%
  mutate(
    sn_plot = as.integer(sn_plot),
    t_date = as.character(t_date))
str(dates_2011)

# add correct dates to data file based on sn_site and sn_plot
data_2011_new <- data_2011 %>%
  left_join(
    dates_2011 %>% select(sn_site, sn_plot, t_date),
    by = c("sn_site", "sn_plot"),
    suffix = c("", "_new")) %>%
  mutate(t_date = t_date_new) %>%
  select(-t_date_new)

str(data_2011_new)
lapply(data_2011_new, unique)


## 2022
lapply(data_2022, unique)
data_2022_new <- swap_presence_komagdalen(data_2022)
lapply(data_2022_new, unique)


## 2024
str(data_2024)
lapply(data_2024, unique)

data_2024_new <- swap_presence_komagdalen(data_2024) %>%
  mutate(
    t_year = as.integer(t_year), 
    t_year = case_when(
      t_year %in% c(9, 10, 11) ~ as.integer(2024),
      TRUE ~ t_year),
    t_date = if_else(
      grepl("^\\d{2}\\.\\d{2}\\.\\d{4}$", t_date),
      format(as.Date(t_date, format = "%d.%m.%Y"), "%Y-%m-%d"),
      t_date))

lapply(data_2024_new, unique)
str(data_2024_new)



## 2025
lapply(data_2025, unique)

data_2025_new <- data_2025 %>%
  mutate(
    t_year = case_when(
      t_year %in% c(1, 2, 10, 11, 12, 31) ~ 2025,
      TRUE ~ t_year),
    t_date = if_else(
      grepl("^\\d{2}\\.\\d{2}\\.\\d{4}$", t_date),
      format(as.Date(t_date, format = "%d.%m.%Y"), "%Y-%m-%d"),
      t_date))

lapply(data_2025_new, unique)



## Save files to "corrected directory"

out_dir <- "C:/Users/gusyn2253/Box/COAT/Modules/Small rodent module/data/V_snowbed_shrub_abundance_intensive/corrected files to data portal" # path to output folder

corrected_files <- list(
  V_snowbed_shrub_abundance_intensive_2011 = data_2011_new,
  V_snowbed_shrub_abundance_intensive_2022 = data_2022_new,
  V_snowbed_shrub_abundance_intensive_2024 = data_2024_new,
  V_snowbed_shrub_abundance_intensive_2025 = data_2025_new)


#for (f in names(corrected_files)) {
write.table(
  corrected_files[[f]],
  file = file.path(out_dir, paste0(f, ".txt")),
  sep = ";",
  row.names = FALSE,
  quote = FALSE
)
message("File written: ", f)
}

