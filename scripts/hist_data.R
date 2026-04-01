
# ==========================================================
# ADD HISTORIC DATA
# ==========================================================

source("scripts/compute_prevalence.R")

config <- yaml::read_yaml("config/config.yml")

data_hist <- readr::read_csv(config$paths$input_hist)

# ==========================================================
# CLEAN HIST DATA
# ==========================================================

data_hist <- data_hist %>%
  rename(
    Unique_Field_ID = id,
    Latitude = lat,
    Longitude = lon,
    Departement = NAME_1,
    Commune = NAME_2,
    Disease_state = BBTD_inc
  ) %>% 
  select(-c(NAME_3, country_name, adm0_name,
            country_iso3, crop, date_created_unixtimestamp , 
            pest, source,
            date_created, date_accessed, date_created_unixtimestamp)) %>% 
  relocate(Disease_state, .after = year)

# ==========================================================
# PREVALENCE
# ==========================================================

prevalence_per_farm_hist <- compute_prevalence(data_hist, config)

write.csv(prevalence_per_farm_hist, config$paths$output_prevalence_hist, row.names = FALSE)
