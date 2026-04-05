# ==========================================================
# DATA CLEANING SCRIPT (OPTIMIZED)
# ==========================================================

source("R/libraries.R")
library(yaml)

config <- yaml::read_yaml("config/config.yml")

# ----------------------------------------------------------
# 1. LOAD DATA (no hardcoding)
# ----------------------------------------------------------
data_cplt <- readxl::read_xlsx(config$paths$input_file, sheet = "Plant_Dim")
meta      <- readxl::read_xlsx(config$paths$input_file, sheet = "Field_Dim")

cplt <- dplyr::left_join(data_cplt, meta, by = config$columns$id)

# ----------------------------------------------------------
# 2. CLEANING PIPELINE
# ----------------------------------------------------------

data <- cplt %>%
  
  # ---- Core numeric fixes ----
mutate(
  `Total Sucker Number` = as.numeric(`Total Sucker Number`),
  `Total Sucker Number` = if_else(
    is.na(`Total Sucker Number`),
    as.numeric(stringr::str_extract(Calculation, "(?<=-)[0-9]+(?=-[AS])")),
    `Total Sucker Number`
  )
) %>%
  
  # ---- Rename once ----
rename(
  PlantNumber = `Plant Number`,
  TotalNsucker = `Total Sucker Number`,
  SurveyedS = `Surveyed Sucker`,
  Disease_state = `Disease State`,
  Severity = `Severity Score`,
  Aphid0wEL = `Aphid Count - Wingless (Emerging Leaf Sheath)`, 
  AphidEL   = `Aphid Count - Winged (Emerging Leaf Sheath)`,
  AphidB    = `Aphid Count - Winged (Base)`,
  Aphid0wB  = `Aphid Count - Wingless (Base)`, 
  AphidN    = `Aphid Count - Winged (Neck)`, 
  Aphid0wN  = `Aphid Count - Wingless (Neck)`,
  Location  = `Location name`, 
  AgroEco   = `Agro-ecology zone`, 
  Fieldsize = `Field size (m2)`,
  PlantationAge = `Musa field Age (Months)`,
  Latitude  = `_Automatic GPS Coordinates_latitude`,
  Longitude = `_Automatic GPS Coordinates_longitude`
) %>%
  
  # ---- Drop noise early (performance gain) ----
dplyr::select(-any_of(c(
  "Calculation","Picture","start","end","Country","YEAR OF COLLECTION",
  "Field ID","Surveyor Name","Data Collector's Initials",
  "Enter a date and time","Country.1",
  "Manual coordinates/Latitude/Latitude hemisphere",
  "Manual coordinates/Latitude/Degrees - latitude",
  "Manual coordinates/Longitude/Longitude hemisphere",
  "Manual coordinates/Longitude/Degrees - longitude",
  "Manual coordinates/Longitude/Altitude(m)",
  "Sample Collected","Unique Plant ID","Automatic GPS Coordinates"
))) %>%
  
  # ---- Vectorized aphid sum ----
mutate(
  TotalAphid = rowSums(across(starts_with("Aphid")), na.rm = TRUE)
) %>%
  
  # ---- Clean categorical numeric fields (single pass) ----


mutate(
  across(
    any_of(c("Severity", "CMV", "BSV", "BXW", "Sigatoka", "Fusarium Wilt", "Management", "Weediness")),
    ~ stringr::str_extract(.x, "\\d+")
  )
) %>%
  
  # ---- Normalize text once ----
mutate(
  Symptoms = stringr::str_squish(stringr::str_to_lower(as.character(Symptoms))),
  Disease_state = as.character(Disease_state)
) %>%
  
  # ---- Disease recoding (vectorized + robust) ----
mutate(
  Disease_state = case_when(
    Disease_state %in% c("Sympomatic", "Symptomatic", "Symptomatic = S") ~ 1,
    Disease_state %in% c("Asymptomatic", "Asymptomatic = A") ~ 0,
    TRUE ~ NA_real_
  )
) %>%
  
  # ---- Evidence-based correction ----
mutate(
  evidence_disease = (!is.na(Symptoms) & Symptoms != "no symptoms") |
    (!is.na(Severity) & Severity != 1),
  
  Disease_state = case_when(
    Disease_state == 1 ~ 1,
    (is.na(Disease_state) | Disease_state == 0) & evidence_disease ~ 1,
    TRUE ~ 0
  )
) %>%
  
  dplyr::select(-evidence_disease) %>%
  
  # ---- Missing value harmonization ----
mutate(
  Symptoms = if_else(is.na(Symptoms) & Disease_state == 0, "no symptoms", Symptoms),
  Severity = as.numeric(as.character(Severity))
  ) %>%
    mutate(
      Severity = if_else(
        is.na(Severity) & Disease_state == 0,
        1,
        Severity
      ),
  SurveyedS = if_else(is.na(SurveyedS) & !is.na(TotalNsucker), TotalNsucker, SurveyedS)
) %>%
  
  # ---- Crops processing (compact + efficient) ----
mutate(
  CropsSurrounding = if ("CropsSurrounding" %in% names(.)) {
    stringr::str_replace_all(
      CropsSurrounding,
      c("Manihot esculenta" = "Manioc",
        "Maïs" = "Maize",
        "Mais" = "Maize",
        "papayer" = "Papayer",
        "palmier" = "Palmier")
    )
  } else {
    NA_character_
  },
  CropsSurrounding = stringr::str_split(CropsSurrounding, ",\\s*|\\set\\s*")
) %>%
  tidyr::unnest(CropsSurrounding) %>%
  mutate(CropsSurrounding = stringr::str_trim(CropsSurrounding)) %>%
  
  mutate(value = 1) %>%
  tidyr::pivot_wider(
    names_from = CropsSurrounding,
    values_from = value,
    values_fill = 0,
    names_prefix = "CS_"
  ) %>%
  
  dplyr::select(-matches("CS_(NA|Non|N|No)$")) %>%
  
  # ---- Efficient factor conversion ----
mutate(
  across(where(is.character), as.factor),
  Disease_state = as.factor(Disease_state)
) %>% 
  # ------ Add year of data collection ---
  mutate(
    year = 2025
  )

# ----------------------------------------------------------
# 3. SPATIAL VERSION (optional)
# ----------------------------------------------------------
data_sf <- data %>%
  filter(!is.na(Longitude), !is.na(Latitude)) %>%
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)

# ----------------------------------------------------------
# 4. RETURN CLEAN DATA
# ----------------------------------------------------------
data
