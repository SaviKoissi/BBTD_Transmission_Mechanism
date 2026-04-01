source("R/libraries.R")
library(yaml)

config <- yaml::read_yaml("config/config.yml")

data_cplt <-read_xlsx("/Users/koissi/Desktop/WAVE_project/Rapid_wave/BBTD/BBTD_transformed_06_2025_v2_complet.xlsx", sheet = "Plant_Dim")

# and metadata 
meta<- read_xlsx("/Users/koissi/Desktop/WAVE_project/Rapid_wave/BBTD/BBTD_transformed_06_2025_v2_complet.xlsx", sheet = "Field_Dim")

cplt <- data_cplt %>%
  left_join(meta, by = "Unique_Field_ID")


# Data  cleaning

data <- cplt %>%
  mutate(
    `Total Sucker Number` = as.numeric(`Total Sucker Number`),  # make sure it's numeric
    `Total Sucker Number` = if_else(
      is.na(`Total Sucker Number`),
      as.numeric(str_extract(Calculation, "(?<=-)[0-9]+(?=-[AS])")),
      `Total Sucker Number`
    )
  )%>%
  # Now rename as before
  rename(
    PlantNumber = `Plant Number`,
    TotalNsucker = "Total Sucker Number",
    SurveyedS = `Surveyed Sucker`,
    Disease_state = "Disease State",
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
    Distance  = `Distance traveled since the last field surveyed (km)`,
    NumberCountedField = `Number of Musa fields counted in the survey, after the last survey conducted`,
    Intercrops = `Intercrops within the field`,
    CropsSurrounding = `Crops around the field`,
    Management = `Managed or Unmanaged`,
    TypeFarming = `Production type`, 
    Latitude  = `_Automatic GPS Coordinates_latitude`,
    Longitude = `_Automatic GPS Coordinates_longitude`, 
    Altitude  = `_Automatic GPS Coordinates_altitude`, 
    CoordPrecision = `_Automatic GPS Coordinates_precision`
  ) %>%
  
  # Drop unwanted variables (including Calculation)
  dplyr::select(-c(
    "Calculation", "Picture", "start", "end", "Country", "YEAR OF COLLECTION", 
    "Field ID", "Surveyor Name", "Data Collector's Initials", "Enter a date and time", 
    "Country.1", "Manual coordinates/Latitude/Latitude hemisphere", 
    "Manual coordinates/Latitude/Degrees - latitude", 
    "Manual coordinates/Longitude/Longitude hemisphere", 
    "Manual coordinates/Longitude/Degrees - longitude", 
    "Manual coordinates/Longitude/Altitude(m)", 
    `Sample Collected`, `Unique Plant ID`,`Automatic GPS Coordinates`
  )) %>% 
  mutate( TotalAphid = Aphid0wEL + AphidEL + 
            AphidB + Aphid0wB + AphidN + Aphid0wN
  ) %>%
  mutate(
    # Step 1: Clean categorical scores (extract only digits)
    across(c(Severity, CMV, BSV, BXW, Sigatoka, `Fusarium Wilt`, Management, Weediness),
           ~ str_extract(.x, "\\d+")),
    
    # Step 2: Recoding variables for consistency
    Symptoms = fct_recode(Symptoms,
                          "No Symptoms" = " No symptoms",
                          "No Symptoms" = "No symptoms Bunching",
                          "No Symptoms" = "No symptoms Bunching Dot-dash symptoms on leaves Dot-dash symptoms on petioles or pseudostem",
                          "No Symptoms" = "No symptoms Dot-dash symptoms on petioles or pseudostem"
    ),
    
    Disease_state = fct_recode(Disease_state,
                               "0" = "Asymptomatic",
                               "0" = "Asymptomatic = A",
                               "1" = "Sympomatic",
                               "1" = "Symptomatic",
                               "1" = "Symptomatic = S"
    ),
    
    Intercrops = fct_recode(Intercrops,
                            "1" = "Yes",
                            "1" = "Manioc",
                            "0" = "No",
                            "0" = "Non"
    ), 
    Variety =fct_recode(Variety, 
                        "Banana plantain" = "banana plantain", 
                        "Sweet Banana" = "sweet banana"
                        
    )
  ) %>%
  # Step 3: Convert cleaned numeric strings into numeric
  mutate(across(c(Severity, CMV, BSV, BXW, Sigatoka, `Fusarium Wilt`,
                  Management, Weediness), as.numeric)) %>%
  
  # Step 4: Harmonize crop names
  mutate(CropsSurrounding = str_replace_all(CropsSurrounding,
                                            c("Manihot esculenta" = "Manioc",
                                              "Maïs" = "Maize",
                                              "Mais" = "Maize", 
                                              "papayer" = "Papayer", 
                                              "palmier" = "Palmier "))) %>%
  
  # Step 5: Split multi-crop entries into separate rows
  mutate(CropsSurrounding = str_split(CropsSurrounding, pattern = ",\\s*|\\set\\s*")) %>%
  unnest(CropsSurrounding) %>%
  mutate(CropsSurrounding = str_trim(CropsSurrounding)) %>%
  
  # Step 6: One-hot encode crops with CS_ prefix
  mutate(value = 1) %>%
  pivot_wider(
    names_from = CropsSurrounding,
    values_from = value,
    values_fill = list(value = 0),
    names_prefix = "CS_"
  ) %>%
  
  # Step 7: Drop irrelevant dummy variables (noise categories)
  dplyr::select(-matches("CS_(NA|Non|N|No)$"))%>%
  mutate(
    TypeFarming= if_else(
      Other == "Plantation/commercial Farm (intercrop)", 
      "Plantation/commercial Farm (intercrop)", 
      TypeFarming
    ),
    Other = if_else(
      Other == "Plantation/commercial Farm (intercrop)", 
      NA_character_, 
      Other
    )
  ) %>% 
  dplyr::select(-c(Other, Aphid0wEL, AphidEL, 
                   AphidB, Aphid0wB, AphidN, Aphid0wN)) %>%
  mutate(
    # safe numeric conversion (handles factors and characters)
    Disease_state = as.numeric(as.character(Disease_state)),
    Severity      = as.numeric(as.character(Severity)),
    
    # normalize symptoms text for reliable comparison
    Symptoms_clean = str_to_lower(str_trim(as.character(Symptoms))),
    
    # define evidence of disease: symptom not "no symptoms" OR severity not 1 (but only if severity is present)
    evidence_disease = ( !is.na(Symptoms_clean) & Symptoms_clean != "no symptoms" ) |
      ( !is.na(Severity) & Severity != 1 ),
    
    # final decision rule
    Disease_state = case_when(
      Disease_state == 1 ~ 1,                                         # keep confirmed 1s
      (is.na(Disease_state) | Disease_state == 0) & evidence_disease ~ 1,  # upgrade to 1 when there is evidence
      TRUE ~ 0                                                         # otherwise mark 0
    )
  ) %>%
  dplyr::select(-Symptoms_clean, -evidence_disease) %>%
  mutate(
    # If Symptoms missing but Disease_state is 0, impute "No symptoms"
    Symptoms = if_else(
      is.na(Symptoms) & Disease_state == 0,
      "No symptoms",
      Symptoms
    ))%>% 
  mutate(
    Severity = if_else(
      is.na(Severity) & Disease_state == 0,
      1,
      Severity
    )
  )%>%
  mutate(
    SurveyedS = if_else(
      is.na(SurveyedS) & !is.na(TotalNsucker),
      TotalNsucker,
      SurveyedS
    )
  ) %>% 
  mutate(
    Disease_state = as.factor(Disease_state), 
    Severity = as.factor(Severity),
    Intercrops = as.factor(Intercrops),
    Management = as.factor( Management),
    Weediness = as.factor(Weediness),
    AgroEco = as.factor(AgroEco), 
    CS_Papayer = as.factor(CS_Papayer),
    CS_Palmier = as.factor(CS_Palmier),
    CS_manguier = as.factor(CS_manguier),
    CS_Amarante = as.factor(CS_Amarante),
    CS_Eucalyptus = as.factor(CS_Eucalyptus),
    CS_cocotiers = as.factor(CS_cocotiers),
    CS_Citronnelle = as.factor( CS_Citronnelle),
    CS_Ananas = as.factor(CS_Ananas),
    CS_Manioc = as.factor(CS_Manioc),
    CS_Vernonia = as.factor(CS_Vernonia),
    CS_basilicum = as.factor(CS_basilicum),
    CS_Maize = as.factor(CS_Maize),
    `CS_Canne à sucre` = as.factor(`CS_Canne à sucre`),
    CS_Basilic = as.factor(CS_Basilic),
    CS_vernonia = as.factor(CS_vernonia),
    `CS_grande morelle` = as.factor(`CS_grande morelle`),
    CS_Assissrè = as.factor(CS_Assissrè),
    CS_Igname = as.factor(CS_Igname),
    CS_Gombo = as.factor(CS_Gombo),
    CS_arachide = as.factor(CS_arachide),
    CS_haricot = as.factor(CS_haricot),
    CMV = as.factor(CMV),
    BSV = as.factor(BSV),
    BXW = as.factor(BXW),
    Sigatoka = as.factor(Sigatoka),
    `Fusarium Wilt` = as.factor(`Fusarium Wilt`)
  )


# Convert into geodata 
data_sf <- data%>%
  filter(!is.na(Longitude), !is.na(Latitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)

data <- cplt  # final cleaned object