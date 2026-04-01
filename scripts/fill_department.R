# ==========================================================
# CHECK FOR MISSING DEPARTMENT NAME
# ==========================================================

config <- yaml::read_yaml("config/config.yml")

benin_shp <- sf::st_read(config$paths$input_shpf)

fill_missing_department <- function(data, shapefile) {
  
  # Ensure sf format
  data_sf <- data %>%
    filter(!is.na(Longitude), !is.na(Latitude)) %>%
    st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)
  
  # Ensure same CRS
  shapefile <- st_transform(shapefile, st_crs(data_sf))
  
  # Spatial join
  joined <- st_join(data_sf, shapefile, left = TRUE)
  
  # Replace missing Departement
  data_filled <- joined %>%
    mutate(
      Departement = if_else(
        is.na(Departement),
        as.character(adm1_name),   # <-- adapt if needed
        as.character(Departement)
      )
    )
  
  return(data_filled)
}