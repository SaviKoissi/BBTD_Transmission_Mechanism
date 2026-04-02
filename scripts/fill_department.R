# ==========================================================
# CHECK FOR MISSING DEPARTMENT NAME
# ==========================================================

config <- yaml::read_yaml("config/config.yml") 
benin_shp <- sf::st_read(config$paths$input_shpf)

fill_missing_department <- function(data, shapefile, dept_col) {
  
  if (!dept_col %in% names(shapefile)) {
    stop(paste("Column", dept_col, "not found in shapefile"))
  }
  
  data_sf <- data %>%
    dplyr::filter(!is.na(Longitude), !is.na(Latitude)) %>%
    sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)
  
  shapefile <- sf::st_make_valid(shapefile)
  shapefile <- sf::st_transform(shapefile, sf::st_crs(data_sf))
  
  joined <- sf::st_join(data_sf, shapefile, left = TRUE)
  
  # Safe replacement
  joined$Departement <- dplyr::coalesce(
    as.character(joined$Departement),
    as.character(joined[[dept_col]])
  )
  
  # Nearest fallback
  missing_idx <- which(is.na(joined$Departement))
  
  if (length(missing_idx) > 0) {
    nearest_idx <- sf::st_nearest_feature(joined[missing_idx, ], shapefile)
    joined$Departement[missing_idx] <- shapefile[[dept_col]][nearest_idx]
  }
  
  return(joined)
}