# ==========================================================
# SPATIAL EMBEDDING 
# ==========================================================

# scripts/phase1/spatial_embedding.R


create_spatial_embedding <- function(data,
                                     id_col = "Unique_Field_ID",
                                     lon_col = "Longitude",
                                     lat_col = "Latitude") {
  
  # Drop geometry if sf object
  if ("sf" %in% class(data)) {
    data <- sf::st_drop_geometry(data)
  }
  
  spatial_df <- data %>%
    dplyr::select(
      farm_id = all_of(id_col),
      longitude = all_of(lon_col),
      latitude = all_of(lat_col)
    ) %>%
    dplyr::distinct()
  
  # Convert to pure numeric matrix (CRITICAL)
  coords <- as.matrix(spatial_df[, c("longitude", "latitude")])
  
  # Ensure numeric
  storage.mode(coords) <- "double"
  
  # Assign rownames safely
  rownames(coords) <- spatial_df$farm_id
  
  return(list(
    spatial_df = spatial_df,
    coords = coords
  ))
}