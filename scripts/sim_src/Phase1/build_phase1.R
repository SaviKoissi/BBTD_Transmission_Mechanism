# ==========================================================
# BUILD PHASE 1
# ==========================================================

build_phase1 <- function(data_full) {
  
  spatial <- create_spatial_embedding(data_full)
  coords <- spatial$coords
  farm_ids <- rownames(coords)
  
  D <- compute_distance_matrix(coords)
  
  A <- create_adjacency_matrix(farm_ids)
  
  X0 <- create_initial_state(data_full, year = 2018)
  
  
  data_2025 <- data_full %>%
    dplyr::filter(year == 2025)
  
  coord <- create_spatial_embedding(data_2025)$coords
  
  S_obs <- compute_observed_metrics(data_2025, coord)
  
  return(list(
    coords = coords,
    distance_matrix = D,
    adjacency_matrix = A,
    initial_state = X0,
    observed_metrics = S_obs
  ))
}