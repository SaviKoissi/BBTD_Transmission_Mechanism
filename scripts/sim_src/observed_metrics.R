# ==========================================================
# OBSERVED METRIC
# ==========================================================


# scripts/phase1/observed_metrics.R


compute_observed_metrics <- function(data_2025, coords,
                                     prev_col = "prevalence",
                                     id_col = "Unique_Field_ID") {
  
  # ----------------------------
  # 1. Aggregate to farm level
  # ----------------------------
  farm_level <- data_2025 %>%
    dplyr::group_by(.data[[id_col]]) %>%
    dplyr::summarise(
      prevalence = mean(.data[[prev_col]], na.rm = TRUE),
      .groups = "drop"
    )
  
  # ----------------------------
  # 2. Align with coords
  # ----------------------------
  farm_ids <- rownames(coords)
  
  farm_level <- farm_level %>%
    dplyr::filter(.data[[id_col]] %in% farm_ids)
  
  farm_level <- farm_level[match(farm_ids, farm_level[[id_col]]), ]
  
  prev_vector <- farm_level$prevalence
  
  # ----------------------------
  # 3. Remove NA safely
  # ----------------------------
  valid_idx <- which(!is.na(prev_vector))
  
  prev_vector <- prev_vector[valid_idx]
  coords_mat <- coords[valid_idx, , drop = FALSE]
  
  # ----------------------------
  # 4. Spatial weights
  # ----------------------------
  nb <- spdep::knn2nb(spdep::knearneigh(coords_mat, k = 6))
  listw <- spdep::nb2listw(nb, style = "W")
  
  # ----------------------------
  # 5. Moran’s I
  # ----------------------------
  moran <- spdep::moran.test(prev_vector, listw)
  
  # ----------------------------
  # 6. Output
  # ----------------------------
  S_obs <- list(
    regional_prevalence = mean(prev_vector, na.rm = TRUE),
    moran_i = moran$estimate[["Moran I statistic"]],
    epidemic_velocity = 37.8
  )
  
  return(S_obs)
}