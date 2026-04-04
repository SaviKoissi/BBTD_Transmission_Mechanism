# ==========================================================
# OBSERVED METRIC
# ==========================================================


# scripts/phase1/observed_metrics.R

compute_observed_metrics <- function(data_2025, coords, 
                                     prev_col = "true_prev_median") { # Use Stan result!
  
  # 1. Align data with coords
  farm_ids <- rownames(coords)
  farm_level <- data_2025 %>%
    dplyr::filter(Unique_Field_ID %in% farm_ids) %>%
    dplyr::arrange(match(Unique_Field_ID, farm_ids))
  
  prev_vector <- farm_level[[prev_col]]
  
  # 2. Moran's I (Global Clustering)
  nb <- spdep::knn2nb(spdep::knearneigh(coords, k = 6))
  listw <- spdep::nb2listw(nb, style = "W")
  moran <- spdep::moran.test(prev_vector, listw)
  
  # 3. Ripley's K-Function (Scale-dependent Clustering)
  # Convert to a point pattern object for spatstat
  # We treat 'high prevalence' farms as the points for the K-function
  # or use the weighted version if your version of spatstat supports it.
  window <- owin(range(coords[,1]), range(coords[,2]))
  ppp_obj <- ppp(coords[,1], coords[,2], window = window, 
                 marks = as.numeric(prev_vector > 0.05)) # Threshold for "Infected"
  k_val <- Kest(ppp_obj[marks(ppp_obj) == 1])
  
  # 4. Final Output Object
  S_obs <- list(
    regional_prevalence = mean(prev_vector, na.rm = TRUE),
    moran_i = moran$estimate[["Moran I statistic"]],
    ripley_k = k_val$iso, # The Ripley's K curve
    epidemic_velocity = 37.8 # Your fixed target from literature
  )
  
  return(S_obs)
}