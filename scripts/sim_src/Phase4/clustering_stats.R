# ==========================================================
# CLUSTERING METRICS
# ==========================================================

# compute_moran_I <- function(X, coords, k = 4) {
#   
#   # remove NA if any
#   valid <- !is.na(X)
#   X <- X[valid]
#   coords <- coords[valid, ]
#   
#   nb <- spdep::knn2nb(spdep::knearneigh(coords, k = k))
#   listw <- spdep::nb2listw(nb, style = "W")
#   
#   test <- spdep::moran.test(X, listw)
#   
#   return(as.numeric(test$estimate["Moran I statistic"]))
# }
# 
# compute_ripley_K <- function(X, coords) {
#   
#   infected_idx <- which(X == 1)
#   
#   if (length(infected_idx) < 5) return(NA)
#   
#   pts <- coords[infected_idx, ]
#   
#   win <- spatstat.geom::owin(
#     xrange = range(coords[,1]),
#     yrange = range(coords[,2])
#   )
#   
#   pp <- spatstat.geom::ppp(
#     x = pts[,1],
#     y = pts[,2],
#     window = win
#   )
#   
#   K <- spatstat.explore::Kest(pp)
#   
#   # summarize K (e.g., mean deviation from CSR)
#   return(mean(K$iso, na.rm = TRUE))
# }
compute_moran_I <- function(X, coords, k = 8) {
  # Standardize types
  X_vec <- as.numeric(unlist(X))
  valid <- !is.na(X_vec)
  
  X_clean <- X_vec[valid]
  coords_clean <- coords[valid, ]
  
  # Zero variance check (if all sites are healthy or all are infected)
  if(sd(X_clean) == 0) return(0)
  
  # k-nearest neighbors (k=8 bridges the clusters in your geometry)
  nb <- spdep::knn2nb(spdep::knearneigh(coords_clean, k = k))
  listw <- spdep::nb2listw(nb, style = "W", zero.policy = TRUE)
  
  # Calculate Moran's I
  test <- spdep::moran.test(X_clean, listw, zero.policy = TRUE, randomisation = FALSE)
  
  return(as.numeric(test$estimate["Moran I statistic"]))
}

compute_ripley_K <- function(X, coords) {
  X_vec <- as.numeric(unlist(X))
  infected_idx <- which(X_vec == 1)
  
  # Ripley's K requires a minimum point density to be meaningful
  if (length(infected_idx) < 10) return(0)
  
  pts <- coords[infected_idx, ]
  win <- spatstat.geom::owin(range(coords[,1]), range(coords[,2]))
  pp  <- spatstat.geom::ppp(x = pts[,1], y = pts[,2], window = win)
  
  # Evaluate K at a specific radius (e.g., 10km) or take the mean
  # This helps differentiate between local spread vs jumps
  K_obs <- spatstat.explore::Kest(pp)
  
  # Return the mean deviation from CSR (Complete Spatial Randomness)
  return(mean(K_obs$iso - K_obs$theo, na.rm = TRUE))
}