# # ==========================================================
# # WAVEFRONT VELOCITY
# # ==========================================================

compute_wavefront_velocity <- function(history_list, coords, times) {
  # 1. Identify the origin (Akpro-Missérété)
  # history_list[[1]] is the state at t_start (2018)
  origin_idx <- which(as.numeric(unlist(history_list[[1]])) == 1)[1]
  
  # Safety: if simulation failed to seed, return 0 velocity
  if (is.na(origin_idx)) return(0)
  origin_coords <- coords[origin_idx, ]
  
  # 2. Calculate expansion over time
  max_distances <- sapply(history_list, function(X) {
    X_vec <- as.numeric(unlist(X))
    infected_idx <- which(X_vec == 1)
    
    if (length(infected_idx) <= 1) return(0)
    
    # Euclidean distance of all infected points from the 2018 origin
    dists <- sqrt((coords[infected_idx, 1] - origin_coords[1])^2 + 
                    (coords[infected_idx, 2] - origin_coords[2])^2)
    return(max(dists))
  })
  
  # 3. Type-Fix: Force to numeric vector for the linear model
  dist_vec <- as.numeric(unlist(max_distances))
  time_vec <- as.numeric(times)
  
  if (length(unique(dist_vec)) < 2) return(0)
  
  # slope = km/year
  fit <- lm(dist_vec ~ time_vec)
  return(as.numeric(coef(fit)[2])) 
}