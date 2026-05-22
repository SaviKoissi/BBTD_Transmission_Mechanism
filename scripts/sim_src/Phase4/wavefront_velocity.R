# ==========================================================
# WAVEFRONT VELOCITY
# ==========================================================

compute_wavefront_velocity <- function(history_list, coords, times) {
  
  # 1. FIX THE ORIGIN (Θ) based on the paper's baseline
  # The paper defines the origin as the 2018 focus. 
  # We extract the coordinates of the cells infected at time step 1.
  first_step_infected <- which(as.numeric(unlist(history_list[[1]])) == 1)
  
  if (length(first_step_infected) == 0) return(0) # Safety: no seeding
  
  # Calculate the centroid of the initial 2018 focus as origin (Theta)
  origin_coords <- colMeans(matrix(coords[first_step_infected, ], ncol = 2))
  
  # 2. TRACK MEAN BOUNDARY DISTANCE (D_t) per the paper's formula
  mean_dists <- sapply(history_list, function(X) {
    inf_idx <- which(as.numeric(unlist(X)) == 1)
    
    # If no infections, distance from origin is 0
    if (length(inf_idx) == 0) return(0)
    
    # Calculate distance from origin to ALL infected points at time t
    dists <- sqrt((coords[inf_idx, 1] - origin_coords[1])^2 + 
                    (coords[inf_idx, 2] - origin_coords[2])^2)
    
    # PAPER ALIGNMENT: Return the MEAN distance (D_t), not the MAX distance
    return(mean(dists))
  })
  
  # 3. REGRESSION ANALYSIS (D_t = beta_0 + beta_1 * t)
  d <- as.numeric(mean_dists)
  t <- as.numeric(times)
  
  # If the mean distance doesn't change across time steps, velocity is 0
  if (length(unique(d)) < 2) return(0)
  
  # Fit the linear model to extract the average rate of spread (beta_1)
  fit <- lm(d ~ t)
  velocity <- as.numeric(coef(fit)[2])
  
  # Ensure we don't pass back a negative slope artifact if things fluctuate
  return(max(0, velocity)) 
}
