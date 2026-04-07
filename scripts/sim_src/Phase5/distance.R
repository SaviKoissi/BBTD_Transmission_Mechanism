# ==========================================================
# DISTANCE FUNCTION
# ==========================================================

# compute_distance <- function(S_sim, S_obs) {
#   # Strip all names and force to pure numbers
#   s1 <- as.numeric(unlist(S_sim))
#   s2 <- as.numeric(unlist(S_obs))
#   
#   # Ensure they are the same length
#   if (length(s1) != length(s2)) {
#     return(Inf) # Cannot compare apples to oranges
#   }
#   
#   # Remove NAs and calculate Scaled Euclidean
#   valid <- is.finite(s1) & is.finite(s2)
#   if (sum(valid) == 0) return(Inf)
#   
#   # The "Science" standard scaling
#   # (Sim - Obs) / Obs
#   diff <- (s1[valid] - s2[valid]) / s2[valid]
#   return(sqrt(sum(diff^2)))
# }

compute_distance <- function(S_sim, S_obs) {
  # 1. Summarize Obs Ripley's K if it's a long vector
  obs_val <- c(
    prevalence = as.numeric(S_obs$regional_prevalence),
    moran_i    = as.numeric(S_obs$moran_i),
    ripley_k   = mean(as.numeric(S_obs$ripley_k), na.rm = TRUE), # Summarize to 1 value
    velocity   = as.numeric(S_obs$epidemic_velocity)
  )
  
  # 2. Ensure Sim is a simple numeric vector in the same order
  # Order: prevalence, moran, ripley, velocity
  sim_val <- as.numeric(S_sim)
  
  # 3. Scaled Euclidean Distance
  valid <- is.finite(sim_val) & is.finite(obs_val)
  diff_scaled <- ((sim_val[valid] - obs_val[valid]) / obs_val[valid])^2
  
  return(sqrt(sum(diff_scaled)))
}