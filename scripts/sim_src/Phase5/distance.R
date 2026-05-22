# ==========================================================
# DISTANCE FUNCTION
# ==========================================================

#scripts/sim_src/Phase5/distance.R
compute_distance <- function(S_sim, S_obs) {
  
  # 1. Force explicit vector formatting with clean names
  sim_val <- S_sim
  
  # 2. Extract observed values safely by their exact named keys
  obs_prev   <- as.numeric(S_obs$regional_prevalence)
  obs_moran  <- as.numeric(S_obs$moran_i)
  obs_ripley <- mean(as.numeric(S_obs$ripley_k), na.rm = TRUE)
  obs_vel    <- as.numeric(S_obs$epidemic_velocity)
  
  # 3. Handle a zero-summary Ripley's K guard
  if (is.na(obs_ripley) || obs_ripley == 0) obs_ripley <- 1
  
  # 4. Compute independent squared differences matching key-by-key
  # This avoids any hidden vector order bugs entirely
  d_prev  <- (as.numeric(sim_val["prevalence"]) - obs_prev)^2
  d_moran <- (as.numeric(sim_val["moran_I"])   - obs_moran)^2
  d_vel   <- (as.numeric(sim_val["velocity"])  - obs_vel)^2
  
  # Scaled by a standard absolute value if obs is zero to prevent division by zero
  denom_vel <- if (obs_vel == 0) 1 else obs_vel
  denom_prev <- if (obs_prev == 0) 1 else obs_prev
  
  # 5. Build a robust Relative Error Distance Framework
  diff_prev   <- ((as.numeric(sim_val["prevalence"]) - obs_prev) / denom_prev)^2
  diff_moran  <- ((as.numeric(sim_val["moran_I"]) - obs_moran) / (obs_moran + 0.001))^2
  diff_ripley <- ((as.numeric(sim_val["ripley_K"]) - obs_ripley) / obs_ripley)^2
  diff_vel    <- ((as.numeric(sim_val["velocity"]) - obs_vel) / denom_vel)^2
  
  total_components <- c(diff_prev, diff_moran, diff_ripley, diff_vel)
  
  # Clean out any stray NaNs or Infs caused by edge cases
  valid_components <- total_components[is.finite(total_components)]
  
  if (length(valid_components) == 0) {
    return(999999) # Return a penalty distance if all calculations failed
  }
  
  return(sqrt(sum(valid_components)))
}



# compute_distance <- function(S_sim, S_obs) {
#   # 1. Summarize Obs Ripley's K if it's a long vector
#   obs_val <- c(
#     prevalence = as.numeric(S_obs$regional_prevalence),
#     moran_i    = as.numeric(S_obs$moran_i),
#     ripley_k   = mean(as.numeric(S_obs$ripley_k), na.rm = TRUE), # Summarize to 1 value
#     velocity   = as.numeric(S_obs$epidemic_velocity)
#   )
#   
#   # 2. Ensure Sim is a simple numeric vector in the same order
#   # Order: prevalence, moran, ripley, velocity
#   sim_val <- as.numeric(S_sim)
#   
#   # 3. Scaled Euclidean Distance
#   valid <- is.finite(sim_val) & is.finite(obs_val)
#   diff_scaled <- ((sim_val[valid] - obs_val[valid]) / obs_val[valid])^2
#   
#   return(sqrt(sum(diff_scaled)))
# }