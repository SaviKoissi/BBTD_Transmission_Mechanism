# ==============================================================================
# RENAME TO MASTER ENGINE: RUN PHASE 3 DYNAMIC TOURNAMENT
# ==============================================================================

run_phase3 <- function(phase2_obj, parallel = FALSE, ...) {
  
  sim_env   <- phase2_obj$simulation_env
  particles <- phase2_obj$particles
  
  # Available biophysical structures for tournament selection
  local_pool <- c("gaussian", "exponential")
  wind_pool  <- c("powerlaw", "cauchy", "levy")
  
  # --- MAP INDEPENDENT MECHANISMS TO PARTICLES ---
  set.seed(42) # Ensure reproducible assignment across tasks
  for(i in seq_along(particles)) {
    particles[[i]]$assigned_local <- sample(local_pool, 1)
    particles[[i]]$assigned_wind  <- sample(wind_pool, 1)
  }
  
  # --- EXECUTION LAYER WITH HIGHER ORDER EVALUATION ---
  if (parallel) {
    results <- future.apply::future_lapply(particles, function(p) {
      k_local <- p$assigned_local
      k_long  <- p$assigned_wind
      
      # The '...' cleanly swallows incoming arguments like network_matrix
      res <- run_particle(sim_env, p, k_local, k_long)
      
      # Explicitly append structural labels to the output metrics
      res$metrics$local_winner <- k_local
      res$metrics$wind_winner  <- k_long
      return(res)
    }, future.seed = TRUE) 
    
  } else {
    results <- lapply(particles, function(p) {
      k_local <- p$assigned_local
      k_long  <- p$assigned_wind
      
      res <- run_particle(sim_env, p, k_local, k_long)
      
      res$metrics$local_winner <- k_local
      res$metrics$wind_winner  <- k_long
      return(res)
    })
  }
  
  return(results)
}