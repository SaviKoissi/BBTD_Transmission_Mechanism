# ==========================================================
# INITIALIZE SIMULATION ENVIRONMENT
# ==========================================================

initialize_simulation <- function(phase1_obj) {
  
  sim_env <- list(
    
    # Spatial structure
    coords = phase1_obj$coords,
    distance_matrix = phase1_obj$distance_matrix,
    adjacency_matrix = phase1_obj$adjacency_matrix,
    
    # Initial infection state
    X0 = phase1_obj$initial_state,
    
    # Observed targets
    S_obs = phase1_obj$observed_metrics
    
  )
  
  return(sim_env)
}