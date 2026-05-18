# ==========================================================
# RUN PHASE 3
# ==========================================================

run_phase3 <- function(phase2_obj, 
                       kernel_local_type = "exponential", 
                       kernel_long_type = "powerlaw", 
                       parallel = FALSE) {
  
  sim_env <- phase2_obj$simulation_env
  particles <- phase2_obj$particles
  
  if (parallel) {
    
    # ENHANCEMENT: future.seed = TRUE ensures parallel-safe random numbers
    # for rbinom() calls inside the simulation.
    results <- future.apply::future_lapply(particles, function(p) {
      run_particle(sim_env, p, kernel_local_type, kernel_long_type)
    }, future.seed = TRUE) 
    
  } else {
    results <- lapply(particles, function(p) {
      run_particle(sim_env, p, kernel_local_type, kernel_long_type)
    })
  }
  
  return(results)
}
