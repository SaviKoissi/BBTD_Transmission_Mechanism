# ==========================================================
# IBM SIMULATION ENGINE
# ==========================================================

run_simulation <- function(sim_env, params,
                           kernel_local_type = "exponential",
                           kernel_long_type = "powerlaw",
                           t_start = 2018,  # Aligned with Akpro-Missérété origin
                           t_end = 2025,
                           dt = 1) {
  
  # 1. Initialize State
  X <- sim_env$X0
  dist_mat <- sim_env$distance_matrix
  A <- sim_env$adjacency_matrix
  
  # 2. Setup Kernels (Pre-calculated once per particle for HPC speed)
  K_local_fun <- select_kernel(kernel_local_type)
  K_long_fun  <- select_kernel(kernel_long_type)
  
  # Build Local Kernel (sigma based)
  K_local <- compute_kernel_matrix(
    dist_mat, 
    K_local_fun, 
    list(sigma = params$sigma)
  )
  
  # Build Long-Distance Kernel (kappa/p based)
  # This is the driver for the 37.8 km/yr jumps
  K_long <- compute_kernel_matrix(
    dist_mat, 
    K_long_fun, 
    list(kappa = params$kappa, p = params$p)
  )
  
  # 3. Stochastic Time Loop
  # We use seq_len or a controlled loop to avoid floating point errors in seq()
  years <- seq(t_start, t_end - dt, by = dt)
  
  # for (year in years) {
  #   # Calculate pressure from all 3 pathways (w1, w2, w3)
  #   lambda <- compute_lambda(X, K_local, K_long, A, params)
  #   
  #   # Probabilistic update
  #   X <- update_state(X, lambda, dt)
  # }
  # Inside your run_simulation function:
  for (t in 1: years) {
    # 1. Generate the breakdown
    l_obj <- compute_lambda(current_X, K_local, K_long, A, params)
    
    # 2. Update state and catch the attribution metadata
    res <- update_state(current_X, l_obj, dt = 1)
    
    # 3. Update the state for the next tick
    current_X <- res$new_state
    
    # 4. (Optional) Log the res$pathway_counts to track prevalence over time!
  }
  
  return(X) # Final simulated map for 2025
}