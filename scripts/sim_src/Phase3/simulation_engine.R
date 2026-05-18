# ==========================================================
# IBM SIMULATION ENGINE
# ==========================================================

# run_simulation <- function(sim_env, params, kernel_local_type = "exponential", 
#                            kernel_long_type = "powerlaw", t_start = 2018, t_end = 2025, dt = 1) {
#   X <- sim_env$X0
#   K_local <- compute_kernel_matrix(sim_env$distance_matrix, select_kernel(kernel_local_type), list(sigma = params$sigma))
#   K_long  <- compute_kernel_matrix(sim_env$distance_matrix, select_kernel(kernel_long_type), list(kappa = params$kappa, p = params$p))
#   
#   years <- seq(t_start, t_end - dt, by = dt)
#   pathway_log <- data.frame(local = 0, long = 0, net = 0)
#   
#   for (i in seq_along(years)) {
#     l_obj <- compute_lambda(X, K_local, K_long, sim_env$adjacency_matrix, params)
#     res <- update_state(X, l_obj, dt = dt)
#     X <- res$new_state
#     pathway_log <- pathway_log + res$pathway_counts
#   }
#   
#   return(list(X_true = X, pathway_log = pathway_log))
# }

run_simulation <- function(sim_env, params, kernel_local_type = "exponential", 
                           kernel_long_type = "powerlaw", t_start = 2018, t_end = 2025, dt = 1) {
  X <- sim_env$X0
  K_local <- compute_kernel_matrix(sim_env$distance_matrix, select_kernel(kernel_local_type), list(sigma = params$sigma))
  K_long  <- compute_kernel_matrix(sim_env$distance_matrix, select_kernel(kernel_long_type), list(kappa = params$kappa, p = params$p))
  
  years <- seq(t_start, t_end, by = dt)
  pathway_log <- data.frame(local = 0, long = 0, net = 0)
  
  # Allocate a history list to preserve the spatial snapshot of each year
  history_list <- vector("list", length(years))
  history_list[[1]] <- X # Store baseline 2018 state
  
  # Run through the tracking years
  for (i in 1:(length(years) - 1)) {
    l_obj <- compute_lambda(X, K_local, K_long, sim_env$adjacency_matrix, params)
    res <- update_state(X, l_obj, dt = dt)
    X <- res$new_state
    pathway_log <- pathway_log + res$pathway_counts
    
    # Save the snapshot to history
    history_list[[i + 1]] <- X
  }
  
  # Return BOTH the final state and the structured time history
  return(list(X_true = X, history = history_list, pathway_log = pathway_log))
}
  