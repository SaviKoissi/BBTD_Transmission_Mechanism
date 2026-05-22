# ==========================================================
# IBM SIMULATION ENGINE
# ==========================================================

run_simulation <- function(sim_env, params, 
                           kernel_local_type = "exponential", 
                           kernel_long_type = "powerlaw", 
                           t_start = 2018, 
                           t_end = 2025, dt = 1) {
  X <- sim_env$X0
  K_local <- compute_kernel_matrix(sim_env$distance_matrix, 
                                   select_kernel(kernel_local_type), 
                                   list(sigma = params$sigma))
  K_long  <- compute_kernel_matrix(sim_env$distance_matrix, 
                                   select_kernel(kernel_long_type), 
                                   list(kappa = params$kappa, p = params$p))
  
  years <- seq(t_start, t_end, by = dt)
  
  # Keep track of counts using a clean, named numeric vector instead of a dataframe
  pathway_log <- c(local = 0, long = 0, net = 0)
  
  history_list <- vector("list", length(years))
  history_list[[1]] <- X 
  
  for (i in 1:(length(years) - 1)) {
    l_obj <- compute_lambda(X, K_local, K_long, 
                            sim_env$adjacency_matrix, params)
    res <- update_state(X, l_obj, dt = dt)
    X <- res$new_state
    
    # Correctly increment the named numeric vector elements
    pathway_log["local"] <- pathway_log["local"] + res$pathway_counts[["local"]]
    pathway_log["long"]  <- pathway_log["long"]  + res$pathway_counts[["long"]]
    pathway_log["net"]   <- pathway_log["net"]   + res$pathway_counts[["net"]]
    
    history_list[[i + 1]] <- X
  }
  
  # FIX: Stripped explicit names to prevent the "unused arguments" runtime crash
  X_obs <- apply_misclassification(X, 0.78, 0.92)
  
  return(list(
    X_true      = X, 
    X_obs       = X_obs, 
    history     = history_list, 
    pathway_log = as.data.frame(t(pathway_log)) # Format as a clean row frame for output
  ))
}