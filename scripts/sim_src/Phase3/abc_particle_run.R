# ==========================================================
# SINGLE PARTICLE RUN
# ==========================================================

# run_particle <- function(sim_env, particle, kernel_local_type, kernel_long_type) {
#   if (is.list(particle) && length(particle) == 1) particle <- particle[[1]]
#   
#   results <- run_simulation(sim_env, particle, kernel_local_type, kernel_long_type)
#   
#   X_obs_sim <- apply_misclassification(results$X_true)
#   
#   # Calculate Metrics
#   log <- results$pathway_log
#   total_inf <- sum(log)
#   if(total_inf == 0) total_inf <- 1 # Prevent division by zero
#   
#   metrics <- list(
#     prop_local = sum(log$local) / total_inf,
#     prop_long  = sum(log$long) / total_inf,
#     prop_net   = sum(log$net) / total_inf,
#     kernel_winner = kernel_local_type
#   )
#   
#   return(list(X_true = results$X_true, X_obs = X_obs_sim, metrics = metrics))
# }

run_particle <- function(sim_env, particle, kernel_local_type, kernel_long_type) {
  if (is.list(particle) && length(particle) == 1) particle <- particle[[1]]
  
  results <- run_simulation(sim_env, particle, kernel_local_type, kernel_long_type)
  X_obs_sim <- apply_misclassification(results$X_true)
  
  log <- results$pathway_log
  total_inf <- sum(log)
  if(total_inf == 0) total_inf <- 1
  
  metrics <- list(
    prop_local = sum(log$local) / total_inf,
    prop_long  = sum(log$long) / total_inf,
    prop_net   = sum(log$net) / total_inf,
    kernel_winner = kernel_local_type
  )
  
  # CRITICAL FIX: Pass results$history up to Phase 4
  return(list(X_true = results$X_true, history = results$history, X_obs = X_obs_sim, metrics = metrics))
}
