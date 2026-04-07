# ==========================================================
# SINGLE PARTICLE RUN
# ==========================================================

run_particle <- function(sim_env, particle, kernel_local_type, kernel_long_type) {
  if (is.list(particle) && length(particle) == 1) particle <- particle[[1]]
  
  results <- run_simulation(sim_env, particle, kernel_local_type, kernel_long_type)
  
  X_obs_sim <- apply_misclassification(results$X_true)
  
  # Calculate Metrics
  log <- results$pathway_log
  total_inf <- sum(log)
  if(total_inf == 0) total_inf <- 1 # Prevent division by zero
  
  metrics <- list(
    prop_local = sum(log$local) / total_inf,
    prop_long  = sum(log$long) / total_inf,
    prop_net   = sum(log$net) / total_inf,
    kernel_winner = kernel_local_type
  )
  
  return(list(X_true = results$X_true, X_obs = X_obs_sim, metrics = metrics))
}

# run_particle <- function(sim_env, particle,
#                          kernel_local_type,
#                          kernel_long_type) {
#   
#   # Rigor Check: Ensure flat list
#   if (is.list(particle) && length(particle) == 1 && is.list(particle[[1]])) {
#     particle <- particle[[1]]
#   }
#   
#   # --- ENHANCEMENT: Step A (Simulation with Logging) ---
#   # We assume run_simulation now returns a list containing:
#   # 1. The final state (X_sim)
#   # 2. The history of contributions (pathway_log)
#   results <- run_simulation(
#     sim_env = sim_env,
#     params = particle,
#     kernel_local_type = kernel_local_type,
#     kernel_long_type = kernel_long_type
#   )
#   
#   X_sim <- results$X_true
#   pathway_log <- results$pathway_log # This holds the local/long/net totals
#   
#   # Step B: Bias injection
#   X_obs_sim <- apply_misclassification(X_sim)
#   
#   # --- ENHANCEMENT: Step C (Calculate Relative Prevalance) ---
#   # Summarize which pathway was the "Winner" for THIS specific particle
#   total_inf <- sum(pathway_log$local + pathway_log$long + pathway_log$net)
#   
#   contribution_summary <- list(
#     prop_local = sum(pathway_log$local) / total_inf,
#     prop_long  = sum(pathway_log$long) / total_inf,
#     prop_net   = sum(pathway_log$net) / total_inf,
#     kernel_winner = ifelse(kernel_local_type == "gaussian", "Gaussian", "Exponential"),
#     long_tail_type = case_when(
#       particle$p < 2 ~ "Levy",
#       particle$p == 2 ~ "Cauchy",
#       TRUE ~ "PowerLaw"
#     )
#   )
#   
#   return(list(
#     X_true = X_sim,
#     X_obs = X_obs_sim,
#     metrics = contribution_summary # Crucial for ranking the patterns
#   ))
# }