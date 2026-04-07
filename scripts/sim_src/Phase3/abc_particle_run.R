# ==========================================================
# SINGLE PARTICLE RUN
# ==========================================================

# run_particle <- function(sim_env, particle,
#                          kernel_local_type,
#                          kernel_long_type) {
#   
#   # Step A: simulate
#   X_sim <- run_simulation(
#     sim_env,
#     params = particle,
#     kernel_local_type = kernel_local_type,
#     kernel_long_type = kernel_long_type
#   )
#   
#   # Step B: bias injection
#   X_obs_sim <- apply_misclassification(X_sim)
#   
#   return(list(
#     X_true = X_sim,
#     X_obs = X_obs_sim
#   ))
# }

# run_particle <- function(sim_env, particle,
#                          kernel_local_type,
#                          kernel_long_type) {
#   
#   # DEBUG/RIGOR CHECK: Ensure particle is a flat list
#   # Sometimes in parallel loops, 'particle' comes in as a nested list [[1]]
#   if (is.list(particle) && length(particle) == 1 && is.list(particle[[1]])) {
#     particle <- particle[[1]]
#   }
#   
#   # Step A: simulate
#   X_sim <- run_simulation(
#     sim_env = sim_env,
#     params = particle, # Now we are sure this is the list containing 'sigma'
#     kernel_local_type = kernel_local_type,
#     kernel_long_type = kernel_long_type
#   )
#   
#   # Step B: bias injection
#   X_obs_sim <- apply_misclassification(X_sim)
#   
#   return(list(
#     X_true = X_sim,
#     X_obs = X_obs_sim
#   ))
# }

run_particle <- function(sim_env, particle,
                         kernel_local_type,
                         kernel_long_type) {
  
  # Rigor Check: Ensure flat list
  if (is.list(particle) && length(particle) == 1 && is.list(particle[[1]])) {
    particle <- particle[[1]]
  }
  
  # --- ENHANCEMENT: Step A (Simulation with Logging) ---
  # We assume run_simulation now returns a list containing:
  # 1. The final state (X_sim)
  # 2. The history of contributions (pathway_log)
  results <- run_simulation(
    sim_env = sim_env,
    params = particle,
    kernel_local_type = kernel_local_type,
    kernel_long_type = kernel_long_type
  )
  
  X_sim <- results$X_true
  pathway_log <- results$pathway_log # This holds the local/long/net totals
  
  # Step B: Bias injection
  X_obs_sim <- apply_misclassification(X_sim)
  
  # --- ENHANCEMENT: Step C (Calculate Relative Prevalance) ---
  # Summarize which pathway was the "Winner" for THIS specific particle
  total_inf <- sum(pathway_log$local + pathway_log$long + pathway_log$net)
  
  contribution_summary <- list(
    prop_local = sum(pathway_log$local) / total_inf,
    prop_long  = sum(pathway_log$long) / total_inf,
    prop_net   = sum(pathway_log$net) / total_inf,
    kernel_winner = ifelse(kernel_local_type == "gaussian", "Gaussian", "Exponential"),
    long_tail_type = case_when(
      particle$p < 2 ~ "Levy",
      particle$p == 2 ~ "Cauchy",
      TRUE ~ "PowerLaw"
    )
  )
  
  return(list(
    X_true = X_sim,
    X_obs = X_obs_sim,
    metrics = contribution_summary # Crucial for ranking the patterns
  ))
}