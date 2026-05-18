# ==========================================================
# RUN PHASE 4
# ==========================================================

# run_phase4 <- function(phase2_obj,
#                        kernel_local_type = "gaussian",
#                        kernel_long_type = "powerlaw",
#                        parallel = FALSE) {
#   
#   sim_env <- phase2_obj$simulation_env
#   particles <- phase2_obj$particles
#   
#   times <- seq(2012, 2025, by = 1)
#   
#   if (parallel) {
#     library(future.apply)
#     
#     results <- future_lapply(particles, function(p) {
#       run_particle(sim_env, p,
#                    kernel_local_type,
#                    kernel_long_type,
#                    times)
#     })
#     
#   } else {
#     
#     results <- lapply(particles, function(p) {
#       run_particle(sim_env, p,
#                    kernel_local_type,
#                    kernel_long_type,
#                    times)
#     })
#   }
#   
#   return(results)
# }
# ==========================================================
# REVISED RUN_PHASE4 (The Post-Processor)
# ==========================================================
# run_phase4 <- function(phase3_results, sim_env) {
#   
#   # 1. Define the timeline based on your Akpro-Missérété start (2018-2025)
#   # This must match the length of your history_list
#   times <- seq(2018, 2025) 
#   
#   # 2. Process each particle's results
#   results <- lapply(phase3_results, function(p_res) {
#     
#     # A. Apply misclassification to the final simulated state
#     p_res$X_obs <- apply_misclassification(p_res$X_true)
#     
#     # B. Compute the summary statistics (Velocity, Moran's I, Attribution)
#     # This calls the enhanced compute_summary_stats from the previous turn
#     stats <- compute_summary_stats(p_res, sim_env, times)
#     
#     return(stats)
#   })
#   
#   return(results)
# }

run_phase4 <- function(phase3_results, sim_env) {
  times <- seq(2018, 2025) 
  
  results <- lapply(phase3_results, function(p_res) {
    p_res$X_obs <- apply_misclassification(p_res$X_true)
    
    # CRITICAL ALIGNMENT: 
    # If compute_summary_stats relies on a custom wrapper, make sure it feeds
    # p_res$history directly into your compute_wavefront_velocity function!
    stats <- compute_summary_stats(p_res, sim_env, times)
    
    return(stats)
  })
  
  return(results)
}