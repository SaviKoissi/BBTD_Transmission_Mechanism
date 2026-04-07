# ==========================================================
# RUN PHASE 5 (FINAL)
# ==========================================================

# run_phase5 <- function(phase4_results, 
#                        phase2_obj, 
#                        S_obs, 
#                        prop = 0.05, 
#                        model_results = NULL) {
#   
#   message("--- Starting Phase 5: Posterior Inference ---")
#   
#   # 1. DISTANCE COMPUTATION
#   distances <- sapply(phase4_results, function(res) {
#     # Extract the vector safely
#     if (is.list(res) && "abc_vector" %in% names(res)) {
#       s_sim <- res$abc_vector
#     } else {
#       s_sim <- res
#     }
#     # Pass ONLY the vector to the distance function
#     compute_distance(s_sim, S_obs)
#   })
#   
#   # 2. SELECTION
#   epsilon_star <- quantile(distances, prop, na.rm = TRUE)
#   # accepted_idx <- which(distances <= epsilon_star)
#   # 
#   # if (length(accepted_idx) == 0) stop("Selection failed.")
#   
#   # Inside run_phase5
#   accepted_idx <- which(distances <= epsilon_star)
#   
#   if (length(accepted_idx) == 0) {
#     stop("ABC Failure: No particles matched the criteria. Check if simulations are spreading.")
#   }
#   
#   # Ensure extract_posterior produces a dataframe even if only 1 particle is accepted
#   posterior_df <- extract_posterior(accepted_idx, phase2_obj, phase4_results)
#   
#   if (nrow(posterior_df) == 0) {
#     stop("Extraction Error: Accepted particles found, but parameters could not be linked.")
#   }
#   
#   # 3. POSTERIOR & SUMMARY
#   # Ensure extract_posterior uses the index to get parameters
#   posterior_df  <- extract_posterior(accepted_idx, phase2_obj, phase4_results)
#   summary_table <- compute_summary_table(posterior_df)
#   dominance     <- identify_dominant_mechanism(posterior_df)
#   
#   # 4. VISUALS
#   plot_posterior_weights(posterior_df)
#   tern_plot <- plot_ternary(posterior_df)
#   
#   # 5. MODEL SELECTION (Question 1 & 2)
#   if (!is.null(model_results)) {
#     model_comp <- compare_models(model_results, S_obs, epsilon_star)
#   } else {
#     model_comp <- NULL
#   }
#   
#   return(list(
#     posterior = posterior_df,
#     summary = summary_table,
#     dominance = dominance,
#     model_comparison = model_comp,
#     epsilon = epsilon_star
#   ))
# }

run_phase5 <- function(phase4_results, phase2_obj, S_obs, prop = 0.05, model_results = NULL) {
  
  message("--- Starting Phase 5: Posterior Inference ---")
  
  # 1. Compute Distances
  # distances <- sapply(phase4_results, function(res) {
  #   # Check if simulation actually spread (Velocity > 0)
  #   if (is.list(res) && res$abc_vector["velocity"] == 0) return(Inf)
  #   
  #   s_sim <- if(is.list(res)) res$abc_vector else res
  #   compute_distance(s_sim, S_obs)
  # })
  
  # Inside run_phase5.R
  distances <- sapply(phase4_results, function(res) {
    s_sim <- res$abc_vector
    # If velocity is 0, give it a massive penalty so it's never accepted
    if (s_sim["velocity"] == 0) return(999999) 
    compute_distance(s_sim, S_obs)
  })
  
  # Diagnostic: How many simulations actually moved?
  moved <- sum(distances < 999999)
  message(paste("Simulations with movement:", moved, "/", length(distances)))
  
  if (moved == 0) stop("All simulations have 0 velocity. Increase w3 (Network) in Phase 2.")
  
  # 2. Safety Check: Are there any valid distances?
  valid_count <- sum(is.finite(distances))
  if (valid_count == 0) {
    stop("CRITICAL: All simulations returned Distance = Inf. Check Phase 4 Velocity logic.")
  }
  
  # 3. Dynamic Selection
  epsilon_star <- quantile(distances, prop, na.rm = TRUE)
  accepted_idx <- which(distances <= epsilon_star)
  
  # 4. Posterior Extraction with Row Guard
  posterior_df <- extract_posterior(accepted_idx, phase2_obj, phase4_results)
  
  if (nrow(posterior_df) == 0) {
    stop("Selection failed: No particles met the threshold.")
  }
  
  # 5. Summary & Analysis
  summary_table <- compute_summary_table(posterior_df)
  dominance <- identify_dominant_mechanism(posterior_df)
  
  return(list(
    posterior = posterior_df,
    summary = summary_table,
    dominance = dominance,
    epsilon = epsilon_star
  ))
}