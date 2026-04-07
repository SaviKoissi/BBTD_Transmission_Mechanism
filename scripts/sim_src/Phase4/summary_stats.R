# ==========================================================
# SUMMARY STATISTICS VECTOR
# ==========================================================

# compute_summary_stats <- function(sim_output, sim_env, times) {
#   
#   X_final <- sim_output$X_obs
#   
#   coords <- sim_env$coords
#   
#   # --- Prevalence
#   prev <- compute_prevalence(X_final)
#   
#   # --- Clustering
#   moran <- compute_moran_I(X_final, coords)
#   ripley <- compute_ripley_K(X_final, coords)
#   
#   # --- Wavefront (needs history)
#   velocity <- compute_wavefront_velocity(
#     sim_output$history,
#     coords,
#     times
#   )
#   
#   return(c(
#     prevalence = prev,
#     moran_I = moran,
#     ripley_K = ripley,
#     velocity = velocity
#   ))
# }
compute_summary_stats <- function(sim_output, sim_env, times) {
  # 1. Setup
  X_final <- sim_output$X_obs
  coords  <- sim_env$coords
  
  # 2. Spatial Metrics (The ABC Fingerprint)
  prev     <- mean(as.numeric(unlist(X_final)), na.rm = TRUE)
  moran    <- compute_moran_I(X_final, coords)
  ripley   <- compute_ripley_K(X_final, coords)
  velocity <- compute_wavefront_velocity(sim_output$history, coords, times)
  
  # 3. Pathway Attribution (The Research Answers)
  # Extracts the relative influence of w1, w2, w3 recorded in Phase 3
  log <- sim_output$pathway_log
  total_inf <- sum(log$local + log$long + log$net)
  if(total_inf <= 0) total_inf <- 1 # Protection
  
  # 4. Balanced Return
  return(list(
    # Use this for Phase 5 Euclidean distance
    abc_vector = c(
      prevalence = prev,
      moran_I = moran,
      ripley_K = ripley,
      velocity = velocity
    ),
    # Use this for Question 3 & 4
    attribution = c(
      prop_local = sum(log$local) / total_inf,
      prop_long  = sum(log$long) / total_inf,
      prop_net   = sum(log$net) / total_inf
    )
  ))
}