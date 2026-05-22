# ==========================================================
# SUMMARY STATISTICS VECTOR
# ==========================================================

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
  # FIX: Coerce the dataframe cells into pure numeric primitives immediately
  raw_local <- as.numeric(unlist(sim_output$pathway_log[["local"]]))
  raw_long  <- as.numeric(unlist(sim_output$pathway_log[["long"]]))
  raw_net   <- as.numeric(unlist(sim_output$pathway_log[["net"]]))
  
  # Ensure we have single scalars to work with
  sum_local <- sum(raw_local, na.rm = TRUE)
  sum_long  <- sum(raw_long, na.rm = TRUE)
  sum_net   <- sum(raw_net, na.rm = TRUE)
  
  total_inf <- sum_local + sum_long + sum_net
  if(is.na(total_inf) || total_inf <= 0) total_inf <- 1 # Protection
  
  # 4. Balanced Return
  return(list(
    # Use this for Phase 5 Euclidean distance
    abc_vector = c(
      prevalence = as.numeric(prev),
      moran_I    = as.numeric(moran),
      ripley_K   = as.numeric(ripley),
      velocity   = as.numeric(velocity)
    ),
    # Use this for Question 3 & 4
    # FIX: Creating a clean, unlisted primitive numeric vector
    attribution = c(
      prop_local = sum_local / total_inf,
      prop_long  = sum_long / total_inf,
      prop_net   = sum_net / total_inf
    )
  ))
}