# ==========================================================================
# RUN PHASE 4 (Corrected & Hardened)
# ==========================================================================
run_phase4 <- function(phase3_results, sim_env, ...) {
  
  # Capture any structural arguments passed down by the loop 
  # (e.g., kernel_local, kernel_long, network_matrix)
  dots <- list(...)
  
  if (length(dots) > 0) {
    message("[DEBUG] Phase 4 processing summary metrics for structural configuration: ", 
            paste(names(dots), "=", as.character(dots), collapse = " | "))
  }
  
  times <- seq(2018, 2025) 
  
  results <- lapply(phase3_results, function(p_res) {
    # 1. Apply observational noise / misclassification mapping
    p_res$X_obs <- apply_misclassification(p_res$X_true)
    
    # 2. Compute spatial summary metrics (Prevalence, Moran's I, Ripley's K, Velocity)
    X_final  <- p_res$X_obs
    coords   <- sim_env$coords
    
    prev     <- mean(as.numeric(unlist(X_final)), na.rm = TRUE)
    moran    <- compute_moran_I(X_final, coords)
    ripley   <- compute_ripley_K(X_final, coords)
    velocity <- compute_wavefront_velocity(p_res$history, coords, times)
    
    # 3. DIRECT EXTRACT: Harvest the metrics Phase 3 already computed
    attr_vals <- c(
      prop_local = as.numeric(p_res$metrics$prop_local),
      prop_long  = as.numeric(p_res$metrics$prop_long),
      prop_net   = as.numeric(p_res$metrics$prop_net)
    )
    
    # 4. Construct the balanced return structure for Phase 5
    #    We append the structural metadata so it is preserved downstream.
    return(list(
      abc_vector = c(
        prevalence = as.numeric(prev),
        moran_I    = as.numeric(moran),
        ripley_K   = as.numeric(ripley),
        velocity   = as.numeric(velocity)
      ),
      attribution = attr_vals,
      # Safely pack the model tags into the list if they exist
      metadata = list(
        kernel_local   = if (!is.null(dots$kernel_local)) dots$kernel_local else NA,
        kernel_long    = if (!is.null(dots$kernel_long)) dots$kernel_long else NA,
        network_matrix = if (!is.null(dots$network_matrix)) "[Matrix Provided]" else "[Null]"
      )
    ))
  })
  
  return(results)
}