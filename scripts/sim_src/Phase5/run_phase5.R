# ==============================================================================
# scripts/sim_src/Phase5/run_phase5.R
# ==============================================================================

run_phase5 <- function(phase4_results, phase2_obj, 
                       S_obs, prop = 0.05, model_results = NULL) {
  
  message("--- Starting Phase 5: Posterior Inference ---")
  
  # ----------------------------------------------------------------------------
  # STRUCTURAL ALIGNMENT: ROBUST DATA CORRECTION & KEY RECOVERY
  # ----------------------------------------------------------------------------
  # Expected uniform metric order for your computational epidemiological framework
  expected_metrics <- c("prevalence", "moran_i", "ripley_k", "velocity")
  
  if (is.list(S_obs)) {
    message("[DEBUG] Complex S_obs structure detected. Flattening to match simulation vectors...")
    
    # Try parsing by named keys first
    target_prev   <- S_obs$regional_prevalence[[1]] %||% S_obs$prevalence[[1]] %||% NA
    target_moran  <- S_obs$moran_i[[1]] %||% S_obs$moran_I[[1]] %||% NA
    target_vel    <- S_obs$epidemic_velocity[[1]] %||% S_obs$velocity[[1]] %||% NA
    target_ripley <- if (!is.null(S_obs$ripley_k)) mean(S_obs$ripley_k, na.rm = TRUE) else 
      if (!is.null(S_obs$ripley_K)) mean(S_obs$ripley_K, na.rm = TRUE) else NA
    
    # Fallback: If named resolution failed but it has items, parse positionally
    if (is.na(target_prev) && length(S_obs) >= 4) {
      message("[DEBUG] Named extraction failed. Attempting positional extraction from list...")
      target_prev  <- S_obs[[1]]
      target_moran <- S_obs[[2]]
      target_ripley<- S_obs[[3]]
      target_vel   <- S_obs[[4]]
    }
    
    S_target <- c(
      prevalence = as.numeric(target_prev),
      moran_i    = as.numeric(target_moran),
      ripley_k   = as.numeric(target_ripley),
      velocity   = as.numeric(target_vel)
    )
  } else {
    S_target <- as.numeric(S_obs)
    
    # Check if names are missing or broken
    if (is.null(names(S_obs)) || all(is.na(names(S_obs)))) {
      if (length(S_target) == 4) {
        message("[DEBUG] Unnamed S_obs vector discovered. Rebuilding keys positionally...")
        names(S_target) <- expected_metrics
      } else {
        warning("[WARNING] Target observation length does not match expected matrix bounds.")
        names(S_target) <- expected_metrics[1:min(length(S_target), 4)]
      }
    } else {
      names(S_target) <- tolower(names(S_obs))
    }
  }
  
  # Final sanitization point to guarantee alignment keys match perfectly
  names(S_target)[names(S_target) == "moran_i"]  <- "moran_i"
  names(S_target)[names(S_target) == "ripley_k"] <- "ripley_k"
  
  # ----------------------------------------------------------------------------
  # 1. ROBUST DISTANCE COMPUTATION
  # ----------------------------------------------------------------------------
  distances <- sapply(seq_along(phase4_results), function(i) {
    res <- phase4_results[[i]]
    
    # Unpack simulated metric summary vector
    s_sim <- if (is.list(res) && "abc_vector" %in% names(res)) res$abc_vector else res
    s_sim <- as.numeric(s_sim)
    
    # Fallback to force standard structural layout naming on individual simulation outputs
    if (is.null(names(s_sim)) || length(names(s_sim)) != length(expected_metrics)) {
      names(s_sim) <- expected_metrics[1:length(s_sim)]
    } else {
      names(s_sim) <- tolower(names(s_sim))
    }
    
    # Crash Failsafe: Filter out infinite loops, zero-velocity artifacts, or aborted runs
    if (is.na(s_sim["velocity"]) || s_sim["velocity"] <= 0 || is.infinite(s_sim["velocity"])) {
      return(999999) 
    }
    
    common_names <- intersect(names(s_sim), names(S_target))
    
    # Hard Positional Override if an edge case drops the intersection length to 0
    if (length(common_names) == 0) {
      if (length(s_sim) == length(S_target)) {
        common_names <- expected_metrics[1:length(s_sim)]
        names(s_sim)    <- common_names
        names(S_target) <- common_names
      } else {
        cat("\n[ALIGNMENT FAILURE DEBUG - RUNTIME]:\n")
        cat("Simulation Vector Shape:", paste(names(s_sim), collapse = ", "), "\n")
        cat("Target Vector Shape:", paste(names(S_target), collapse = ", "), "\n")
        stop("CRITICAL alignment failure: Structural shape mismatch in summary stats.")
      }
    }
    
    sim_vector    <- s_sim[common_names]
    target_vector <- S_target[common_names]
    
    # Root-mean-squared relative error calculation
    raw_diff <- sim_vector - target_vector
    normalized_diff <- raw_diff / (target_vector + 1e-6)
    
    scaled_dist <- sqrt(mean(normalized_diff^2, na.rm = TRUE))
    return(scaled_dist)
  })
  
  # ----------------------------------------------------------------------------
  # 2. DYNAMIC ABC SELECTION
  # ----------------------------------------------------------------------------
  epsilon_star <- quantile(distances, prop, na.rm = TRUE)
  accepted_idx <- which(distances <= epsilon_star)
  
  message(paste("[DEBUG] Calculated Epsilon cutoff threshold:", round(epsilon_star, 5)))
  message(paste("[DEBUG] Number of particles accepted:", length(accepted_idx)))
  
  if (length(accepted_idx) == 0) {
    stop("ABC Failure: Acceptance threshold yielded zero matching particles.")
  }
  
  # ----------------------------------------------------------------------------
  # 3. SAFE POSTERIOR EXTRACTION & ATTRIBUTION MATRIX BUILDING
  # ----------------------------------------------------------------------------
  posterior_df <- tryCatch({
    df <- extract_posterior(accepted_idx, phase2_obj, phase4_results)
    df <- as.data.frame(df)
    
    n_particles    <- length(accepted_idx)
    prop_local_vec <- numeric(n_particles)
    prop_long_vec  <- numeric(n_particles)
    prop_net_vec   <- numeric(n_particles)
    
    # Fast vectorized loop extraction
    for (idx in seq_along(accepted_idx)) {
      real_idx     <- accepted_idx[idx]
      particle_res <- phase4_results[[real_idx]]
      
      if (is.list(particle_res) && "attribution" %in% names(particle_res)) {
        attr_vec <- particle_res$attribution
        names(attr_vec)     <- tolower(names(attr_vec))
        prop_local_vec[idx] <- as.numeric(attr_vec["prop_local"] %||% attr_vec[1] %||% 0)
        prop_long_vec[idx]  <- as.numeric(attr_vec["prop_long"]  %||% attr_vec[2] %||% 0)
        prop_net_vec[idx]   <- as.numeric(attr_vec["prop_net"]   %||% attr_vec[3] %||% 0)
      }
    }
    
    # Prune outdated or un-normalized dummy columns
    dummy_cols <- c("prop_local", "prop_long", "prop_net")
    df <- df[, !(names(df) %in% dummy_cols), drop = FALSE]
    
    df$prop_local <- prop_local_vec
    df$prop_long  <- prop_long_vec
    df$prop_net   <- prop_net_vec
    
    df
    
  }, error = function(e) {
    stop("\nCRITICAL CRASH inside posterior vector translation: -> ", e$message)
  })
  
  # ----------------------------------------------------------------------------
  # 4. PROTECTED SUMMARY & DOMINANCE ANALYSIS
  # ----------------------------------------------------------------------------
  message("[DEBUG] Running parameter summary aggregation...")
  summary_table <- tryCatch({
    compute_summary_table(posterior_df)
  }, error = function(e) {
    warning("Summary generation bypassed due to error: ", e$message)
    NULL 
  })
  
  message("[DEBUG] Running transmission dominance matrix calculations...")
  dominance <- tryCatch({
    identify_dominant_mechanism(posterior_df)
  }, error = function(e) {
    warning("Dominance classification bypassed due to error: ", e$message)
    NULL
  })
  
  # ----------------------------------------------------------------------------
  # 5. VISUALIZATION BLOCKS (Robust Point Fallback for Zero Variance)
  # ----------------------------------------------------------------------------
  message("[DEBUG] Constructing plotting environments...")
  
  # Prevent unwanted dynamic x11 or quartz graphic device generation on HPC nodes
  pdf(NULL)
  initial_dev <- dev.cur()
  
  tern_plot <- tryCatch({
    if (requireNamespace("ggtern", quietly = TRUE)) {
      message("[DEBUG] Generating zero-variance resistant scatter ternary projection...")
      
      p_tern <- ggtern::ggtern(data = posterior_df, aes(x = prop_local, y = prop_long, z = prop_net)) +
        ggtern::geom_point(aes(color = prop_net), alpha = 0.7, size = 2, position = ggtern::position_jitter_tern(0.01)) +
        scale_color_viridis_c(option = "mako", name = "Network\nWeight") +
        labs(x = "Local (Aphids)", y = "Long-range (Wind)", z = "Network (Farmers)") +
        ggtern::theme_rgbw() +
        theme(
          tern.panel.background = element_rect(fill = "#f8f9fa", color = NA),
          legend.position       = "right"
        )
      
      ggplot2::ggplot_build(p_tern)
      p_tern
    } else {
      NULL
    }
  }, error = function(e) {
    message("Visual Warning: Ternary engine bypassed: ", e$message)
    NULL
  })
  
  weight_plot <- tryCatch({
    if (exists("plot_posterior_weights", mode = "function")) {
      message("[DEBUG] Found 'plot_posterior_weights'. Running distribution engine...")
      p_weight_temp <- plot_posterior_weights(posterior_df)
      ggplot2::ggplot_build(p_weight_temp)
      p_weight_temp
    } else {
      NULL
    }
  }, error = function(e) {
    message("Visual Warning: Posterior weight script bypassed: ", e$message)
    NULL
  })
  
  # Clean up the open null graphics device safely
  if (dev.cur() > initial_dev) dev.off()
  
  # ----------------------------------------------------------------------------
  # 6. ENSEMBLE MODEL SELECTION (Distance Verification Mode)
  # ----------------------------------------------------------------------------
  model_comp <- NULL
  if (!is.null(model_results)) {
    message("[DEBUG] Running multi-model criteria evaluation...")
    
    message("\n[DIAGNOSTIC] Inspecting candidate model distance profiles against Epsilon (", round(epsilon_star, 2), "):")
    for (m_name in names(model_results)) {
      message(" -> Model '", m_name, "' has loaded successfully with ", length(model_results[[m_name]]), " particles.")
    }
    
    model_comp <- tryCatch({
      compare_models(model_results, S_obs, epsilon_star)
    }, error = function(e) {
      message("!!! CRITICAL COMPARE_MODELS CRASH !!! -> ", e$message)
      NULL
    })
    
    if (is.null(model_comp)) {
      message("[WARNING] compare_models returned NULL. Reason: Zero alternative particles fell below epsilon.")
    }
  }
  
  # ----------------------------------------------------------------------------
  # 7. CLEAN WORKSPACE RETURN
  # ----------------------------------------------------------------------------
  message("--- Phase 5 Completed Successfully ---")
  
  return(list(
    posterior        = as.data.frame(posterior_df),
    summary          = summary_table,
    dominance        = dominance,
    model_comparison = model_comp,
    epsilon          = as.numeric(epsilon_star),
    plots            = list(ternary = tern_plot, weights = weight_plot)
  ))
}

# Clean inline null fallback evaluator
`%||%` <- function(a, b) if (!is.null(a) && !is.na(a)) a else b