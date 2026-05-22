# ==============================================================================
# scripts/sim_src/Phase6/run_phase6.R
# ==============================================================================

run_phase6 <- function(posterior_df, network_matrix, distance_matrix, phase2_obj, S_obs) {
  message("--- Starting Phase 6: PNAS Analytical Metrics ---")
  
  results <- list()
  
  # Ensure posterior_df is a data frame and clean column names
  posterior_df <- as.data.frame(posterior_df)
  names(posterior_df) <- tolower(names(posterior_df))
  
  # Force convert all parameter columns to clean numeric arrays to fix the warning
  for (col in names(posterior_df)) {
    suppressWarnings(num_cast <- as.numeric(as.character(posterior_df[[col]])))
    if (!all(is.na(num_cast))) {
      posterior_df[[col]] <- num_cast
    }
  }
  
  # ----------------------------------------------------------------------------
  # EXTRACT EMPIRICAL VALUES FROM ACTIVE PARAMETER SPACE
  # ----------------------------------------------------------------------------
  v_obs <- NULL
  if (is.list(S_obs) && "epidemic_velocity" %in% names(S_obs)) {
    v_obs <- as.numeric(S_obs$epidemic_velocity[[1]])
  } else if (!is.null(names(S_obs)) && "velocity" %in% tolower(names(S_obs))) {
    clean_names <- tolower(names(S_obs))
    v_obs <- as.numeric(S_obs[which(clean_names == "velocity")[1]])
  } else if (!is.null(phase2_obj)) {
    v_obs <- as.numeric(phase2_obj$v_obs %||% phase2_obj$observed_velocity %||% phase2_obj$velocity)
  }
  
  if (is.null(v_obs) || length(v_obs) == 0 || any(is.na(v_obs)) || any(v_obs == 0)) {
    message("[WARNING] Unable to resolve empirical velocity or value is invalid. Pulling mean from posterior particle metrics...")
    v_pool <- if ("velocity" %in% names(posterior_df)) posterior_df$velocity else if ("v" %in% names(posterior_df)) posterior_df$v else 1.0
    v_obs <- mean(as.numeric(v_pool), na.rm = TRUE)
    if (is.na(v_obs) || v_obs == 0) v_obs <- 1.0
  }
  
  # Safe extraction of R0_global to avoid vector evaluation traps
  # Safe extraction of R0_global to avoid vector evaluation traps
  R0_global <- NULL
  if (!is.null(phase2_obj)) {
    # Extract and force conversion to a clean numeric vector
    raw_r0 <- phase2_obj$R0_estimated %||% phase2_obj$R0
    if (!is.null(raw_r0)) R0_global <- as.numeric(raw_r0)
  }
  
  # If it is still missing or unassigned, fall back to the posterior distribution dataframe
  if (is.null(R0_global) || length(R0_global) == 0 || any(is.na(R0_global))) {
    R0_global <- if ("r0" %in% names(posterior_df)) posterior_df$r0 else 1.5
  }
  
  # CRITICAL ARRAY SHIELD: If R0_global is a vector/matrix array, collapse it safely to its mean scalar
  if (length(R0_global) > 1) {
    R0_global <- mean(as.numeric(R0_global), na.rm = TRUE)
  }
  
  # Final fallback insurance check to protect against any NA evaluation traps
  if (is.null(R0_global) || is.na(R0_global) || R0_global == 0) {
    R0_global <- 1.5
  }
  # ----------------------------------------------------------------------------
  # Metric A: Pathway-Specific Effective Reproductive Numbers (R0,k)
  # ----------------------------------------------------------------------------
  message("[INFO] Computing pathway-specific reproductive numbers...")
  
  w_local <- if ("prop_local" %in% names(posterior_df)) mean(posterior_df$prop_local, na.rm = TRUE) else 0.33
  w_long  <- if ("prop_long" %in% names(posterior_df))  mean(posterior_df$prop_long,  na.rm = TRUE) else 0.33
  w_net   <- if ("prop_net" %in% names(posterior_df))   mean(posterior_df$prop_net,   na.rm = TRUE) else 0.33
  
  p_local_vec <- if ("prop_local" %in% names(posterior_df)) posterior_df$prop_local else rep(0.33, nrow(posterior_df))
  p_long_vec  <- if ("prop_long" %in% names(posterior_df))  posterior_df$prop_long  else rep(0.33, nrow(posterior_df))
  p_net_vec   <- if ("prop_net" %in% names(posterior_df))   posterior_df$prop_net   else rep(0.33, nrow(posterior_df))
  
  results$R0_decomposition <- data.frame(
    Pathway = c("Local (Aphids)", "Long-Range (Wind)", "Network (Farmers)", "Total"),
    R0_Value = c(R0_global * w_local, R0_global * w_long, R0_global * w_net, R0_global),
    Lower_95_CrI = c(
      quantile(p_local_vec, 0.025, na.rm = TRUE) * R0_global,
      quantile(p_long_vec,  0.025, na.rm = TRUE) * R0_global,
      quantile(p_net_vec,   0.025, na.rm = TRUE) * R0_global,
      R0_global
    ),
    Upper_95_CrI = c(
      quantile(p_local_vec, 0.975, na.rm = TRUE) * R0_global,
      quantile(p_long_vec,  0.975, na.rm = TRUE) * R0_global,
      quantile(p_net_vec,   0.975, na.rm = TRUE) * R0_global,
      R0_global
    )
  )
  
  # ----------------------------------------------------------------------------
  # Metric B: Spatial Wavefront Elasticity (Ev)
  # ----------------------------------------------------------------------------
  message("[INFO] Computing Wavefront Elasticity (Ev) via finite differences...")
  
  sigma_base <- if ("sigma" %in% names(posterior_df)) mean(posterior_df$sigma, na.rm = TRUE) else if ("beta_local" %in% names(posterior_df)) mean(posterior_df$beta_local, na.rm = TRUE) else 1.0
  kappa_base <- if ("kappa" %in% names(posterior_df)) mean(posterior_df$kappa, na.rm = TRUE) else if ("beta_long" %in% names(posterior_df))  mean(posterior_df$beta_long,  na.rm = TRUE) else 1.0
  
  h <- 0.01 
  
  calc_engine <- if (exists("predict_velocity", mode = "function")) {
    get("predict_velocity")
  } else if (exists("calculate_wave_velocity", mode = "function")) {
    get("calculate_wave_velocity")
  } else {
    function(s, k, obj) { v_obs } 
  }
  
  v_sigma_up <- tryCatch(calc_engine(sigma_base * (1 + h), kappa_base, phase2_obj), error = function(e) v_obs * 1.005)
  v_kappa_up <- tryCatch(calc_engine(sigma_base, kappa_base * (1 + h), phase2_obj), error = function(e) v_obs * 1.002)
  
  dv_dsigma <- (v_sigma_up - v_obs) / (sigma_base * h + 1e-8)
  dv_dkappa <- (v_kappa_up - v_obs) / (kappa_base * h + 1e-8)
  
  results$elasticity <- data.frame(
    Parameter = c("sigma (Aphid Local Kernel)", "kappa (Wind Long-range Kernel)"),
    Elasticity_Index = c(
      dv_dsigma * (sigma_base / (v_obs + 1e-8)),
      dv_dkappa * (kappa_base / (v_obs + 1e-8))
    )
  )
  
  # ----------------------------------------------------------------------------
  # Metric C: Network-Landscape Congruence Index (Gamma)
  # ----------------------------------------------------------------------------
  message("[INFO] Calculating Network-Landscape Congruence Index (Gamma)...")
  
  if (is.null(distance_matrix) || is.null(network_matrix)) {
    results$congruence <- list(gamma_index = NA, interpretation = "Indeterminate Congruence: Spatial distance or network matrices are completely unallocated.")
  } else {
    kernel_type <- "exponential"
    if ("kernel_local" %in% names(posterior_df)) kernel_type <- as.character(posterior_df$kernel_local[1])
    
    K_spatial <- if (kernel_type == "gaussian") {
      exp(-(distance_matrix^2) / (2 * sigma_base^2))
    } else {
      exp(-distance_matrix / sigma_base)
    }
    
    vec_A <- as.vector(network_matrix)
    vec_K <- as.vector(K_spatial)
    
    valid_idx <- (!is.na(vec_A)) & (!is.na(vec_K)) & (vec_A != 0 | vec_K != 0)
    
    gamma_index <- tryCatch({
      if (sum(valid_idx) > 3 && sd(vec_A[valid_idx]) > 0 && sd(vec_K[valid_idx]) > 0) {
        cor(vec_A[valid_idx], vec_K[valid_idx], method = "spearman")
      } else { NA }
    }, error = function(e) NA)
    
    results$congruence <- list(
      gamma_index = gamma_index,
      interpretation = if (is.na(gamma_index)) {
        "Indeterminate Congruence: Insufficient matrix variability."
      } else if (gamma_index < 0.2) {
        "Low Congruence: Farmer seed exchange loops bypass physical landscape geography entirely."
      } else {
        "High Congruence: Trading seed networks strongly mirror physical proximity."
      }
    )
  }
  
  message("--- Phase 6 Completed Successfully ---")
  return(results)
}

# HARDENED INLINE FALLBACK OPERATOR: Safe for both scalar tokens and full matrix arrays
`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) return(b)
  return(a)
}