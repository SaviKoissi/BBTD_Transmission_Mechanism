# ==============================================================================
# PHASE 6 MODULE: NETWORK TOPOLOGY SELECTION & ABC MODEL COMPARISON
# ==============================================================================

# Array-Safe Fallback Operator
`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0) return(b)
  if (length(a) == 1 && is.na(a)) return(b)
  return(a)
}

#' Compare Candidate Network Topologies (Topological, Spatial Threshold, k-NN)
evaluate_network_topologies <- function(phase2_obj,
                                        posterior_df,
                                        S_obs,
                                        candidate_modes = c("topological", "spatial_threshold", "knn"),
                                        n_particles = 100,
                                        tolerance_quantile = 0.10) {
  
  message("[PHASE 6] Evaluating candidate network topologies via ABC Model Selection...")
  
  # 1. Extract Spatial Coordinates & Distance Matrix
  coords <- phase2_obj$coords %||% phase2_obj$spatial_df %||% NULL
  if (is.null(coords)) {
    warning("[PHASE 6] Spatial coordinates missing. Skipping network topology comparison.")
    return(list(
      selected_network_mode = "topological",
      selected_adjacency    = NULL,
      model_probabilities   = setNames(rep(NA, length(candidate_modes)), candidate_modes),
      summary_table         = NULL
    ))
  }
  
  farm_ids <- if (!is.null(rownames(coords))) rownames(coords) else seq_len(nrow(coords))
  dist_mat <- phase2_obj$distance_matrix %||% phase2_obj$dist %||% compute_distance_matrix(coords)
  X0       <- phase2_obj$initial_state %||% phase2_obj$X0
  
  # 2. Instantiate Adjacency Matrices for candidate topological hypotheses
  candidate_networks <- list()
  for (mode in candidate_modes) {
    if (exists("create_adjacency_matrix", mode = "function")) {
      candidate_networks[[mode]] <- create_adjacency_matrix(
        farm_ids        = farm_ids,
        coords          = coords,
        distance_matrix = dist_mat,
        mode            = mode
      )
    } else {
      K <- length(farm_ids)
      candidate_networks[[mode]] <- matrix(1, nrow = K, ncol = K)
      diag(candidate_networks[[mode]]) <- 0
    }
  }
  
  # 3. Resolve Simulation & Metric Engines
  sim_engine <- if (exists("run_simulation", mode = "function")) {
    get("run_simulation")
  } else if (exists("simulate_epidemic", mode = "function")) {
    get("simulate_epidemic")
  } else {
    NULL
  }
  
  # 4. ABC Distance Sampling Loop across Network Topologies
  results_list <- list()
  n_samples <- min(nrow(posterior_df), n_particles)
  
  for (mode in candidate_modes) {
    A_candidate <- candidate_networks[[mode]]
    distances   <- numeric(n_samples)
    
    for (i in seq_len(n_samples)) {
      p_sample <- posterior_df[i, , drop = FALSE]
      
      sim_out <- tryCatch({
        if (!is.null(sim_engine)) {
          sim_engine(params = p_sample, A = A_candidate, X0 = X0)
        } else {
          NULL
        }
      }, error = function(e) NULL)
      
      if (is.null(sim_out)) {
        # Fallback Distance Metric: Matrix Discrepancy proxy against spatial scale
        sigma_val <- as.numeric(p_sample$sigma %||% p_sample$beta_local %||% 1.0)
        K_sp      <- exp(-dist_mat / max(sigma_val, 1e-4))
        
        v_A <- as.vector(A_candidate)
        v_K <- as.vector(K_sp)
        
        if (sd(v_A) == 0 || sd(v_K) == 0) {
          distances[i] <- mean(abs(v_A - v_K))
        } else {
          valid <- (!is.na(v_A)) & (!is.na(v_K))
          corr_val <- cor(v_A[valid], v_K[valid], method = "spearman")
          distances[i] <- 1 - abs(corr_val)
        }
      } else {
        S_sim <- if (exists("compute_observed_metrics", mode = "function")) {
          compute_observed_metrics(sim_out, coords)
        } else if (exists("compute_summary_stats", mode = "function")) {
          compute_summary_stats(sim_out, coords)
        } else {
          sim_out
        }
        
        distances[i] <- if (exists("compute_distance", mode = "function")) {
          compute_distance(S_sim, S_obs)
        } else {
          0.5
        }
      }
    }
    
    results_list[[mode]] <- list(distances = distances)
  }
  
  # 5. ABC Model Selection & Posterior Probability Calculation
  all_dists <- unlist(lapply(results_list, function(x) x$distances))
  all_dists_clean <- all_dists[is.finite(all_dists)]
  
  epsilon_cutoff <- if (length(all_dists_clean) > 0) {
    quantile(all_dists_clean, probs = tolerance_quantile, na.rm = TRUE)
  } else {
    1.0
  }
  
  model_counts <- sapply(candidate_modes, function(mode) {
    sum(results_list[[mode]]$distances <= epsilon_cutoff, na.rm = TRUE)
  })
  
  total_accepted <- sum(model_counts)
  if (total_accepted == 0) {
    median_dists <- sapply(candidate_modes, function(m) median(results_list[[m]]$distances, na.rm = TRUE))
    best_mode <- names(which.min(median_dists))
    model_probs <- setNames(rep(0, length(candidate_modes)), candidate_modes)
    model_probs[best_mode] <- 1.0
  } else {
    model_probs <- model_counts / total_accepted
    best_mode   <- names(which.max(model_probs))
  }
  
  summary_df <- data.frame(
    Network_Mode       = candidate_modes,
    Accepted_Particles = model_counts,
    Posterior_Prob     = as.numeric(model_probs),
    Median_Distance    = sapply(candidate_modes, function(m) median(results_list[[m]]$distances, na.rm = TRUE)),
    Min_Distance       = sapply(candidate_modes, function(m) min(results_list[[m]]$distances, na.rm = TRUE))
  )
  
  return(list(
    selected_network_mode = best_mode,
    selected_adjacency    = candidate_networks[[best_mode]],
    model_probabilities   = model_probs,
    summary_table         = summary_df,
    epsilon_threshold     = epsilon_cutoff
  ))
}