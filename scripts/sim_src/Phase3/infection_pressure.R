# ==========================================================
# FORCE OF INFECTION & STATE TRANSITION
# ==========================================================
  #' Compute Kernel Matrix with Normalization
  #' Ensures that the shape of the kernel doesn't artificially inflate beta.
  compute_kernel_matrix <- function(dist_mat, kernel_fun, kernel_params) {
    dist_mat <- as.matrix(dist_mat)
    
    if ("sigma" %in% names(kernel_params)) {
      # Local Kernel (Gaussian/Exponential)
      K <- kernel_fun(dist_mat, sigma = as.numeric(kernel_params$sigma))
    } else {
      # Long-Range Kernel (Powerlaw/Cauchy/Levy)
      K <- kernel_fun(dist_mat,
                      kappa = as.numeric(kernel_params$kappa),
                      p = as.numeric(kernel_params$p))
    }
    
    K <- as.matrix(K)
    diag(K) <- 0 # Remove self-infection
    
    # Crucial: normalize_kernel_matrix should be defined in your Phase 3/normalization.R
    return(normalize_kernel_matrix(K))
  }
  
  #' Compute Lambda with Pathway Attribution
    
  compute_lambda <- function(X, K_local, K_long, A, params) {
    X_vec <- as.numeric(unlist(X))
    
    # Calculate separate pressures
    p_local <- (as.matrix(K_local) %*% X_vec)
    p_long  <- (as.matrix(K_long) %*% X_vec)
    p_net   <- (as.matrix(A) %*% X_vec)
    
    # Apply weights (ensure beta_eff is used)
    lambda_local <- params$beta_eff * params$w1 * p_local
    lambda_long  <- params$beta_eff * params$w2 * p_long
    lambda_net   <- params$beta_eff * params$w3 * p_net
    
    return(list(
      total = as.vector(lambda_local + lambda_long + lambda_net),
      local_comp = as.vector(lambda_local),
      long_comp  = as.vector(lambda_long),
      net_comp   = as.vector(lambda_net)
    ))
  }
  
  update_state <- function(X, lambda_obj, dt = 1) {
    # Extraction with safety check
    L_total <- if(is.list(lambda_obj)) lambda_obj$total else lambda_obj
    
    X_old <- as.numeric(unlist(X))
    p_infect <- 1 - exp(-L_total * dt)
    is_infected_now <- rbinom(length(X_old), 1, p_infect)
    
    # Attribution logic for Phase 3 metrics
    newly_infected <- (X_old == 0 & is_infected_now == 1)
    attribution <- data.frame(local = 0, long = 0, net = 0)
    
    if(any(newly_infected) && is.list(lambda_obj)) {
      denom <- L_total[newly_infected]
      denom[denom <= 0] <- 1e-10 
      attribution$local <- sum(lambda_obj$local_comp[newly_infected] / denom)
      attribution$long  <- sum(lambda_obj$long_comp[newly_infected] / denom)
      attribution$net   <- sum(lambda_obj$net_comp[newly_infected] / denom)
    }
    
    return(list(
      new_state = pmax(X_old, is_infected_now),
      pathway_counts = attribution
    ))
  }  
