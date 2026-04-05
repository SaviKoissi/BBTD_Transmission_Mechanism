# ==========================================================
# PARAMETER SAMPLING
# ==========================================================

sample_parameters <- function(priors) {
  
  # 1. Weights (Dirichlet)
  # This covers the 5 motion types via w1 (local) and w2 (long-range/Lévy/Cauchy)
  weights <- as.numeric(MCMCpack::rdirichlet(1, priors$dirichlet_alpha))
  
  # 2. Scale Sampling with Index Safety
  # We use [[1]] and [[2]] to ensure we get the numbers even if names (min/max) are lost
  s_val <- runif(1, priors$sigma[[1]], priors$sigma[[2]])
  k_val <- runif(1, priors$kappa[[1]], priors$kappa[[2]])
  
  # 3. Hierarchy Constraint
  # Ensures Long-range (kappa) > Local (sigma) to keep the 37.8 km/yr velocity realistic
  if(k_val < s_val) {
    tmp <- s_val
    s_val <- k_val
    k_val <- tmp
  }
  
  # 4. Final Parameter List
  # Using double brackets [[ ]] is safer for list elements
  params <- list(
    w1 = weights[1],
    w2 = weights[2],
    w3 = weights[3],
    
    r     = runif(1, priors$r[[1]],     priors$r[[2]]),
    tau   = runif(1, priors$tau[[1]],   priors$tau[[2]]),
    beta  = runif(1, priors$beta[[1]],  priors$beta[[2]]),
    
    sigma = s_val,
    kappa = k_val,
    p     = runif(1, priors$p[[1]],     priors$p[[2]])
  )
  
  return(params)
}