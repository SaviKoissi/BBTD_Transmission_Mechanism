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
  #' Separates the force of infection into Local, Long-Range, and Network.
  compute_lambda <- function(X, K_local, K_long, A, params) {
    # 1. Vectorize state
    X_vec <- as.numeric(as.vector(unlist(X)))
    N_cases <- length(X_vec)
    
    # 2. Rigor Check
    if (nrow(K_local) != N_cases) {
      stop(sprintf("Geometry Mismatch: Matrix has %d rows, but X has %d elements.",
                   nrow(K_local), N_cases))
    }
    
    # 3. Calculate separate pressures
    # We apply beta_eff and weights here so the components are ready for attribution
    p_local <- (as.matrix(K_local) %*% X_vec)
    p_long  <- (as.matrix(K_long) %*% X_vec)
    p_net   <- (as.matrix(A) %*% X_vec)
    
    lambda_local <- params$beta_eff * params$w1 * p_local
    lambda_long  <- params$beta_eff * params$w2 * p_long
    lambda_net   <- params$beta_eff * params$w3 * p_net
    
    # Total Force (The actual numeric vector used for the Poisson process)
    lambda_total <- as.vector(lambda_local + lambda_long + lambda_net)
    
    # Return a List (Object) to avoid the "invalid argument to unary operator" error
    return(list(
      total = lambda_total,
      local_comp = as.vector(lambda_local),
      long_comp  = as.vector(lambda_long),
      net_comp   = as.vector(lambda_net)
    ))
  }
  
  #' Update State with Competing Risks Logic
  #' Identifies which pathway 'won' for every new infection.
  update_state <- function(X, lambda_obj, dt = 1) {
    # EXTRACT: We must pull the numeric vector out of the list
    L_total <- lambda_obj$total
    L_local <- lambda_obj$local_comp
    L_long  <- lambda_obj$long_comp
    L_net   <- lambda_obj$net_comp
    
    X_old <- as.numeric(unlist(X))
    
    # 1. Stochastic binomial draw based on total pressure
    # This is where the "-lambda" error was happening; it's now explicitly -L_total
    p_infect <- 1 - exp(-L_total * dt)
    is_infected_now <- rbinom(length(X_old), 1, p_infect)
    
    # 2. Identify brand-new infections (where 0 became 1)
    newly_infected <- (X_old == 0 & is_infected_now == 1)
    
    # 3. PATHWAY ATTRIBUTION (The "Who Won?" Logic)
    # We initialize a dataframe to store the 'credit' for this time step
    attribution <- data.frame(local = 0, long = 0, net = 0)
    
    if(any(newly_infected)) {
      # Get the total pressure specifically for the sites that just got infected
      denom <- L_total[newly_infected]
      
      # Avoid division by zero
      denom[denom <= 0] <- 1e-10 
      
      # Sum up the proportional contribution of each pathway to these new cases
      attribution$local <- sum(L_local[newly_infected] / denom)
      attribution$long  <- sum(L_long[newly_infected] / denom)
      attribution$net   <- sum(L_net[newly_infected] / denom)
    }
    
    return(list(
      new_state = pmax(X_old, is_infected_now),
      pathway_counts = attribution
    ))
  }


# # ==========================================================
# # FORCE OF INFECTION & STATE TRANSITION (Denoised)
# # ==========================================================
# 
# compute_kernel_matrix <- function(dist_mat, kernel_fun, kernel_params) {
#   # 1. Type Safety: Ensure input is a raw matrix
#   dist_mat <- as.matrix(dist_mat)
#   
#   # 2. Dispatch: Extract parameters as strictly numeric to avoid 'non-numeric' errors
#   if ("sigma" %in% names(kernel_params)) {
#     K <- kernel_fun(dist_mat, sigma = as.numeric(kernel_params$sigma))
#   } else {
#     K <- kernel_fun(dist_mat, 
#                     kappa = as.numeric(kernel_params$kappa), 
#                     p = as.numeric(kernel_params$p))
#   }
#   
#   # 3. Structural Integrity: Convert result and kill self-infection loops
#   K <- as.matrix(K)
#   diag(K) <- 0 
#   
#   # Normalize to ensure weights sum correctly for the lambda calculation
#   return(normalize_kernel_matrix(K))
# }
# 
# compute_lambda <- function(X, K_local, K_long, A, params) {
#   # 1. Forced Vectorization (Cleans X of any dataframe/list attributes)
#   X_vec <- as.numeric(as.vector(unlist(X)))
#   N_cases <- length(X_vec)
#   
#   # 2. Geometry Guard (Prevents "non-conformable" crashes)
#   if (nrow(K_local) != N_cases) {
#     stop(sprintf("Geometry Mismatch: Matrix (%d) vs X (%d). Run Phase 1 again.", 
#                  nrow(K_local), N_cases))
#   }
#   
#   # 3. Path-Specific Pressures
#   # Each pathway is scaled by its weight (w) and its specific scaling factor
#   # w1, w2, w3 come from the Dirichlet; beta and phi from the LHS prior
#   
#   # Local Vector (Aphids)
#   L_local <- params$w1 * (as.matrix(K_local) %*% X_vec)
#   
#   # Long-Range (Wind/Jump)
#   L_long  <- params$w2 * (as.matrix(K_long) %*% X_vec)
#   
#   # Human Network (Trade) - Now includes 'phi' for global network strength
#   # We cast A to matrix to ensure matrix multiplication performance
#   L_net   <- params$w3 * as.numeric(params$phi) * (as.matrix(A) %*% X_vec)
#   
#   # 4. Total Force of Infection (Infection Pressure)
#   # Beta acts as the global 'tempo' of the epidemic
#   lambda <- as.numeric(params$beta) * (L_local + L_long + L_net)
#   
#   return(as.vector(lambda))
# }
# 
# update_state <- function(X, lambda, dt = 1) {
#   # 1. Calculate Probability (Infection over time step dt)
#   # 1 - exp(-lambda) is the standard transformation for Poisson rates
#   p_infect <- 1 - exp(-as.numeric(lambda) * dt)
#   
#   # 2. Stochastic Bernoulli Trial
#   # rbinom handles the 'coin flip' for each of the 379 sites
#   # Use unlist to ensure we are working with a clean vector
#   X_old <- as.numeric(unlist(X))
#   new_infections <- rbinom(n = length(X_old), size = 1, prob = p_infect)
#   
#   # 3. SI Model Logic: Once infected (1), stay infected (1)
#   # pmax(0,1)=1; pmax(1,0)=1; pmax(1,1)=1
#   X_new <- pmax(X_old, new_infections)
#   
#   return(X_new)
# }