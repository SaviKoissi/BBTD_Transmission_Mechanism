# ==========================================================
# PRIOR DEFINITIONS
# ==========================================================

define_priors <- function(config = NULL) {
  priors <- list(
    # 1. MODEL WEIGHTS (The "Probability" of each motion type)
    # w1 = Local Vector, w2 = Long-distance Kernel, w3 = Human Network
    dirichlet_alpha = c(1, 1, 1), 
    
    # 2. LOCAL SCALE (Gaussian/Exponential)
    sigma = c(min = 0.1, max = 15),     # Restricted to aphid flight range
    
    # 3. DISPERSAL KERNEL (Power-law / Cauchy / Lévy)
    # Higher kappa = further jumps; Lower p = fatter tails
    kappa = c(min = 1, max = 100),    
    p = c(min = 1.1, max = 5),         # 1.1 allows for very heavy Lévy tails
    
    # 4. NETWORK STRENGTH
    # This scales the importance of the Adjacency Matrix
    phi_network = c(min = 0.001, max = 0.5), 
    
    # 5. EPIDEMIC TEMPO
    beta = c(min = 0.001, max = 1),     # Transmission speed
    r = c(min = 0.01, max = 0.5),        # Growth rate
    tau = c(min = 1, max = 120)
  )
  return(priors)
}