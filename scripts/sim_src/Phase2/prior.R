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

# # ==========================================================
# # ENHANCED PRIOR GENERATOR (LHS & EXPANDED SPACE)
# # ==========================================================
# 
# define_priors <- function(n_particles = 2000) {
#   # 1. FORCE NUMERIC TYPES
#   # Prevents 'invalid arguments' if n_particles came from a YAML string
#   n <- as.integer(n_particles)
#   if (is.na(n) || n <= 0) stop("n_particles must be a positive integer.")
#   
#   # 2. DIRICHLET ALPHA (Strictly Numeric Vector)
#   # rgamma() is sensitive to the 'alpha' shape
#   alpha_vec <- as.numeric(c(1, 1, 1))
#   
#   # 3. GENERATE WEIGHTS (w1, w2, w3)
#   # We use tryCatch to ensure the cluster environment doesn't hang on an error
#   weights <- tryCatch({
#     MCMCpack::rdirichlet(n, alpha = alpha_vec)
#   }, error = function(e) {
#     # Fail-safe: Generate uniform weights that sum to 1
#     m <- matrix(runif(n * 3), ncol = 3)
#     m / rowSums(m)
#   })
#   
#   # 4. LATIN HYPERCUBE DESIGN (7 Dimensions)
#   lhs_design <- randomLHS(n, 7) 
#   
#   # 5. CONSTRUCT PARTICLE DATAFRAME
#   # Using qunif ensures we map the [0,1] LHS space to our biological ranges
#   priors <- data.frame(
#     w1 = as.numeric(weights[,1]), 
#     w2 = as.numeric(weights[,2]), 
#     w3 = as.numeric(weights[,3]),
#     
#     # Pathway scaling & tempo
#     beta  = qunif(lhs_design[,1], 0.001, 2.5),
#     r     = qunif(lhs_design[,2], 0.01, 0.8),
#     tau   = qunif(lhs_design[,3], 1, 120),
#     phi   = qunif(lhs_design[,4], 0.001, 0.7),
#     
#     # Spatial Scales
#     sigma = qunif(lhs_design[,5], 0.1, 25),
#     kappa = qunif(lhs_design[,6], 5, 250),
#     p     = qunif(lhs_design[,7], 1.01, 7.0)
#   )
#   
#   # 6. SPATIAL HIERARCHY RULE
#   # Enforce Long-range > Local to denoise the ABC-SMC search
#   priors$kappa <- pmax(priors$kappa, priors$sigma + 15)
#   
#   return(priors)
# }