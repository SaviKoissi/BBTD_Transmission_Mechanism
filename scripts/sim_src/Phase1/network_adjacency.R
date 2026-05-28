# ==========================================================
# NETWORK ADJACENCY
# ==========================================================

# scripts/phase1/network_adjacency.R

create_adjacency_matrix <- function(farm_ids, coords = NULL,
                                    distance_matrix = NULL, 
                                    mode = "topological") {
  
  K <- length(farm_ids)
  
  # Initialize an empty matrix
  A <- matrix(0, nrow = K, ncol = K)
  rownames(A) <- farm_ids
  colnames(A) <- farm_ids
  
  if (mode == "topological") {
    # HYPOTHESIS 1: Global Mean-Field Mixing (Your current placeholder)
    A[] <- 1
    diag(A) <- 0
    
  } else if (mode == "spatial_threshold") {
    # HYPOTHESIS 2: Pure Localized Spatial Spread (e.g., Short-range vector movement)
    if (is.null(distance_matrix)) stop("distance_matrix is required for spatial modes.")
    
    spatial_cutoff_km <- 15.0 # Farms can only interact if within 15km
    A[distance_matrix <= spatial_cutoff_km] <- 1
    diag(A) <- 0
    
  } else if (mode == "knn") {
    # HYPOTHESIS 3: k-Nearest Neighbor Topology (Localized agricultural clusters)
    if (is.null(distance_matrix)) stop("distance_matrix is required for KNN mode.")
    
    k <- 5 # Each farm links only to its 5 closest neighbors
    for(i in 1:K) {
      # Find indices of the closest neighbors (excluding self)
      nn <- order(distance_matrix[i, ])[2:(k + 1)]
      A[i, nn] <- 1
    }
    
  } else {
    stop("Unknown adjacency mode: choose 'topological', 'spatial_threshold', or 'knn'")
  }
  
  return(A)
}