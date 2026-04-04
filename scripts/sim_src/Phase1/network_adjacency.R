# ==========================================================
# NETWORK ADJACENCY
# ==========================================================


# scripts/phase1/network_adjacency.R

create_adjacency_matrix <- function(farm_ids, mode = "topological") {
  
  K <- length(farm_ids)
  
  if (mode == "topological") {
    # No prior structure → fully connected binary placeholder
    A <- matrix(1, nrow = K, ncol = K)
    diag(A) <- 0
  } else {
    stop("Unknown adjacency mode")
  }
  
  rownames(A) <- farm_ids
  colnames(A) <- farm_ids
  
  return(A)
}