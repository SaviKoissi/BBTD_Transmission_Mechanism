# ==========================================================
# COMPUTE DISTANCE MATRIX
# ==========================================================


# scripts/phase1/distance_matrix.R

compute_distance_matrix <- function(coords) {
  
  # Haversine or Euclidean (here Euclidean for projected coords)
  dist_matrix <- as.matrix(dist(coords, method = "euclidean"))
  
  rownames(dist_matrix) <- rownames(coords)
  colnames(dist_matrix) <- rownames(coords)
  
  return(dist_matrix)
}