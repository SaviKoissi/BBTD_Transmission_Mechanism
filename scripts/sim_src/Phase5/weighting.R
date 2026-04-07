# ==========================================================
# PARTICLE WEIGHTS
# ==========================================================

compute_weights <- function(distances, epsilon) {
  
  weights <- exp(- (distances^2) / (2 * epsilon^2))
  
  weights <- weights / sum(weights, na.rm = TRUE)
  
  return(weights)
}