# ==========================================================
# MISCLASSIFICATION FILTER
# ==========================================================

apply_misclassification <- function(X_true, Se = 0.78, Sp = 0.92) {
  # Vectorized: significantly faster for 379 sites across 1000s of particles
  p_obs <- ifelse(X_true == 1, Se, 1 - Sp)
  X_obs <- rbinom(length(X_true), 1, p_obs)
  return(X_obs)
}