# ==========================================================
# MISCLASSIFICATION (OBSERVATION MODEL)
# ==========================================================

apply_misclassification <- function(X_true, Se = 0.78, Sp = 0.92) {
  # Vectorized logic:
  # If X_true is 1, use Se. If X_true is 0, use (1 - Sp).
  p_obs <- ifelse(X_true == 1, Se, 1 - Sp)
  
  # Draw all observations in one go
  X_obs <- rbinom(length(X_true), 1, p_obs)
  
  return(X_obs)
}