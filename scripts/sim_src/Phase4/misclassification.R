# ==========================================================
# MISCLASSIFICATION FILTER
# ==========================================================

# apply_misclassification <- function(X_true, Se = 0.78, Sp = 0.92) {
#   
#   X_obs <- sapply(X_true, function(x) {
#     if (x == 1) {
#       rbinom(1, 1, Se)        # true positive detected
#     } else {
#       rbinom(1, 1, 1 - Sp)    # false positive
#     }
#   })
#   
#   return(as.numeric(X_obs))
# }

apply_misclassification <- function(X_true, Se = 0.78, Sp = 0.92) {
  # Vectorized: significantly faster for 379 sites across 1000s of particles
  p_obs <- ifelse(X_true == 1, Se, 1 - Sp)
  X_obs <- rbinom(length(X_true), 1, p_obs)
  return(X_obs)
}