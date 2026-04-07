# ==========================================================
# RESAMPLING
# ==========================================================

resample_particles <- function(particles, weights, n_particles) {
  
  idx <- sample(seq_along(particles),
                size = n_particles,
                replace = TRUE,
                prob = weights)
  
  return(particles[idx])
}