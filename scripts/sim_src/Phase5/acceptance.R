# ==========================================================
# PARTICLE ACCEPTANCE
# ==========================================================

accept_particles <- function(results_phase4, S_obs, epsilon) {
  
  distances <- sapply(results_phase4, function(res) {
    compute_distance(res$S_sim, S_obs)
  })
  
  accepted_idx <- which(distances < epsilon)
  
  return(list(
    accepted = results_phase4[accepted_idx],
    distances = distances,
    accepted_idx = accepted_idx
  ))
}