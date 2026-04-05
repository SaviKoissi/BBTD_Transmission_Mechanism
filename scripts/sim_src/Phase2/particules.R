# ==========================================================
# PARTICLE GENERATION
# ==========================================================

generate_particle <- function(priors) {
  
  params <- sample_parameters(priors)
  params <- transform_parameters(params)
  
  return(params)
}


generate_particles <- function(n_particles, priors) {
  
  particles <- vector("list", n_particles)
  
  for (i in seq_len(n_particles)) {
    particles[[i]] <- generate_particle(priors)
  }
  
  return(particles)
}
