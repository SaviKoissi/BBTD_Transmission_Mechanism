# ==========================================================
# RUN PHASE 2
# ==========================================================


run_phase2 <- function(phase1_obj,
                       n_particles = 20000,
                       seed = 123) {
  
  set.seed(seed)
  
  # ------------------------------------------------------------------
  # 1. Initialize simulation environment from your Phase 1 object
  # ------------------------------------------------------------------
  sim_env <- initialize_simulation(phase1_obj)
  
  # ------------------------------------------------------------------
  # 2. Load the uniform prior distributions for your latent variables
  # ------------------------------------------------------------------
  priors <- define_priors()
  
  # ------------------------------------------------------------------
  # 3. Generate the initial raw particle parameters batch
  # ------------------------------------------------------------------
  particles <- generate_particles(n_particles, priors)
  
  # ------------------------------------------------------------------
  # 4. Package everything into a clean Phase 2 object
  # ------------------------------------------------------------------
  phase2_obj <- list(
    simulation_env = sim_env,
    priors = priors,
    particles = particles
  )
  
  return(phase2_obj)
}