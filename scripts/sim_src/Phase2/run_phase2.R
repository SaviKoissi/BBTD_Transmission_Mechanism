# ==========================================================
# RUN PHASE 2
# ==========================================================

run_phase2 <- function(phase1_obj,
                       n_particles = 1000,
                       seed = 123) {
  
  set.seed(seed)
  
  # ----------------------------
  # 1. Initialize simulation env
  # ----------------------------
  sim_env <- initialize_simulation(phase1_obj)
  
  # ----------------------------
  # 2. Load priors
  # ----------------------------
  priors <- define_priors()
  
  # ----------------------------
  # 3. Generate particles
  # ----------------------------
  particles <- generate_particles(n_particles, priors)
  
  # ----------------------------
  # 4. Package output
  # ----------------------------
  phase2_obj <- list(
    simulation_env = sim_env,
    priors = priors,
    particles = particles
  )
  
  return(phase2_obj)
}
