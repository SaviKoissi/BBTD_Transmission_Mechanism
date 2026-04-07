# ==========================================================
#  PHASE 4
# ==========================================================

# 1. Run the Simulations (Phase 3)
# This generates the raw epidemic growth and pathway logs
phase3_output <- run_phase3(
  phase2, 
  kernel_local_type = "gaussian", 
  kernel_long_type = "powerlaw", 
  parallel = TRUE
)

# 2. Process the Results (Phase 4)
# This calculates the 37.8 km/yr velocity and the % attribution
phase4_results <- run_phase4(
  phase3_results = phase3_output, 
  sim_env = phase2$simulation_env
)

saveRDS(phase4_results, "output/Phase4.rds")