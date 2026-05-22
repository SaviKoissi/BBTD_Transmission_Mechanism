# ==========================================================================
#  PHASE 4 EXECUTION BLOCK
# ==========================================================================

# 1. Run the Simulations (Phase 3)
# This generates the raw epidemic growth and pathway logs
phase3_output <- run_phase3(
  phase2, 
  kernel_local_type = "gaussian", 
  kernel_long_type = "powerlaw", 
  parallel = TRUE
)


# 2. Process the Results (Phase 4)
# Aligned directly with your root-level YAML structure and environment variables
# phase4_results <- run_phase4(
#   phase3_results = phase3_output, 
#   sim_env        = phase2$simulation_env,  # Fixed from bare sim_env
#   kernel_local   = config$models$kernel_local,   # Fixed path (removed $phase3)
#   kernel_long    = config$models$kernel_long,    # Fixed path (removed $phase3)
#   network_matrix = if (isTRUE(config$models$use_network)) network_full else NULL
# )
phase4_results <- run_phase4(
  phase3_results = phase3_output, 
  sim_env        = phase2$simulation_env,
  kernel_local   = "gaussian",  # Hardcoded to match your Phase 3 explicit arguments
  kernel_long    = "powerlaw",  # Hardcoded to match your Phase 3 explicit arguments
  network_matrix = if (exists("network_full")) network_full else NULL
)

# 3. Serialize outputs for Phase 5 particle filtration
saveRDS(phase4_results, "output/Phase4.rds")
message("[SUCCESS] Phase 4 processing complete. Metadata captured cleanly.")