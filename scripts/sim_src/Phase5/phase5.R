# ==========================================================================
# PHASE 5 (FINAL) - COMPREHENSIVE PIPELINE ORCHESTRATOR
# ==========================================================================

# 1. Safely extract network architectures from your verified Phase 2 environment
if (!exists("network_full")) {
  if (!is.null(phase2$simulation_env$adjacency_matrix)) {
    message("[INFO] network_full not found globally. Mapping to phase2$simulation_env$adjacency_matrix.")
    network_full <- phase2$simulation_env$adjacency_matrix
  } else {
    stop("CRITICAL ERROR: 'adjacency_matrix' is missing from phase2$simulation_env.")
  }
}

# Ensure network_sparse exists; safely apply the threshold filter
if (!exists("network_sparse")) {
  if (!is.null(network_full)) {
    network_sparse <- network_full * (network_full > 0.5)
  } else {
    network_sparse <- NULL
  }
}

# 2. Build out structural variations by passing simulation data AND metadata
message("Processing Phase 4 simulation candidates across structural variations...")
all_candidate_models <- list(
  "Gaussian_Cauchy_FullNet"  = run_phase4(
    phase3_results = phase3_output, 
    sim_env        = phase2$simulation_env, 
    kernel_local   = "gaussian", 
    kernel_long    = "cauchy", 
    network_matrix = network_full
  ),
  "Gaussian_Levy_SparseNet"  = run_phase4(
    phase3_results = phase3_output, 
    sim_env        = phase2$simulation_env, 
    kernel_local   = "gaussian", 
    kernel_long    = "levy",   
    network_matrix = network_sparse
  ),
  "Expon_PowerLaw_FullNet"   = run_phase4(
    phase3_results = phase3_output, 
    sim_env        = phase2$simulation_env, 
    kernel_local   = "exponential", 
    kernel_long    = "powerlaw", 
    network_matrix = network_full
  ),
  "Expon_Cauchy_NoNet"       = run_phase4(
    phase3_results = phase3_output, 
    sim_env        = phase2$simulation_env, 
    kernel_local   = "exponential", 
    kernel_long    = "cauchy",   
    network_matrix = NULL
  )
)

# ==========================================================================
# DIAGNOSTIC CALIBRATION CHECK
# ==========================================================================
message("\n=== Running Pre-Inference Calibration Check ===")

sim_matrix <- do.call(rbind, lapply(all_candidate_models$Gaussian_Cauchy_FullNet, function(x) {
  if (is.list(x) && "abc_vector" %in% names(x)) return(x$abc_vector) else return(x)
}))

mean_simulated_metrics <- colMeans(sim_matrix, na.rm = TRUE)

print("--- ACTUAL TARGET FIELD METRICS (S_obs Output Structure) ---")
print(phase2$simulation_env$S_obs)

print("--- CURRENT SIMULATION AVERAGE OUTCOMES ---")
print(mean_simulated_metrics)

if (any(is.na(mean_simulated_metrics))) {
  warning("ALERT: Some summary statistic dimensions are generating NaNs. Visualizations may collapse.")
}
message("================================================\n")


# ==========================================================================
# PHASE 5: POSTERIOR INFERENCE RUN
# ==========================================================================
# Executes structural vector processing and runs multi-model selections safely
phase5_outputs <- run_phase5(
  phase4_results = all_candidate_models$Gaussian_Cauchy_FullNet, # Primary baseline
  phase2_obj     = phase2,                                       
  S_obs          = phase2$simulation_env$S_obs,                  
  prop           = 0.05,
  model_results  = all_candidate_models                          # Fed downstream
)

# 4. Save your final posterior parameters and summaries to disk
if (!dir.exists("output")) dir.create("output")
saveRDS(phase5_outputs, "output/Phase5_Inference.rds")
message("SUCCESS: Phase 5 completed cleanly! Output stored in output/Phase5_Inference.rds.")