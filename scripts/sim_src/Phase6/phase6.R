# ==============================================================================
# PIPELINE EXECUTION GATES WITH HARDENED ARCHIVAL LOADING
# ==============================================================================

# Ensure our clean analytical function dependencies are compiled in RAM
source("scripts/sim_src/Phase6/run_phase6.R")

# 1. Recover original spatial landscape configurations
p2_runtime_context <- if (exists("phase2_obj", envir = .GlobalEnv)) {
  get("phase2_obj", envir = .GlobalEnv)
} else if (exists("phase2_results", envir = .GlobalEnv)) {
  get("phase2_results", envir = .GlobalEnv)
} else {
  absolute_cluster_path <- "./output/phase1_objects.rds"
  if (file.exists(absolute_cluster_path)) {
    message("[ORCHESTRATOR] Restoring Phase 1/2 spatial context layers from disk archive...")
    readRDS(absolute_cluster_path)
  } else {
    NULL # Pass safely; internal fallbacks inside Phase 6 will handle it
  }
}

# 2. Run analytical synthesis diagnostics (Phase 6 Entry Gate)
# Safely extracts metrics and executes calculations across your 100 posterior points
phase6_metrics <- run_phase6(
  posterior_df    = phase5_outputs$posterior,
  network_matrix  = p2_runtime_context$network_matrix  %||% p2_runtime_context$network %||% NULL,
  distance_matrix = p2_runtime_context$distance_matrix %||% p2_runtime_context$dist    %||% NULL,
  phase2_obj      = p2_runtime_context,
  S_obs           = S_obs
)

# ==============================================================================
# DISPLAY ANALYTICAL MATRIX RUN OUTPUTS 
# ==============================================================================
cat("\n=== PIPELINE METRIC ATTRIBUTION BREAKDOWN ===\n")
print(phase6_metrics$R0_decomposition)

cat("\n=== LANDSCAPE SENSITIVITY AND WAVEFRONT ELASTICITY ===\n")
print(phase6_metrics$elasticity)

cat("\n=== SEED NETWORK EXCHANGE SPATIAL CONGRUENCE ===\n")
cat("Index Value:", phase6_metrics$congruence$gamma_index, "\n")
cat("Interpretation:", phase6_metrics$congruence$interpretation, "\n\n")