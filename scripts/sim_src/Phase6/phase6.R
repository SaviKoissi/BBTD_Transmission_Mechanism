# ==============================================================================
# PIPELINE EXECUTION GATES WITH HARDENED ARCHIVAL LOADING
# ==============================================================================

# Source cleaner dependencies directly
source("scripts/sim_src/Phase6/network_selection.R")
source("scripts/sim_src/Phase6/run_phase6.R")

# 1. Recover original spatial landscape configurations
p2_runtime_context <- if (exists("phase2_obj", envir = .GlobalEnv)) {
  get("phase2_obj", envir = .GlobalEnv)
} else if (exists("phase2_results", envir = .GlobalEnv)) {
  get("phase2_results", envir = .GlobalEnv)
} else if (exists("phase1", envir = .GlobalEnv)) {
  get("phase1", envir = .GlobalEnv)
} else {
  absolute_cluster_path <- "./output/phase1_objects.rds"
  if (file.exists(absolute_cluster_path)) {
    message("[ORCHESTRATOR] Restoring Phase 1/2 spatial context layers from disk archive...")
    readRDS(absolute_cluster_path)
  } else {
    NULL
  }
}

# Ensure S_obs is retrieved safely
target_S_obs <- if (exists("S_obs", envir = .GlobalEnv)) get("S_obs", envir = .GlobalEnv) else p2_runtime_context$observed_metrics

# 2. Run analytical synthesis diagnostics (Phase 6 Entry Gate)
phase6_metrics <- run_phase6(
  posterior_df    = phase5_outputs$posterior,
  network_matrix  = p2_runtime_context$adjacency_matrix %||% p2_runtime_context$network_matrix %||% p2_runtime_context$network %||% NULL,
  distance_matrix = p2_runtime_context$distance_matrix  %||% p2_runtime_context$dist           %||% NULL,
  phase2_obj      = p2_runtime_context,
  S_obs           = target_S_obs
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

cat("\n=== NETWORK TOPOLOGY MODEL SELECTION SUMMARY ===\n")
if (!is.null(phase6_metrics$network_selection$summary_table)) {
  print(phase6_metrics$network_selection$summary_table)
  cat("\nMost Probable Network Structure:", phase6_metrics$network_selection$selected_network_mode, "\n\n")
} else {
  cat("Network topology selection table not available.\n\n")
}