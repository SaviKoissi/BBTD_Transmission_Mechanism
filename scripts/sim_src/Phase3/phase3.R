# ==========================================================
# PHASE 3
# ==========================================================

run_particle

# Phase 3
phase3 <- run_phase3(
  phase2,
  kernel_local_type = "gaussian",
  kernel_long_type = "powerlaw",
  parallel = TRUE
)

saveRDS(phase3, "output/Phase3.rds")