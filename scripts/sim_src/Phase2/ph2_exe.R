# ==========================================================
# RUN PHASE 2 Exe
# ==========================================================

# Phase 2
phase2 <- run_phase2(phase1, n_particles = 2000)
saveRDS(phase2, "output/Phase2_Particles.rds")