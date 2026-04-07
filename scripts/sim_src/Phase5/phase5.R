# ==========================================================
# PHASE 5 (FINAL)
# ==========================================================


# FINAL PHASE 5 EXECUTION
phase5 <- run_phase5(
  phase4_results = phase4_results,
  phase2_obj     = phase2,
  S_obs          = phase1$observed_metrics,
  prop           = 0.01,   # Let's be strict: Top 1% (best of the best)
  model_results  = list(
    gp = phase4_gp, 
    ec = phase4_ec
  )
)