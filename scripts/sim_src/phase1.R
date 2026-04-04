# ==========================================================
# PHASE
# ==========================================================


run_phase1 <- function(data_full) {
  
  phase1 <- build_phase1(data_full)
  
  saveRDS(phase1, file = "output/phase1_objects.rds")
  
  return(phase1)
}

# In your master pipeline
phase1 <- run_phase1(posterior_prev)