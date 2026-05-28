# ==========================================================================
#  PHASE 4 EXECUTION BLOCK
# ==========================================================================

# Match your exact internal nomenclature
local_types <- c("gaussian", "exponential")
long_types  <- c("powerlaw", "cauchy", "levy")
model_grid  <- expand.grid(kernel_local_type = local_types, 
                           kernel_long_type = long_types, 
                           stringsAsFactors = FALSE)

total_particles <- 20000
particles_per_model <- total_particles / nrow(model_grid) # 3,333 particles per configuration

cat("Initializing Multi-Model Tournament across", nrow(model_grid), "structural configurations...\n")

phase4_results <- list()
particle_counter <- 1

for(m in 1:nrow(model_grid)) {
  current_local <- as.character(model_grid$kernel_local_type[m])
  current_long  <- as.character(model_grid$kernel_long_type[m])
  
  cat(sprintf(" -> Executing batch: kernel_local_type [%s] + kernel_long_type [%s]\n", 
              current_local, current_long))
  
  # 1. Generate active running particles for this specific configuration
  phase3_output <- run_phase3(
    phase2            = phase2, 
    kernel_local_type = current_local, 
    kernel_long_type  = current_long, 
    network_matrix    = if (exists("network_full")) network_full else NULL,
    parallel          = TRUE
  )
  
  # 2. Process results using identical naming options for the stats engine
  phase4_batch <- run_phase4(
    phase3_results = phase3_output, 
    sim_env        = phase2$simulation_env,
    kernel_local   = current_local,  
    kernel_long    = current_long,  
    network_matrix = if (exists("network_full")) network_full else NULL
  )
  
  # 3. Collate into your master collection structure
  for(i in seq_along(phase4_batch)) {
    phase4_results[[particle_counter]] <- phase4_batch[[i]]
    particle_counter <- particle_counter + 1
  }
}

# 4. Serialize your clean, functional multi-model outputs using your exact filename
saveRDS(phase4_results, "output/Phase4.rds")
message("[SUCCESS] Phase 4 has successfully processed active, running particles across all structural variations.")