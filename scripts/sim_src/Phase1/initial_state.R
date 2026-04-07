# ==========================================================
# INITIAL STATE
# ==========================================================

create_initial_state <- function(data_full, year = 2018) {
  
  # 1. DEFINE THE UNIVERSE (The 379 unique field locations)
  # This matches the dimensions of your 379x379 distance matrix
  master_ids <- data_full %>%
    dplyr::select(Unique_Field_ID) %>%
    dplyr::distinct() %>%
    dplyr::arrange(Unique_Field_ID)
  
  N_total <- nrow(master_ids) # Confirmed 379
  
  # 2. EXTRACT THE 2018 OBSERVATIONS
  # We identify every field that had at least one diseased plant in 2018
  obs_2018 <- data_full %>%
    dplyr::filter(year == !!year) %>%
    dplyr::group_by(Unique_Field_ID) %>%
    dplyr::summarize(is_infected = any(NumbDiseasedPlant_Farm > 0), .groups = 'drop')
  
  # 3. INITIALIZE THE FULL VECTOR (379 Zeros)
  x0_vector <- rep(0L, N_total)
  
  # 4. MAP 2018 DATA TO THE 379 SLOTS
  # We use match() to ensure the 1s go exactly where the IDs align
  infected_ids <- obs_2018$Unique_Field_ID[obs_2018$is_infected == TRUE]
  x0_vector[master_ids$Unique_Field_ID %in% infected_ids] <- 1L
  
  # 5. DATA INTEGRITY CHECK
  # If 2018 is used, we expect more than just 1 or 2 infections
  n_infected <- sum(x0_vector)
  message(sprintf("Phase 1: X0 initialized for year %d. Found %d infected sites out of %d total fields.", 
                  year, n_infected, N_total))
  
  # 6. GEOMETRY GUARD
  # This prevents the 'non-conformable arguments' error in Phase 3
  if(length(x0_vector) != 379) {
    stop(paste("CRITICAL ALIGNMENT ERROR: X0 length is", length(x0_vector), "but must be 379!"))
  }
  
  return(as.integer(x0_vector))
}