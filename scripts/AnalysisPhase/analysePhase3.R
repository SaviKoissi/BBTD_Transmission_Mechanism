
# ======================================================================
# PHASE 3: VISUALIZATION - TERNARY WEIGHT CLOUD (MOCK SMC OUTPUT)
# ======================================================================

analyze_phase3_particles <- function(phase3_list) {
  # Clean out any NULL items or broken extractions safely
  valid_elements <- Filter(function(x) !is.null(x[["metrics"]]), phase3_list)
  num_particles  <- length(valid_elements)
  
  cat("Processing", num_particles, "validated simulation particles...\n")
  
  # 1. Aggregate tracking parameters across the entire population space
  parameter_space <- lapply(1:num_particles, function(i) {
    p <- valid_elements[[i]]
    data.frame(
      Particle_ID = i,
      w_local  = p$metrics$prop_local,
      w_long   = p$metrics$prop_long,
      w_net    = p$metrics$prop_net,
      winner   = p$metrics$kernel_winner,
      stringsAsFactors = FALSE
    )
  }) %>% bind_rows()
  
  # 2. Reconstruct the temporal growth trajectories safely
  trajectory_space <- lapply(1:num_particles, function(i) {
    p <- valid_elements[[i]]
    steps <- length(p$history)
    data.frame(
      Particle_ID = i,
      Step = 1:steps,
      Infected_Count = sapply(p$history, function(h) sum(h == 1))
    )
  }) %>% bind_rows()
  
  # Compute statistical profiles over time intervals
  trajectory_summary <- trajectory_space %>%
    group_by(Step) %>%
    summarise(
      Median_Inf = median(Infected_Count),
      Lower_Inf  = quantile(Infected_Count, 0.025),
      Upper_Inf  = quantile(Infected_Count, 0.975),
      .groups = "drop"
    )
  
  # ======================================================================
  # VISUALIZATION PANEL GENERATION
  # ======================================================================
  
  # Plot A: Exploding Infection Trajectories vs Noise Floor Base
  plot_trajectories <- ggplot(trajectory_summary, aes(x = Step)) +
    geom_ribbon(aes(ymin = Lower_Inf, ymax = Upper_Inf), fill = "#e0ecf4", alpha = 0.5) +
    geom_line(aes(y = Median_Inf), color = "#8856a7", size = 1.2) +
    geom_hline(yintercept = 379 * 0.08, linetype = "dashed", color = "red", size = 0.8) +
    annotate("text", x = 1.2, y = (379 * 0.08) + 15, label = "Savi 2026 Noise Floor (8%)", color = "red", fontface = "bold") +
    theme_minimal() +
    labs(title = "Epidemic Saturation Trajectories Across Iterations",
         x = "Simulation Step (Time)", y = "Total Infected Farms (Out of 379)")
  
  # Plot B: Structural Density Layout of Pathway Weights
  weight_long <- parameter_space %>%
    pivot_longer(cols = c(w_local, w_long, w_net), names_to = "Pathway", values_to = "Weight")
  
  plot_densities <- ggplot(weight_long, aes(x = Weight, fill = Pathway)) +
    geom_density(alpha = 0.6) +
    scale_fill_manual(values = c("w_local" = "#74a9cf", "w_long" = "#3690c0", "w_net" = "#023858"),
                      labels = c("w_local" = "Local Vector", "w_long" = "Long-Distance", "w_net" = "Seed Network")) +
    theme_minimal() +
    labs(title = "Posterior Parametric Space Distribution",
         x = "Assigned Pathway Weight (w)", y = "Density Profile")
  
  # Output integrated plot matrix layout
  print(plot_trajectories / plot_densities)
  
  # 3. Descriptive Stats Summaries for the Manuscript text
  cat("\n==================================================\n")
  cat("     CUSTOM PHASE 3 PARAMETER SPACE ANALYSIS      \n")
  cat("==================================================\n")
  cat("Total Aggregated Nodes Evaluated  : 379\n")
  cat("Mean Seed Network Influence (w3)  :", round(mean(parameter_space$w_net), 4), "\n")
  cat("Mean Local Vector Influence (w1)  :", round(mean(parameter_space$w_local), 4), "\n")
  cat("Model Kernel Winner Distribution  :\n")
  print(table(parameter_space$winner))
  cat("==================================================\n")
  
  return(list(parameters = parameter_space, trajectories = trajectory_summary))
}

# Run the updated profile check over your raw structure
parameter_space_diagnostics <- analyze_phase3_particles(phase3)
#==============================================================


# ==============================================================================
# ADVANCED BAYESIAN MODEL SELECTION: VECTOR INFRASTRUCTURE
# ==============================================================================


# 1. High-speed extraction of assigned kernels from your real phase3 object
extracted_metrics <- lapply(phase3, function(particle) {
  data.frame(
    local_winner = particle$metrics$local_winner,
    wind_winner  = particle$metrics$wind_winner,
    # Using the simulation error metric as our likelihood weight
    # Lower distance/error = higher match to Savi et al. 2026 targets
    distance     = ifelse(!is.null(particle$metrics$distance), particle$metrics$distance, 1)
  )
})

df_real_tournament <- bind_rows(extracted_metrics)

# 2. Evaluation Function for Model Tournaments
run_formal_bms <- function(df, column_name, category_label) {
  cat("\n==================================================\n")
  cat("  TOURNAMENT SELECTION:", category_label, "\n")
  cat("==================================================\n")
  
  summary_stats <- df %>%
    group_by(Kernel = !!sym(column_name)) %>%
    summarise(
      Accepted_Particles = n(),
      Mean_ABC_Distance  = mean(distance),
      .groups = 'drop'
    ) %>%
    mutate(
      # Transform distance to pseudo-likelihood: L = exp(-distance)
      Pseudo_Likelihood  = exp(-Mean_ABC_Distance),
      # Normalize to get posterior probability
      Posterior_Prob     = Pseudo_Likelihood / sum(Pseudo_Likelihood)
    ) %>%
    arrange(desc(Posterior_Prob))
  
  print(kable(summary_stats, digits = 4, format = "markdown"))
  
  # Calculate Bayes Factor for evidence strength
  if (nrow(summary_stats) >= 2) {
    bf <- summary_stats$Posterior_Prob[1] / summary_stats$Posterior_Prob[2]
    evidence <- case_when(
      bf > 100 ~ "Decisive Evidence",
      bf > 10  ~ "Strong Evidence",
      bf > 3   ~ "Substantial Evidence",
      TRUE     ~ "Anecdotal/Weak Evidence"
    )
    cat(paste0("\nBayes Factor (BF) for Winner vs Runner-up: ", round(bf, 2), " (", evidence, ")\n"))
  }
  return(summary_stats)
}

# 3. Execute Deep Inferences
local_bms_table <- run_formal_bms(df_real_tournament, "local_winner", "LOCAL CANOPY DYNAMICS")
wind_bms_table  <- run_formal_bms(df_real_tournament, "wind_winner", "WIND-ASSISTED ATMOSPHERIC DISPERSAL")


# ==============================================================================
# CALIBRATED ABC MODEL SELECTION (FILTERING FOR THE TOP 5% PERFORMERS)
# ==============================================================================

# 1. Define your ABC acceptance quantile (Top 5% closest matches to field data)
tolerance_threshold <- quantile(df_real_tournament$distance, probs = 0.05)

# 2. Filter the dataset to isolate only the elite particles
df_calibrated_subset <- df_real_tournament %>%
  filter(distance <= tolerance_threshold)

cat("--- POST-FILTER CALIBRATION HEALTH CHECK ---\n")
cat("Total particles evaluated       :", nrow(df_real_tournament), "\n")
cat("Particles surviving ABC filter   :", nrow(df_calibrated_subset), " (Top 5%)\n")
cat("Maximum allowed distance cutoff :", tolerance_threshold, "\n\n")

# ==============================================================================
# RE-RUN TOURNAMENTS ON GENUINE EMPIRICAL SURVIVORS
# ==============================================================================
run_formal_bms_calibrated <- function(df, column_name, category_label) {
  cat("==================================================\n")
  cat("  CALIBRATED TOURNAMENT:", category_label, "\n")
  cat("==================================================\n")
  
  summary_stats <- df %>%
    group_by(Kernel = !!sym(column_name)) %>%
    summarise(
      Accepted_Particles = n(),
      Mean_ABC_Distance  = mean(distance),
      .groups = 'drop'
    ) %>%
    mutate(
      # Standard Bayesian conversion from distance metric to relative likelihood
      Pseudo_Likelihood  = exp(-Mean_ABC_Distance),
      Posterior_Prob     = Pseudo_Likelihood / sum(Pseudo_Likelihood)
    ) %>%
    arrange(desc(Posterior_Prob))
  
  print(knitr::kable(summary_stats, digits = 4, format = "markdown"))
  
  if (nrow(summary_stats) >= 2) {
    bf <- summary_stats$Posterior_Prob[1] / summary_stats$Posterior_Prob[2]
    evidence <- case_when(
      bf > 100 ~ "Decisive Evidence",
      bf > 10  ~ "Strong Evidence",
      bf > 3   ~ "Substantial Evidence",
      TRUE     ~ "Anecdotal/Weak Evidence"
    )
    cat(paste0("\nBayes Factor (BF) for Winner vs Runner-up: ", round(bf, 2), " (", evidence, ")\n\n"))
  }
}

# Execute the final, calibrated selection
run_formal_bms_calibrated(df_calibrated_subset, "local_winner", "LOCAL CANOPY DYNAMICS")
run_formal_bms_calibrated(df_calibrated_subset, "wind_winner", "WIND-ASSISTED ATMOSPHERIC DISPERSAL")


# ==============================================================================
# ON-THE-FLY ABC CALIBRATION AND BIOPHYSICAL INFERENCE ENGINE
# ==============================================================================


# --- STEP 1: CALCULATE EMPIRICAL DISTANCES FOR ALL 20,000 PARTICLES ---
cat("Calculating empirical distances against field targets...\n")

# Target profile: 379 nodes matching your Savi et al. 2026 field observations.
# If you have a real target vector, swap it here. Otherwise, we calculate the 
# variance distance of X_obs against the global target prevalence baseline (0.5787)
calculated_metrics <- lapply(seq_along(phase3), function(i) {
  particle <- phase3[[i]]
  
  # Ensure the particle is healthy and contains simulation metrics
  if (!is.null(particle$X_obs) && !is.null(particle$metrics$local_winner)) {
    
    # Calculate Mean Squared Error (MSE) of simulated node states against target baseline
    # 1 represents an infected node, 0 represents healthy
    simulated_prevalence <- mean(particle$X_obs) 
    empirical_distance   <- (simulated_prevalence - 0.5787)^2
    
    return(data.frame(
      Particle_ID  = i,
      local_winner = particle$metrics$local_winner,
      wind_winner  = particle$metrics$wind_winner,
      distance     = empirical_distance,
      stringsAsFactors = FALSE
    ))
  } else {
    return(NULL)
  }
})

df_real_tournament <- bind_rows(calculated_metrics)

# --- STEP 2: APPLY THE ACCORDING 5% ABC TOLERANCE FILTER ---
tolerance_threshold  <- quantile(df_real_tournament$distance, probs = 0.05)
df_calibrated_subset <- df_real_tournament %>% filter(distance <= tolerance_threshold)

cat("\n--- POST-FILTER CALIBRATION HEALTH CHECK ---\n")
cat("Total valid particles processed :", nrow(df_real_tournament), "\n")
cat("Particles surviving ABC filter  :", nrow(df_calibrated_subset), " (Top 5%)\n")
cat("Maximum allowed distance cutoff :", round(tolerance_threshold, 6), "\n\n")

# --- STEP 3: EXECUTE THE BAYESIAN MODEL TOURNAMENT ---
run_formal_bms_calibrated <- function(df, column_name, category_label) {
  cat("==================================================\n")
  cat("  CALIBRATED TOURNAMENT:", category_label, "\n")
  cat("==================================================\n")
  
  summary_stats <- df %>%
    group_by(Kernel = !!sym(column_name)) %>%
    summarise(
      Accepted_Particles = n(),
      Mean_ABC_Distance  = mean(distance),
      .groups = 'drop'
    ) %>%
    mutate(
      # Standard Bayesian conversion from distance metric to relative likelihood
      Pseudo_Likelihood  = exp(-Mean_ABC_Distance),
      Posterior_Prob     = Pseudo_Likelihood / sum(Pseudo_Likelihood)
    ) %>%
    arrange(desc(Posterior_Prob))
  
  print(knitr::kable(summary_stats, digits = 4, format = "markdown"))
  
  if (nrow(summary_stats) >= 2) {
    bf <- summary_stats$Posterior_Prob[1] / summary_stats$Posterior_Prob[2]
    evidence <- case_when(
      bf > 100 ~ "Decisive Evidence",
      bf > 10  ~ "Strong Evidence",
      bf > 3   ~ "Substantial Evidence",
      TRUE     ~ "Anecdotal/Weak Evidence"
    )
    cat(paste0("\nBayes Factor (BF) for Winner vs Runner-up: ", round(bf, 2), " (", evidence, ")\n\n"))
  }
}

# Run the final calibrated choice models
run_formal_bms_calibrated(df_calibrated_subset, "local_winner", "LOCAL CANOPY DYNAMICS")
run_formal_bms_calibrated(df_calibrated_subset, "wind_winner", "WIND-ASSISTED ATMOSPHERIC DISPERSAL")


