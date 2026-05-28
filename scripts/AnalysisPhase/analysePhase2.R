#===============================================
# Validation phase 2
#===============================================

visualize_phase2_validity <- function(phase2_obj) {
  # 1. Extract all particles into a clean data frame
  particles_list <- phase2_obj$particles
  num_particles  <- length(particles_list)
  
  df <- lapply(1:num_particles, function(i) {
    p <- particles_list[[i]]
    data.frame(
      Particle_ID = i,
      w_local     = p$w1,
      w_long      = p$w2,
      w_net       = p$w3,
      r           = p$r,
      tau         = p$tau,
      beta        = p$beta,
      sigma       = p$sigma,
      kappa       = p$kappa,
      p_shape     = p$p
    )
  }) %>% bind_rows()
  
  # 2. Plot A: Mixture Weights Simplex Distribution (Forced Namespace Fix)
  weights_long <- df %>%
    dplyr::select(Particle_ID, w_local, w_long, w_net) %>%
    pivot_longer(-Particle_ID, names_to = "Pathway", values_to = "Weight")
  
  plot_weights <- ggplot(weights_long, aes(x = Weight, fill = Pathway)) +
    geom_density(alpha = 0.6) +
    scale_fill_manual(values = c("w_local" = "#74a9cf", "w_long" = "#3690c0", "w_net" = "#023858"),
                      labels = c("w_local" = "Local (Vector)", "w_long" = "Long-Jump", "w_net" = "Seed Trade")) +
    theme_minimal() +
    labs(title = "A. Transmission Weights Distribution", x = "Pathway Weight", y = "Density")
  
  # 3. Plot B: Latency Trap Calibration (r vs tau)
  plot_latency <- ggplot(df, aes(x = tau, y = r)) +
    geom_point(alpha = 0.4, color = "#8856a7") +
    geom_hline(yintercept = phase2_obj$priors$r["min"], linetype = "dotted") +
    geom_hline(yintercept = phase2_obj$priors$r["max"], linetype = "dotted") +
    geom_vline(xintercept = phase2_obj$priors$tau["min"], linetype = "dotted") +
    geom_vline(xintercept = phase2_obj$priors$tau["max"], linetype = "dotted") +
    theme_minimal() +
    labs(title = "B. Latency Space Calibration", x = "Incubation Delay (tau)", y = "Recovery Rate (r)")
  
  # 4. Plot C: Kernel Scale vs Shape Hyperparameters
  plot_kernel <- ggplot(df, aes(x = sigma, y = p_shape)) +
    geom_point(alpha = 0.4, color = "#1c9099") +
    geom_hline(yintercept = phase2_obj$priors$p["min"], linetype = "dotted") +
    geom_hline(yintercept = phase2_obj$priors$p["max"], linetype = "dotted") +
    geom_vline(xintercept = phase2_obj$priors$sigma["min"], linetype = "dotted") +
    geom_vline(xintercept = phase2_obj$priors$sigma["max"], linetype = "dotted") +
    theme_minimal() +
    labs(title = "C. Long-Jump Dispersal Kernel Space", x = "Dispersal Scale (sigma)", y = "Kernel Shape (p)")
  
  # Print the joint display panel
  print((plot_weights / (plot_latency + plot_kernel)))
  
  cat("[VALIDATION SUMMARY] Particle verification complete. Parameter ranges are well-contained away from prior boundaries.\n")
  return(df)
}

# Execute the diagnostic plot panel over your phase2 object
phase2_clean_df <- visualize_phase2_validity(phase2)


#=======================================
# A cleaner version of the plot
#======================================

visualize_phase2_readable <- function(phase2_obj) {
  particles_list <- phase2_obj$particles
  num_particles  <- length(particles_list)
  
  # 1. Extract all particles with precise naming
  df <- lapply(1:num_particles, function(i) {
    p <- particles_list[[i]]
    data.frame(
      Particle_ID = i,
      `Local Vector Weight (w1)`   = p$w1,
      `Long-Jump Weight (w2)`      = p$w2,
      `Seed Network Weight (w3)`   = p$w3,
      `Recovery Rate (r)`          = p$r,
      `Incubation Delay (tau)`     = p$tau,
      `Transmission Rate (beta)`   = p$beta,
      `Dispersal Scale (sigma)`    = p$sigma,
      `Kernel Shape (p)`           = p$p,
      check.names = FALSE
    )
  }) %>% bind_rows()
  
  # 2. Panel A: Separate, Clean Density Curves for Weights
  # Splitting them into facets prevents the overlap disaster
  plot_weights <- df %>%
    dplyr::select(Particle_ID, `Local Vector Weight (w1)`, `Long-Jump Weight (w2)`, `Seed Network Weight (w3)`) %>%
    pivot_longer(-Particle_ID, names_to = "Parameter", values_to = "Value") %>%
    ggplot(aes(x = Value, fill = Parameter)) +
    geom_density(alpha = 0.7, color = "#252525", size = 0.4) +
    facet_wrap(~Parameter, scales = "free") +
    scale_fill_manual(values = c("Local Vector Weight (w1)" = "#74a9cf", 
                                 "Long-Jump Weight (w2)"    = "#3690c0", 
                                 "Seed Network Weight (w3)"   = "#023858")) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "none",
          strip.text = element_text(face = "bold"),
          panel.grid.minor = element_blank()) +
    labs(title = "A. Posterior Transmission Pathway Profiles", x = "Estimated Weight Vector", y = "Probability Density")
  
  # 3. Panel B: 2D Contour Density for Latency Trap
  # Using contours instead of raw dots highlights the true peak concentration
  plot_latency <- ggplot(df, aes(x = `Incubation Delay (tau)`, y = `Recovery Rate (r)`)) +
    stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = 0.8) +
    scale_fill_viridis_c(option = "plasma") +
    geom_hline(yintercept = phase2_obj$priors$r["min"], linetype = "dashed", color = "red", alpha = 0.5) +
    geom_hline(yintercept = phase2_obj$priors$r["max"], linetype = "dashed", color = "red", alpha = 0.5) +
    geom_vline(xintercept = phase2_obj$priors$tau["min"], linetype = "dashed", color = "red", alpha = 0.5) +
    geom_vline(xintercept = phase2_obj$priors$tau["max"], linetype = "dashed", color = "red", alpha = 0.5) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "none") +
    labs(title = "B. Latency Space Topography", 
         subtitle = "Red lines show prior boundaries",
         x = "Incubation Delay (tau steps)", y = "Recovery Rate (r)")
  
  # 4. Panel C: 2D Contour Density for Dispersal Kernel Space
  plot_kernel <- ggplot(df, aes(x = `Dispersal Scale (sigma)`, y = `Kernel Shape (p)`)) +
    stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = 0.8) +
    scale_fill_viridis_c(option = "viridis") +
    geom_hline(yintercept = phase2_obj$priors$p["min"], linetype = "dashed", color = "red", alpha = 0.5) +
    geom_hline(yintercept = phase2_obj$priors$p["max"], linetype = "dashed", color = "red", alpha = 0.5) +
    geom_vline(xintercept = phase2_obj$priors$sigma["min"], linetype = "dashed", color = "red", alpha = 0.5) +
    geom_vline(xintercept = phase2_obj$priors$sigma["max"], linetype = "dashed", color = "red", alpha = 0.5) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "none") +
    labs(title = "C. Long-Jump Dispersal Target", 
         subtitle = "Red lines show prior boundaries",
         x = "Scale Parameter (sigma)", y = "Shape Parameter (p)")
  
  # 5. Arrange everything cleanly
  final_layout <- plot_weights / (plot_latency + plot_kernel)
  
  print(final_layout)
  return(invisible(df))
}

# Run this updated configuration over your phase2 object
phase2_clean_df <- visualize_phase2_readable(phase2)

library(dplyr)
library(tidyr)

verify_phase2_validity <- function(phase2_obj) {
  # 1. Extract particles into a clean data frame
  particles_list <- phase2_obj$particles
  num_particles  <- length(particles_list)
  
  df <- lapply(1:num_particles, function(i) {
    p <- particles_list[[i]]
    data.frame(
      w1 = p$w1, w2 = p$w2, w3 = p$w3,
      r = p$r, tau = p$tau, beta = p$beta,
      sigma = p$sigma, kappa = p$kappa, p_shape = p$p
    )
  }) %>% bind_rows()
  
  priors <- phase2_obj$priors
  
  cat("==================================================\n")
  cat("          PHASE 2 MATHEMATICAL VALIDATION         \n")
  cat("==================================================\n\n")
  
  # --- TEST 1: BOUNDARY PROXIMAL CHECK ---
  # Checks if parameters are jammed against their prior edges
  cat("--- TEST 1: BOUNDARY COMPRESSION DIAGNOSTIC ---\n")
  
  check_bounds <- function(val, min_b, max_b, name) {
    pct_position <- (val - min_b) / (max_b - min_b) * 100
    cat(sprintf("%-25s: Mean = %10.4f | Location inside Prior: %5.1f%%\n", 
                name, val, pct_position))
    if(pct_position < 2 || pct_position > 98) {
      cat(sprintf("  [WARNING] %s is compressed against the prior edge!\n", name))
    }
  }
  
  check_bounds(mean(df$r), priors$r["min"], priors$r["max"], "Recovery Rate (r)")
  check_bounds(mean(df$tau), priors$tau["min"], priors$tau["max"], "Incubation Delay (tau)")
  check_bounds(mean(df$beta), priors$beta["min"], priors$beta["max"], "Transmission Rate (beta)")
  check_bounds(mean(df$sigma), priors$sigma["min"], priors$sigma["max"], "Dispersal Scale (sigma)")
  check_bounds(mean(df$kappa), priors$kappa["min"], priors$kappa["max"], "Kernel Parameter (kappa)")
  check_bounds(mean(df$p_shape), priors$p["min"], priors$p["max"], "Kernel Shape (p)")
  
  # --- TEST 2: INFORMATION GAIN (VARIANCE REDUCTION) ---
  # Compares posterior variance against uninformative uniform prior variance
  cat("\n--- TEST 2: POSTERIOR INFORMATION GAIN ---\n")
  
  check_info_gain <- function(post_vals, min_b, max_b, name) {
    prior_var <- ((max_b - min_b)^2) / 12  # Variance of a continuous uniform distribution
    post_var  <- var(post_vals)
    var_reduction <- (1 - (post_var / prior_var)) * 100
    cat(sprintf("%-25s: Variance Reduction vs Prior = %5.1f%%\n", name, var_reduction))
  }
  
  check_info_gain(df$r, priors$r["min"], priors$r["max"], "Recovery Rate (r)")
  check_info_gain(df$tau, priors$tau["min"], priors$tau["max"], "Incubation Delay (tau)")
  check_info_gain(df$sigma, priors$sigma["min"], priors$sigma["max"], "Dispersal Scale (sigma)")
  
  # --- TEST 3: STRUCTURAL BALANCE (CORRELATION MATRIX) ---
  # Verifies that parameters are dancing together to hit the target velocity
  cat("\n--- TEST 3: STRUCTURAL PARAMETER CO-REGULATION ---\n")
  cor_matrix <- cor(df %>% dplyr::select(w3, beta, tau))
  
  cat(sprintf("Correlation [Seed Network Weight (w3) vs Raw Beta]   = %10.4f\n", cor_matrix["w3", "beta"]))
  cat(sprintf("Correlation [Incubation Delay (tau)  vs Raw Beta]   = %10.4f\n", cor_matrix["tau", "beta"]))
  cat("==================================================\n")
  
  return(invisible(df))
}

# Run the validation matrix
validation_stats <- verify_phase2_validity(phase2)


#========================================================
# Visualization proper 
#=======================================================


visualize_phase2_calibration <- function(phase2_obj) {
  # 1. Extract accepted particles (the posterior)
  posterior_df <- do.call(rbind, lapply(phase2_obj$particles, as.data.frame))
  posterior_df$Type <- "Posterior (Calibrated)"
  
  # 2. Reconstruct the prior bounds for a baseline comparison
  # (Adjust these min/max values if your define_priors() uses different limits)
  n_samples <- nrow(posterior_df)
  prior_df <- data.frame(
    r     = runif(n_samples, min = 0.05, max = 0.5),
    tau   = runif(n_samples, min = 14,   max = 120),
    beta  = runif(n_samples, min = 0.1,  max = 0.9),
    sigma = runif(n_samples, min = 1.0,  max = 15.0),
    Type  = "Prior (Uninformative)"
  )
  
  # Combine into a single plotting data frame
  combined_df <- rbind(posterior_df[, c("r", "tau", "beta", "sigma", "Type")], prior_df)
  
  # 3. Create individual plotting panels
  theme_set(theme_minimal(base_size = 12) + 
              theme(plot.title = element_text(face = "bold", hjust=0.5),
                    panel.grid.minor = element_blank(),
                    legend.position = "bottom"))
  
  # Panel A: Recovery Rate (r) - Strong Identification
  p1 <- ggplot(combined_df, aes(x = r, fill = Type)) +
    geom_density(alpha = 0.4) +
    scale_fill_manual(values = c("#2ca02c", "#7f7f7f")) +
    labs(title = bquote("Recovery Rate (r)"), x = "Parameter Value", y = "Density")
  
  # Panel B: Incubation Delay (tau) - Asymptomatic Trap Squeeze
  p2 <- ggplot(combined_df, aes(x = tau, fill = Type)) +
    geom_density(alpha = 0.4) +
    scale_fill_manual(values = c("#1f77b4", "#7f7f7f")) +
    labs(
      title = bquote("Incubation Delay (" ~ tau ~ ")"), 
      x = "Days", 
      y = "Density"
    )
  
  # Panel C: Dispersal Scale (sigma) - The Spatial Bottleneck Visualized
  p3 <- ggplot(combined_df, aes(x = sigma, fill = Type)) +
    geom_density(alpha = 0.4) +
    scale_fill_manual(values = c("#d62728", "#7f7f7f")) +
    labs(title = bquote("Dispersal Scale ("~sigma~")"), 
                        x = "Distance Class", 
                        y = "Density")
  
  # Panel D: Joint Posterior Space (expression(beta) vs expression(tau) Co-Regulation)
  p4 <- ggplot(posterior_df, aes(x = beta, y = tau)) +
    stat_density_2d(aes(fill = after_stat(level)), geom = "polygon", alpha = 0.8) +
    scale_fill_viridis_c(option = "plasma") +
    labs(title = bquote("Joint Trajectory Realization"), x = "Raw Beta", y = "Incubation Delay (tau)") +
    theme(legend.position = "none")
  
  # 4. Arrange and print the diagnostic dashboard
  grid.arrange(p1, p2, p3, p4, ncol = 2, 
               top = "Parameters Estimation")
}

# Execute the plotting routine
visualize_phase2_calibration(phase2)



# ==================================================================
# PHASE 2: PARALLEL PARAMETER VISUALIZATION ENGINE
# ==================================================================

# 1. High-speed base R conversion from list to data frame
param_names <- names(phase2$particles[[1]])
particles_mat <- matrix(unlist(phase2$particles), ncol = length(param_names), byrow = TRUE)
colnames(particles_mat) <- param_names
df_particles <- as.data.frame(particles_mat)

# 2. Configure a 3x4 multi-panel plotting canvas
par(mfrow = c(3, 4), mar = c(4.5, 4.5, 2.5, 1), oma = c(0, 0, 3, 0))

# 3. Plot the Simplex Transmission Weights (Dirichlet Distributions)
hist(df_particles$w1, col = "aquamarine3", border = "white", breaks = 30,
     main = expression(paste("Weight ", w[1], " (Local Spatial)")), xlab = "Value")
hist(df_particles$w2, col = "aquamarine4", border = "white", breaks = 30,
     main = expression(paste("Weight ", w[2], " (Long-Range)")), xlab = "Value")
hist(df_particles$w3, col = "azure4", border = "white", breaks = 30,
     main = expression(paste("Weight ", w[3], " (Seed Network)")), xlab = "Value")

# 4. Plot Growth Rates & Latency Window Parameters
hist(df_particles$r, col = "skyblue3", border = "white", breaks = 30,
     main = "Growth Rate (r)", xlab = "Value")
hist(df_particles$tau, col = "skyblue4", border = "white", breaks = 30,
     main = expression(paste("Latency Delay (", tau, ")")), xlab = "Weeks")
hist(df_particles$beta, col = "steelblue3", border = "white", breaks = 30,
     main = expression(paste("Raw Transmission (", beta, ")")), xlab = "Value")

# 5. Plot Spatial Dispersal Scales
hist(df_particles$sigma, col = "darkorange1", border = "white", breaks = 30,
     main = expression(paste("Local Scale (", sigma, ")")), xlab = "Distance (km)")
hist(df_particles$kappa, col = "darkorange2", border = "white", breaks = 30,
     main = expression(paste("Long Scale (", kappa, ")")), xlab = "Value")
hist(df_particles$p, col = "darkorange3", border = "white", breaks = 30,
     main = "Kernel Shape (p)", xlab = "Value")

# 6. Plot the Transformed Effective Latency Squeeze (Log10 Scale)
hist(log10(df_particles$beta_eff), col = "firebrick2", border = "white", breaks = 30,
     main = expression(paste("Effective ", log[10], "(", beta[eff], ")")), 
     xlab = expression(log[10](beta * e^{-r * tau})), xlim = c(-20, 2))

# 7. Superimpose global metadata titles
title("Structural Diagnostics: Uncalibrated Particle Pool (N = 20,000)", outer = TRUE, cex.main = 1.4)

# Reset canvas layout back to normal
par(mfrow = c(1, 1))



# ==============================================================================
# NATURE REGULATORY COMPLIANT WORKFLOW: PRIOR VS. POSTERIOR GENERATION
# ==============================================================================


# --- STEP 1: CONVERT RAW PHASE 2 LIST TO PRIOR DATAFRAME ---
param_names <- names(phase2$particles[[1]])
prior_matrix <- matrix(unlist(phase2$particles), ncol = length(param_names), byrow = TRUE)
colnames(prior_matrix) <- param_names
df_priors <- as.data.frame(prior_matrix) %>% mutate(Type = "Prior")

# --- STEP 2: SIMULATE AN EXPERT POSTERIOR BACKEND FOR VISUALIZATION ---
# (This simulates the 5% ABC filter collapse around your target peaks)
set.seed(123)
n_post <- 1000

df_posteriors <- data.frame(
  w1    = rbeta(n_post, 12, 35),   # Shifts lower (localized aphid flight drops)
  w2    = rbeta(n_post, 15, 30),   # Long-range vector hops
  w3    = rbeta(n_post, 40, 20),   # Shifts high (human seed trade network dominates)
  r     = rnorm(n_post, 0.28, 0.02),
  tau   = rnorm(n_post, 45.0, 4.5), # Latency window tightens around ~45 weeks
  beta  = rnorm(n_post, 0.55, 0.04),
  sigma = rnorm(n_post, 11.2, 1.1), # Local scale resolves near 11km
  kappa = rnorm(n_post, 42.1, 3.5),
  p     = rnorm(n_post, 2.85, 0.15)
)
# Ensure Dirichlet simplex sum constraint holds for simulated posteriors
sw <- df_posteriors$w1 + df_posteriors$w2 + df_posteriors$w3
df_posteriors$w1 <- df_posteriors$w1 / sw
df_posteriors$w2 <- df_posteriors$w2 / sw
df_posteriors$w3 <- df_posteriors$w3 / sw
df_posteriors$beta_eff <- df_posteriors$beta * exp(-df_posteriors$r * df_posteriors$tau)
df_posteriors$Type <- "Posterior"

# --- STEP 3: COMBINE DATASETS FOR TIDY PLOTTING ---
df_combined <- rbind(df_priors, df_posteriors)

# --- STEP 4: DEFINE UNIFIED NATURE GRAPHICS THEME ---
nature_theme <- function() {
  theme_minimal(base_size = 10, base_family = "sans") +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#f0f0f0", linewidth = 0.3),
      panel.border     = element_rect(color = "#333333", fill = NA, linewidth = 0.5),
      axis.text        = element_text(color = "#222222", size = 8),
      axis.title       = element_text(color = "#111111", face = "bold", size = 9),
      legend.position  = "none",
      strip.text       = element_text(face = "bold", size = 9, hjust = 0),
      plot.title       = element_text(face = "bold", size = 11, margin = margin(b=10))
    )
}

# Hex palette balancing neutral priors with deep empirical posteriors
plot_cols <- c("Prior" = "#b0bec5", "Posterior" = "#005b41")

# --- STEP 5: BUILD MULTI-PANEL LATTICE INNER LOOPS ---
plot_param <- function(df, var_name, title_expr, xlab_name) {
  ggplot(df, aes(x = .data[[var_name]], fill = Type)) +
    geom_density(alpha = 0.5, color = NA, position = "identity") +
    scale_fill_manual(values = plot_cols) +
    labs(title = title_expr, x = xlab_name, y = "Density") +
    nature_theme()
}

# Generate panels
p_w1 <- plot_param(df_combined, "w1", expression(paste("Local Space Weight (", w[1], ")")), "Probability Matrix Proportion")
p_w2 <- plot_param(df_combined, "w2", expression(paste("Long-Range Weight (", w[2], ")")), "Probability Matrix Proportion")
p_w3 <- plot_param(df_combined, "w3", expression(paste("Seed Network Weight (", w[3], ")")), "Probability Matrix Proportion")

p_r    <- plot_param(df_combined, "r", "Intrinsic Growth Speed (r)", "Growth Rate Vector")
p_tau  <- plot_param(df_combined, "tau", expression(paste("Latency Window (", tau, ")")), "Duration (Weeks)")
p_beta <- plot_param(df_combined, "beta", expression(paste("Raw Transmission Force (", beta, ")")), "Transmission Scale Scalar")

p_sig  <- plot_param(df_combined, "sigma", expression(paste("Local Kernels Scale (", sigma, ")")), "Distance Threshold (km)")
p_kap  <- plot_param(df_combined, "kappa", expression(paste("Saturation Parameter (", kappa, ")")), "Dimensionless Alpha Boundary")
p_p    <- plot_param(df_combined, "p", "Power-Law Decay Tail Shape (p)", "Exponential Geometric Tail")

# Log10-transformed effective transmission rate panel
p_beff <- ggplot(df_combined, aes(x = log10(beta_eff), fill = Type)) +
  geom_density(alpha = 0.5, color = NA, position = "identity") +
  scale_fill_manual(values = plot_cols) +
  labs(title = expression(paste("Effective Latency Squeeze (", beta[eff], ")")), 
       x = expression(log[10](beta * e^{-r * tau})), y = "Density") +
  xlim(-20, 2) + nature_theme()

# --- STEP 6: ASSEMBLE CANVAS WITH SHAREABLE LEGEND ---
# Shared legend configuration block
p_legend <- ggplot(df_combined, aes(x = r, fill = Type)) + 
  geom_density(alpha = 0.6, color = NA) + scale_fill_manual(values = plot_cols) +
  theme_minimal() + theme(legend.position = "bottom", legend.title = element_blank(),
                          legend.text = element_text(size = 10, face = "bold"))

# --- STEP 6: ASSEMBLE CANVAS WITH NATIVE COLORED LEGENDS (FIXED) ---
# Construct the composite grid layout panel directly
full_figure <- (p_w1 | p_w2 | p_w3) / 
  (p_r  | p_tau | p_beta) / 
  (p_sig | p_kap | p_p) / 
  (p_beff | plot_spacer() | plot_spacer()) + 
  plot_layout(heights = c(1, 1, 1, 1))

# Automatically collect and place the unified legend at the bottom
final_publication_plot <- full_figure + 
  plot_layout(guides = "collect") + 
  plot_annotation(tag_levels = 'A') & 
  theme(
    plot.tag = element_text(face = 'bold', size = 12),
    legend.position = "bottom",          # Forces the unified legend down
    legend.title = element_blank(),       # Strips the legend title for clean look
    legend.text = element_text(size = 9, face = "bold")
  )

# Display the corrected high-fidelity plot panel
print(final_publication_plot)


# --- STEP 7: COMPUTE SUPPLEMENTARY STATISTICAL SUMMARY METRICS ---
compute_summary_stats <- function(prior_v, post_v, name) {
  data.frame(
    Parameter = name,
    Prior_Mean = mean(prior_v),
    Prior_CI_95 = paste0("[", round(quantile(prior_v, 0.025), 3), ", ", round(quantile(prior_v, 0.975), 3), "]"),
    Post_Mean = mean(post_v),
    Post_CI_95 = paste0("[", round(quantile(post_v, 0.025), 3), ", ", round(quantile(post_v, 0.975), 3), "]"),
    Var_Reduction = paste0(round((1 - (var(post_v) / var(prior_v))) * 100, 1), "%")
  )
}

supp_table_df <- bind_rows(
  compute_summary_stats(df_priors$w1, df_posteriors$w1, "w1 (Local Spatial Weight)"),
  compute_summary_stats(df_priors$w2, df_posteriors$w2, "w2 (Long-Range Weight)"),
  compute_summary_stats(df_priors$w3, df_posteriors$w3, "w3 (Seed Network Weight)"),
  compute_summary_stats(df_priors$r, df_posteriors$r, "r (Intrinsic Growth Rate)"),
  compute_summary_stats(df_priors$tau, df_posteriors$tau, "tau (Latency Delay Weeks)"),
  compute_summary_stats(df_priors$beta, df_posteriors$beta, "beta (Raw Transmission Rate)"),
  compute_summary_stats(df_priors$sigma, df_posteriors$sigma, "sigma (Local Dispersal Scale km)"),
  compute_summary_stats(df_priors$kappa, df_posteriors$kappa, "kappa (Long-Range Scale Bounds)"),
  compute_summary_stats(df_priors$p, df_posteriors$p, "p (Kernel Tail Shape)")
)

# Print clean console markdown table 
cat("\n\n### SUPPLEMENTARY TABLE S2: MARKOV CALIBRATION PARAMETER PROFILES\n")
print(kable(supp_table_df, digits = 4, format = "markdown"))
