#=========================================
# Analysis phase 4
#=========================================

# ==============================================================================
# PHASE 4 IN-DEPTH ANALYSIS: PATHWAY MULTI-VARIATE REGRESSION & PCA
# ==============================================================================

cat("Assembling Phase 4 analytical matrix...\n")
df_analysis <- lapply(seq_along(phase4_results), function(i) {
  p <- phase4_results[[i]]
  
  if(!is.null(p$abc_vector) && !is.null(p$attribution)) {
    return(data.frame(
      Particle_ID  = i,
      # Bracket indexing for named atomic vectors
      prop_local   = p$attribution["prop_local"],
      prop_long    = p$attribution["prop_long"],
      prop_net     = p$attribution["prop_net"],
      
      prevalence   = p$abc_vector["prevalence"],
      moran_I      = p$abc_vector["moran_I"],
      ripley_K     = p$abc_vector["ripley_K"],
      velocity     = p$abc_vector["velocity"],
      stringsAsFactors = FALSE
    ))
  } else { return(NULL) }
}) %>% bind_rows()

# Clean data matrix and strip copied row names
df_clean <- df_analysis %>% filter(!is.na(velocity) & !is.na(moran_I))
rownames(df_clean) <- NULL

# ------------------------------------------------------------------------------
# REPLACEMENT ANALYSIS A: MULTI-VARIATE PATHWAY REGRESSION
# ------------------------------------------------------------------------------
cat("\n==================================================\n")
cat("  ANALYSIS A: MULTI-VARIATE PATHWAY REGRESSION     \n")
cat("==================================================\n")

# Combine your macro spatial targets into a multi-variate response matrix
Y_targets <- as.matrix(df_clean[, c("velocity", "prevalence", "moran_I", "ripley_K")])

# Run multiple regression against your active pathways 
# (We exclude prop_local because the simplex sums to 1, avoiding collinearity)
pathway_model <- lm(Y_targets ~ prop_net + prop_long, data = df_clean)

# Print the overall multi-variate summary
print(summary(pathway_model))

# ------------------------------------------------------------------------------
# ANALYSIS B: LANDSCAPE SPACE CHARACTERIZATION (TARGET PCA)
# ------------------------------------------------------------------------------
cat("\n==================================================\n")
cat("  ANALYSIS B: PRINCIPAL COMPONENT ANALYSIS (PCA)  \n")
cat("==================================================\n")

pca_targets <- FactoMineR::PCA(df_clean[, c("velocity", "prevalence", "moran_I", "ripley_K")], 
                               scale.unit = TRUE, graph = FALSE)

# Variance Profile
eigenvalues <- factoextra::get_eigenvalue(pca_targets)
cat("\n--- Variance Explained by Principal Components ---\n")
print(knitr::kable(eigenvalues, digits = 3, format = "markdown"))

# Loading Profile
var_coords <- factoextra::get_pca_var(pca_targets)
cat("\n--- Target Loading Matrix (Dimensions 1 & 2) ---\n")
loadings_table <- data.frame(
  Variable = rownames(var_coords$coord),
  Dim_1_Loading = var_coords$coord[,1],
  Dim_2_Loading = var_coords$coord[,2],
  Dim_1_Contrib = var_coords$contrib[,1],
  Dim_2_Contrib = var_coords$contrib[,2]
)
print(knitr::kable(loadings_table, digits = 3, format = "markdown"))

# Pearson Interaction Matrix
cat("\n--- Pathway vs. Target Pearson Correlations ---\n")
corr_matrix <- cor(df_clean[, c("velocity", "prevalence", "moran_I", "ripley_K", "prop_net", "prop_long")])
print(round(corr_matrix, 4))


# ==============================================================================
# SCIENCE FIGURE 1: TERNARY LANDSCAPE MATRIX
# ==============================================================================


# Generate high-end ternary landscape
fig1_ternary <- ggtern(data = df_clean, aes(x = prop_local, y = prop_long, z = prop_net)) +
  # Continuous density layer to highlight the parameter space density
  stat_density_tern(geom = 'polygon', aes(fill = ..level.., alpha = ..level..), show.legend = FALSE) +
  # Individual particles colored by continuous velocity wavefront metrics
  geom_point(aes(color = velocity), size = 1.2, alpha = 0.6) +
  scale_color_viridis_c(option = "plasma", name = "Wavefront Velocity\n(km/year)") +
  scale_fill_gradient(low = "transparent", high = "#2C3E50") +
  theme_rgbw() +
  labs(
    title = "Macro-Epidemiological Phase Space Core Selection",
    x = "Local Canopy (w1)",
    y = "Wind Drift (w2)",
    z = "Seed Trade (w3)"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right",
    tern.axis.title.L = element_text(vjust = 0.5),
    tern.axis.title.R = element_text(vjust = 0.5)
  )

fig1_ternary
# Save vector graphic for publication layout
#ggsave("figures/Figure1_Ternary_Landscape.pdf", plot = fig1_ternary, width = 7, height = 6, useDingbats = FALSE)

# ==============================================================================
# SCIENCE FIGURE 2: PATHOGEN FOOTPRINT VARIANCE BIPLOT
# ==============================================================================

fig2_pca <- fviz_pca_var(pca_targets,
                         col.var = "contrib", # Color by contributions to the components
                         gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                         repel = TRUE,        # Avoid text overlapping
                         ggtheme = theme_minimal()) +
  labs(
    title = "Spatial Footprint Trajectory Decomposition",
    x = "Dimension 1 (59.61% Variance Explained)",
    y = "Dimension 2 (22.06% Variance Explained)",
    color = "Target Variable\nContribution (%)"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.title = element_text(face = "bold"),
    legend.position = "right"
  )
fig2_pca
#ggsave("figures/Figure2_PCA_Decomposition.pdf", plot = fig2_pca, width = 6, height = 5, useDingbats = FALSE)

#========================================


# ==============================================================================
# FINAL SCALED VALIDATION RIDGE PLOT
# ==============================================================================

# 1. Define your real-world target metric
target_velocity <- 37.8 

# 2. Extract and scale your active, propagating particles
df_active_particles <- df_clean %>%
  # Filter out particles that failed to ignite an outbreak
  filter(velocity > 0.001) %>%
  # Scale raw spatial decimals to real-world kilometers per year
  mutate(velocity_km_year = velocity * 240.92)

# 3. Build the publication-ready vector plot
plot_2a <- ggplot(df_active_particles, aes(x = velocity_km_year)) + 
  # Shaded density profile representing your calibrated parameter space
  geom_density(fill = "#3498db", alpha = 0.5, color = "#2980b9", lwd = 1.1) +
  # Distinct dashed line pinpointing the empirical field target
  geom_vline(xintercept = target_velocity, linetype = "dashed", color = "#e74c3c", lwd = 1.2) +
  # Tighten coordinate space to focus strictly on your active parameter range
  xlim(0, 50) +
  theme_minimal(base_size = 12) +
  labs(
    x = "Wavefront Velocity (km/year)", 
    y = "Posterior Probability Density", 
    title = "Velocity Calibration Profile",
    subtitle = "Calibrated Phase 4 Particles vs. Savi et al. 2026 Empirical Target"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# Display the final plot directly in your RStudio panel
print(plot_2a)

# Save high-resolution vector PDF for manuscript assembly
#ggsave("figures/Validation_Ridge_2A.pdf", plot = plot_2a, width = 6, height = 4, useDingbats = FALSE)

# ------------------------------------------------------------------------------
# FIGURE 3: STRUCTURAL INDIFFERENCE VIOLINS
# ------------------------------------------------------------------------------
# Mocking three kernel categories across your 20k rows for layout demonstration
if(length(unique(df_clean$kernel_long)) < 2) {
  df_clean$kernel_long <- sample(c("powerlaw", "cauchy", "levy"), nrow(df_clean), replace = TRUE)
}

v_panel_A <- ggplot(df_clean, aes(x = kernel_long, y = velocity, fill = kernel_long)) +
  geom_violin(alpha = 0.7, color = "#2c3e50", draw_quantiles = c(0.25, 0.5, 0.75)) +
  scale_fill_brewer(palette = "Pastel1") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none", axis.title.x = element_blank()) +
  labs(y = "Velocity (km/year)", title = "Macroecological Invariance Profiles")

v_panel_B <- ggplot(df_clean, aes(x = kernel_long, y = moran_I, fill = kernel_long)) +
  geom_violin(alpha = 0.7, color = "#2c3e50", draw_quantiles = c(0.25, 0.5, 0.75)) +
  scale_fill_brewer(palette = "Pastel1") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none") +
  labs(x = "Assigned Long-Range Dispersal Kernel Geometry", y = "Spatial Clustering (Moran's I)")

fig3_violins <- v_panel_A / v_panel_B

# ==============================================================================
# SCIENCE FIGURE 3: LOCAL KERNEL STRUCTURAL INVARIANCE VIOLINS
# ==============================================================================

# --- PRE-PLOT DATA CLEANING ---
# Filter out failed runs to focus entirely on active epidemiological space
df_violin_active <- df_clean %>%
  filter(velocity > 0.001) %>%
  mutate(velocity_km_year = velocity * 240.92)

# NOTE: If your current dataset only contains "gaussian", this line safely 
# splits your data 50/50 so you can preview exactly what the final figure 
# will look like when your multi-kernel simulation finishes.
if(length(unique(df_violin_active$kernel_local)) < 2) {
  cat("Simulating Exponential factor levels for visual layout preview...\n")
  set.seed(42)
  df_violin_active$kernel_local <- sample(c("Gaussian", "Exponential"), 
                                          nrow(df_violin_active), replace = TRUE)
}

# ------------------------------------------------------------------------------
# PANEL A: VELOCITY INVARIANCE
# ------------------------------------------------------------------------------
panel_A <- ggplot(df_violin_active, aes(x = kernel_local, y = velocity_km_year, fill = kernel_local)) +
  geom_violin(alpha = 0.60, color = "#2c3e50", draw_quantiles = c(0.25, 0.5, 0.75), lwd = 0.8) +
  scale_fill_manual(values = c("Gaussian" = "#34495e", "Exponential" = "#16a085")) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(face = "bold")
  ) +
  labs(y = "Wavefront Velocity (km/year)", title = "Macroecological Invariance Profiles")

# ------------------------------------------------------------------------------
# PANEL B: MORAN'S I (SPATIAL PATCHINESS) INVARIANCE
# ------------------------------------------------------------------------------
panel_B <- ggplot(df_violin_active, aes(x = kernel_local, y = moran_I, fill = kernel_local)) +
  geom_violin(alpha = 0.60, color = "#2c3e50", draw_quantiles = c(0.25, 0.5, 0.75), lwd = 0.8) +
  scale_fill_manual(values = c("Gaussian" = "#34495e", "Exponential" = "#16a085")) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(face = "bold")
  ) +
  labs(x = "Assigned Local Dispersal Kernel Geometry", y = "Spatial Clustering (Moran's I)")

# --- COMBINE AND ALIGN PANELS FOR THE MANUSCRIPT ---
fig3_violins <- panel_A / panel_B

# Render immediately to your active R plot viewer
print(fig3_violins)

# Save as a pristine vector PDF for publication layout
#ggsave("figures/Figure3_Kernel_Invariance_Violins.pdf", plot = fig3_violins, width = 5, height = 7, useDingbats = FALSE)


# ==============================================================================
# PHASE 4: STRUCTURAL PATH COMPARISON (GAUSSIAN VS. EXPONENTIAL VIZ & STATS)
# ==============================================================================

# 1. Build and clean the comparative data frame
df_compare <- lapply(seq_along(phase4_results), function(i) {
  p <- phase4_results[[i]]
  if(!is.null(p$abc_vector) && !is.null(p$attribution) && !is.null(p$metadata)) {
    data.frame(
      Particle_ID  = i,
      kernel_local = as.factor(p$metadata$kernel_local),
      kernel_long  = as.factor(p$metadata$kernel_long),
      prop_net     = as.numeric(p$attribution["prop_net"]),
      prop_long    = as.numeric(p$attribution["prop_long"]),
      prevalence   = as.numeric(p$abc_vector["prevalence"]),
      moran_I      = as.numeric(p$abc_vector["moran_I"]),
      velocity     = as.numeric(p$abc_vector["velocity"]) * 240.92, # Scale to km/year
      stringsAsFactors = FALSE
    )
  }
}) %>% bind_rows() %>% filter(velocity > 0.001 & !is.na(moran_I))

# ------------------------------------------------------------------------------
# STEP 2: THE MULTIVARIATE TOURNAMENT (TYPE III MANOVA)
# ------------------------------------------------------------------------------
cat("--- MULTIVARIATE ANALYSIS OF VARIANCE (MANOVA) INDEPTH ---\n")
Y_matrix <- as.matrix(df_compare[, c("velocity", "prevalence", "moran_I")])

# Fit full interaction model with continuous attribution covariates
fit_manova <- lm(Y_matrix ~ kernel_local * kernel_long + prop_net + prop_long, data = df_compare)
manova_results <- car::Anova(fit_manova, type = "III")
print(manova_results)

# Calculate Partial Eta-Squared to prove structural indifference magnitude
cat("\n--- EFFECT SIZE DECOMPOSITION (PARTIAL ETA-SQUARED) ---\n")
print(heplots::etasq(fit_manova, type = 3))

# ------------------------------------------------------------------------------
# STEP 3: VISUALIZING STRUCTURAL PATH INSIGNIFICANCE VS NETWORK DOMINANCE
# ------------------------------------------------------------------------------
# Panel A: Local Kernel Comparison (Velocity)
comp_v1 <- ggplot(df_compare, aes(x = kernel_local, y = velocity, fill = kernel_local)) +
  geom_violin(alpha = 0.6, color = "#2c3e50", draw_quantiles = 0.5, lwd = 0.8) +
  scale_fill_manual(values = c("gaussian" = "#2c3e50", "exponential" = "#16a085")) +
  theme_minimal(base_size = 11) + 
  theme(legend.position = "none", axis.title.x = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Wavefront Velocity (km/year)", title = "A: Local Scale Path Profile")

# Panel B: Long Range Kernel Comparison (Spatial Aggregation)
comp_v2 <- ggplot(df_compare, aes(x = kernel_long, y = moran_I, fill = kernel_long)) +
  geom_violin(alpha = 0.6, color = "#2c3e50", draw_quantiles = 0.5, lwd = 0.8) +
  scale_fill_brewer(palette = "Dark2") +
  theme_minimal(base_size = 11) + 
  theme(legend.position = "none", panel.grid.minor = element_blank()) +
  labs(x = "Assigned Dispersal Kernel Path", y = "Spatial Aggregation (Moran's I)", title = "B: Macro Dispersal Path Profile")

fig_comparison <- comp_v1 / comp_v2
ggsave("figures/Phase4_Structural_Path_Comparison.pdf", plot = fig_comparison, width = 6, height = 7, useDingbats = FALSE)
print(fig_comparison)