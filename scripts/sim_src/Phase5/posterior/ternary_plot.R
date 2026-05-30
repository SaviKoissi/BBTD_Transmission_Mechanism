# ==========================================================
# TERNARY PLOT
# ==========================================================

plot_ternary_weights <- function(posterior_df) {
  # Force clean evaluation as a standard data frame
  plot_data <- as.data.frame(posterior_df)
  
  # Ensure your column assignments match ggtern's vertex mapping:
  # L = Left Vertex, T = Top Vertex, R = Right Vertex
  # mapping x=L, y=T, z=R binds them perfectly to the triangular axes
  p <- ggtern(data = plot_data, aes(x = prop_local, y = prop_long, z = prop_net)) +
    
    # 1. Geometry: Use alpha-blended direct points instead of an unstable density smooth.
    # This prevents boundary calculations from dropping edge data (bdl errors).
    geom_point(
      aes(color = prop_net), 
      alpha = 0.3, 
      size = 1.8,
      stroke = 0,
      position = "jitter" # Gentle jittering unmasks points stacked on the 1.0 wall
    ) +
    
    # 2. Add an explicit crosshair or indicator at the true Bayesian Median
    # to guide the reviewer's eye directly to the convergence point.
    annotate(
      geom = "point", 
      x = 0.0029, y = 0.0028, z = 0.9931, 
      color = "#d35400", 
      shape = 21, 
      fill = "#f39c12", 
      size = 5, 
      stroke = 1.5
    ) +
    
    # 3. Apply a publication-ready color palette that highlights network dominance
    scale_color_viridis_c(
      option = "plasma", 
      name = "Network Allocation\n(prop_net)",
      limits = c(0.8, 1.0) # Focuses color contrast right at the cluster zone
    ) +
    
    # 4. Strict, clear vertex labeling matching PNAS/Science editorial standards
    labs(
      x = "Local Aphid\n(prop_local)",
      y = "Wind Drift\n(prop_long)",
      z = "Seed Network\n(prop_net)",
      title = "Bayesian Posterior Simplex Space Convergence",
      subtitle = "Empirical BBTV Attractor Basin Concentrated at Network Vertex"
    ) +
    
    # 5. Professional minimal theme setup
    theme_rgbw() +
    theme(
      plot.title         = element_text(face = "bold", size = 13, color = "#2c3e50"),
      plot.subtitle      = element_text(size = 10, color = "#7f8c8d"),
      tern.axis.arrow    = element_line(color = "#34495e", lwd = 1),
      tern.axis.text     = element_text(size = 9, face = "bold"),
      tern.axis.title    = element_text(size = 10, face = "bold"),
      legend.position    = "right",
      legend.title       = element_text(size = 9, face = "bold"),
      panel.grid.major   = element_line(color = "#e2e8f0", lty = "dotted")
    )
  
  return(p)
}