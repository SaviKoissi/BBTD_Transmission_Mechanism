# ==========================================================
# TERNARY PLOT
# ==========================================================

# plot_ternary_weights <- function(posterior_df) {
#   # Standardize column mapping
#   plot_data <- as.data.frame(posterior_df)
#   
#   p <- ggtern(data = plot_data, aes(x = prop_local, y = prop_long, z = prop_net)) +
#     # 1. Force polygon rendering with fill mapped to density levels
#     stat_density_tern(
#       aes(fill = after_stat(level)),
#       geom     = "polygon",
#       bins     = 25,
#       color    = NA,          # Removes brittle wireframe border lines
#       alpha    = 0.85
#     ) +
#     # 2. Apply a clean continuous color ramp
#     scale_fill_viridis_c(option = "mako", name = "Posterior\nDensity") +
#     # 3. Label vertex components matching your study pathways
#     labs(
#       T = "Long-range (Wind)",
#       L = "Local (Aphids)",
#       R = "Network (Farmers)"
#     ) +
#     theme_rgbw() +            # Robust, clean white baseline theme for ternary projections
#     theme(
#       tern.panel.background = element_rect(fill = "#f8f9fa", color = NA),
#       legend.position       = "right"
#     )
#   
#   return(p)
# }

plot_ternary_weights <- function(posterior_df) {
  plot_data <- as.data.frame(posterior_df)
  
  p <- ggtern(data = plot_data, aes(x = prop_local, y = prop_long, z = prop_net)) +
    # Use geom_point with alpha and color mapped directly to density to guarantee bright visuals
    stat_density_tern(
      aes(alpha = after_stat(level), color = after_stat(level)),
      geom = "point",
      bdl = "MESSY", # Tells ggtern to safely handle points sitting on the 0 boundary
      bdl.val = 0.001,
      size = 2.5
    ) +
    scale_color_viridis_c(option = "mako", name = "Posterior\nDensity") +
    scale_alpha_continuous(range = c(0.4, 1.0), guide = "none") +
    labs(
      T = "Long-range (Wind)",
      L = "Local (Aphids)",
      R = "Network (Farmers)"
    ) +
    theme_rgbw() +
    theme(
      tern.panel.background = element_rect(fill = "#f8f9fa", color = NA),
      legend.position       = "right"
    )
  
  return(p)
}