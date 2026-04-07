# ==========================================================
# TERNARY PLOT
# ==========================================================

plot_ternary <- function(posterior_df) {
  ggtern(data = posterior_df,
         aes(x = w1, y = w2, z = w3)) +
    geom_point(alpha = 0.4) +
    theme_bw() +
    labs(
      title = "Posterior distribution of transmission weights",
      T = "Local (Aphids)",
      L = "Long-range (Wind)",
      R = "Network (Farmers)"
    )
}