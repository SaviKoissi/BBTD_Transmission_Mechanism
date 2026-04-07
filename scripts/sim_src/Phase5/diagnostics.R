# ==========================================================
# DIAGNOSTICS
# ==========================================================

plot_distance_distribution <- function(distances) {
  hist(distances, breaks = 30, main = "Distance distribution")
}