# ==========================================================
# POSTERIOR DISTRIBUTIONS
# ==========================================================

plot_posterior_weights <- function(posterior_df) {
  
  par(mfrow = c(1, 3))
  
  hist(posterior_df$w1, main = "w1 (Local)", xlab = "Weight")
  hist(posterior_df$w2, main = "w2 (Long-range)", xlab = "Weight")
  hist(posterior_df$w3, main = "w3 (Network)", xlab = "Weight")
}