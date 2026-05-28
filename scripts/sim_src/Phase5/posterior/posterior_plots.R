# ==========================================================
# POSTERIOR DISTRIBUTIONS
# ==========================================================

plot_posterior_weights <- function(posterior_df) {
  # Coerce to plain dataframe
  df_clean <- as.data.frame(posterior_df)
  
  req_cols <- c("prop_local", "prop_long", "prop_net")
  if (!all(req_cols %in% colnames(df_clean))) {
    stop("Input data is missing required pathway columns.")
  }
  
  # Structural isolation: extract variables cleanly using base matrix mappings 
  # to completely bypass the tidyverse zero-variance filtering bugs
  extracted_df <- data.frame(
    Local = as.numeric(df_clean[["prop_local"]]),
    Long  = as.numeric(df_clean[["prop_long"]]),
    Net   = as.numeric(df_clean[["prop_net"]])
  )
  
  # Explicitly pivot long without throwing filtering exceptions over 0 values
  long_df <- data.frame(
    pathway = rep(c("Local (Aphids)", "Long-range (Wind)", 
                    "Network (Farmers)"), each = nrow(extracted_df)),
    weight  = c(extracted_df$Local, extracted_df$Long, extracted_df$Net)
  )
  
  # Drop genuine NAs, keeping true zero values intact
  long_df <- long_df[!is.na(long_df$weight), ]
  
  p <- ggplot(long_df, aes(x = weight, fill = pathway)) +
    geom_histogram(aes(y = after_stat(count) / sum(after_stat(count))),
                   bins = 30, alpha = 0.75, color = "white", boundary = 0) +
    facet_wrap(~pathway, scales = "free_y") +
    scale_fill_viridis_d(option = "mako", begin = 0.2, end = 0.7) +
    scale_x_continuous(limits = c(-0.02, 1.02), breaks = seq(0, 1, 0.2)) +
    theme_minimal(base_size = 11, base_family = "sans") +
    theme(
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "#f4f4f5", color = NA),
      strip.text = element_text(face = "bold", color = "#27272a"),
      legend.position = "none"
    ) +
    labs(x = "Posterior Attribution Weight", y = "Relative Frequency")
  
  return(p)
}