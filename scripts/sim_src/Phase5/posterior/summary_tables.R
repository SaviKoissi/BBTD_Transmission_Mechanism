# ==========================================================
# SUMMARY TABLES
# ==========================================================

compute_summary_table <- function(posterior_df) {
  
  summary <- data.frame(
    parameter = names(posterior_df),
    
    mean = sapply(posterior_df, mean, na.rm = TRUE),
    median = sapply(posterior_df, median, na.rm = TRUE),
    sd = sapply(posterior_df, sd, na.rm = TRUE),
    
    q2.5 = sapply(posterior_df, function(x)
      quantile(x, 0.025, na.rm = TRUE)),
    
    q97.5 = sapply(posterior_df, function(x)
      quantile(x, 0.975, na.rm = TRUE))
  )
  
  return(summary)
}