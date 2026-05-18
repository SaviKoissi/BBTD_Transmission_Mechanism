# ==========================================================
# SUMMARY TABLES
# ==========================================================

# compute_summary_table <- function(posterior_df) {
#   
#   summary <- data.frame(
#     parameter = names(posterior_df),
#     
#     mean = sapply(posterior_df, mean, na.rm = TRUE),
#     median = sapply(posterior_df, median, na.rm = TRUE),
#     sd = sapply(posterior_df, sd, na.rm = TRUE),
#     
#     q2.5 = sapply(posterior_df, function(x)
#       quantile(x, 0.025, na.rm = TRUE)),
#     
#     q97.5 = sapply(posterior_df, function(x)
#       quantile(x, 0.975, na.rm = TRUE))
#   )
#   
#   return(summary)
# }

compute_summary_table <- function(posterior_df) {
  # Coerce to a strict data frame
  posterior_df <- as.data.frame(posterior_df)
  
  # Identify only numeric parameters to summarize
  numeric_cols <- sapply(posterior_df, is.numeric)
  valid_df <- posterior_df[, numeric_cols, drop = FALSE]
  
  # Compute metrics column by column safely
  summary_list <- lapply(names(valid_df), function(col_name) {
    col_data <- valid_df[[col_name]]
    
    # Calculate quantiles safely
    quants <- quantile(col_data, probs = c(0.025, 0.5, 0.975), na.rm = TRUE)
    
    # Return as a explicit 1-row data frame
    data.frame(
      parameter = col_name,
      mean      = mean(col_data, na.rm = TRUE),
      p2.5      = quants[1],
      median    = quants[2],
      p97.5     = quants[3],
      sd        = sd(col_data, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })
  
  # Bind rows vertically (avoids row-mismatch column-binding bugs entirely)
  summary_table <- do.call(rbind, summary_list)
  rownames(summary_table) <- NULL
  
  return(summary_table)
}