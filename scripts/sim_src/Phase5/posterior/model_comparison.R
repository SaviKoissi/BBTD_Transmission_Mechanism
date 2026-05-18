# ==========================================================
# MODEL COMPARISON
# ==========================================================

compare_models <- function(all_model_results, S_obs, epsilon) {
  # all_model_results should be a list of Phase 4 outputs
  # e.g., list(Gaussian = res_gauss, Exponential = res_exp)
  
  comp <- lapply(names(all_model_results), function(m_name) {
    dists <- sapply(all_model_results[[m_name]], function(x) compute_distance(x, S_obs))
    n_acc <- sum(dists < epsilon)
    
    data.frame(
      model = m_name,
      n_accepted = n_acc,
      p_acc = n_acc / length(dists)
    )
  })
  
  df <- do.call(rbind, comp)
  # Bayes Factor (approximate) = Ratio of acceptance probabilities
  df$BayesFactor_vs_Best <- max(df$p_acc) / df$p_acc
  return(df)
}
# ==========================================================
# DOMINANT MECHANISM
# ==========================================================

# identify_dominant_mechanism <- function(posterior_df) {
#   
#   means <- colMeans(posterior_df[, c("w1", "w2", "w3")], na.rm = TRUE)
#   
#   dominant <- names(which.max(means))
#   
#   interpretation <- switch(dominant,
#                            w1 = "Local vector-driven transmission dominates (aphids)",
#                            w2 = "Long-range dispersal dominates (wind)",
#                            w3 = "Network-mediated spread dominates (human activity)"
#   )
#   
#   return(list(
#     dominant_weight = dominant,
#     interpretation = interpretation,
#     means = means
#   ))
# }
  # This section might contain some bugs 
# identify_dominant_mechanism <- function(posterior_df) {
#   # We look at the actual realized transmission from Phase 4
#   realized <- colMeans(posterior_df[, c("prop_local", "prop_long", "prop_net")])
#   
#   # We also look at the inferred weights from Phase 2/3
#   weights <- colMeans(posterior_df[, c("w1", "w2", "w3")])
#   
#   return(list(
#     realized_prevalence = realized,
#     inferred_weights = weights,
#     conclusion = paste("Human network explains", round(realized["prop_net"]*100), "% of infections.")
#   ))
# }

identify_dominant_mechanism <- function(posterior_df) {
  posterior_df <- as.data.frame(posterior_df)
  
  # 1. Calculate the overall average for the attribution pathways safely
  pathways <- c("prop_local", "prop_long", "prop_net")
  
  # Ensure these columns exist to prevent a separate crash
  if (!all(pathways %in% colnames(posterior_df))) {
    stop("Attribution columns missing from posterior_df. Check extract_posterior.R")
  }
  
  mean_attributions <- colMeans(posterior_df[, pathways], na.rm = TRUE)
  
  # 2. Determine dominance based on the highest overall mean pathway
  dominant_channel <- pathways[which.max(mean_attributions)]
  
  # 3. Calculate proportions of runs where each channel was dominant
  # Find which channel was max row-by-row
  row_max_idx <- max.col(posterior_df[, pathways], ties.method = "first")
  row_winners <- pathways[row_max_idx]
  
  prop_local_dom <- sum(row_winners == "prop_local") / nrow(posterior_df)
  prop_long_dom  <- sum(row_winners == "prop_long") / nrow(posterior_df)
  prop_net_dom   <- sum(row_winners == "prop_net") / nrow(posterior_df)
  
  # 4. Construct the output dataframe safely by specifying scalars/matched lengths
  dominance_summary <- data.frame(
    mechanism = c("prop_local", "prop_long", "prop_net"),
    mean_attribution = as.numeric(mean_attributions),
    proportion_sims_dominant = c(prop_local_dom, prop_long_dom, prop_net_dom),
    overall_winner = dominant_channel,
    stringsAsFactors = FALSE
  )
  
  return(dominance_summary)
}