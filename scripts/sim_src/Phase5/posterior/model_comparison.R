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
# 
identify_dominant_mechanism <- function(posterior_df) {
  # We look at the actual realized transmission from Phase 4
  realized <- colMeans(posterior_df[, c("prop_local", "prop_long", "prop_net")])
  
  # We also look at the inferred weights from Phase 2/3
  weights <- colMeans(posterior_df[, c("w1", "w2", "w3")])
  
  return(list(
    realized_prevalence = realized,
    inferred_weights = weights,
    conclusion = paste("Human network explains", round(realized["prop_net"]*100), "% of infections.")
  ))
}