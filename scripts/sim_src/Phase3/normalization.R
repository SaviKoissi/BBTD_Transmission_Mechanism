# ==========================================================
# NORMALIZATION
# ==========================================================

normalize_kernel_matrix <- function(K) {
  # Calculate total pressure exerted by each source farm
  row_sums <- rowSums(K, na.rm = TRUE)
  
  # Avoid division by zero for isolated farms
  row_sums[row_sums == 0] <- 1
  
  # Standardize so each farm distributes exactly '1' unit of risk
  K_norm <- K / row_sums
  
  return(K_norm)
}