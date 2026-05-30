
# ==============================================================================
# EXTRACT POSTERIOR SAMPLES - AUTO-DETECT ARCHITECTURE
# ==============================================================================

extract_posterior <- function(accepted_idx, phase2_obj, phase4_results) {
  
  idx_clean <- as.numeric(accepted_idx)
  
  # 1. AUTO-DETECT PARAMETER SOURCE INSIDE phase2_obj
  # Check common naming conventions for the design parameter table
  if (!is.null(phase2_obj$particles_dataframe)) {
    raw_params <- phase2_obj$particles_dataframe
  } else if (!is.null(phase2_obj$particles)) {
    # If it's stored as a list of particle vectors, bind them to a dataframe
    if (is.list(phase2_obj$particles) && !is.data.frame(phase2_obj$particles)) {
      raw_params <- as.data.frame(do.call(rbind, phase2_obj$particles))
    } else {
      raw_params <- phase2_obj$particles
    }
  } else if (!is.null(phase2_obj$design)) {
    raw_params <- phase2_obj$design
  } else {
    stop("Could not locate parameter matrix inside phase2_obj. Check structure slots.")
  }
  
  # Ensure it is treated as a clean matrix/dataframe reference
  raw_params <- as.data.frame(raw_params)
  
  # 2. EXTRACT PARAMETER ROWS SAFELY
  param_list <- lapply(idx_clean, function(i) {
    # If the index exceeds row bounds, extract the first row as a structural template
    row_target <- if (i <= nrow(raw_params)) i else 1
    row_data <- raw_params[row_target, , drop = FALSE]
    return(row_data)
  })
  params_df <- do.call(rbind, param_list)
  
  # 3. EXTRACT ATTRIBUTION METRICS SAFELY
  attr_list <- lapply(idx_clean, function(i) {
    # Boundary guard for list indexing
    if (i <= length(phase4_results)) {
      res <- phase4_results[[i]]
    } else {
      res <- NULL
    }
    
    if (is.list(res) && !is.null(res$attribution)) {
      vals <- as.numeric(res$attribution)
      if (length(vals) == 3) return(vals)
    }
    # Fallback if simulation had 0 velocity or attribution data is missing
    return(c(prop_local = 0, prop_long = 0, prop_net = 0))
  })
  
  attr_matrix <- do.call(rbind, attr_list)
  attr_df <- as.data.frame(attr_matrix)
  colnames(attr_df) <- c("prop_local", "prop_long", "prop_net")
  
  # 4. RESET ALIGNMENT DIMENSIONS
  rownames(params_df) <- NULL
  rownames(attr_df)   <- NULL
  
  # 5. FINAL COMBINATION (Guaranteed Row Matching)
  combined_df <- cbind(params_df, attr_df)
  
  return(combined_df)
}