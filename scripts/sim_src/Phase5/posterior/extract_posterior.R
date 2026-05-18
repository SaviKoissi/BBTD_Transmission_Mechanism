# ==========================================================
# EXTRACT POSTERIOR SAMPLES
# ==========================================================

# extract_posterior <- function(accepted_idx, phase2_obj, phase4_results) {
#   # 1. Extract parameters from the Phase 2 design
#   params_df <- phase2_obj$particles_dataframe[accepted_idx, ]
#   
#   # 2. Extract attribution (Question 3 & 4) from Phase 4
#   # We use [[ ]] to ensure we are accessing the list elements correctly
#   attr_list <- lapply(phase4_results[accepted_idx], function(res) {
#     if (is.list(res)) return(res$attribution)
#     return(NULL) # Return NULL if attribution wasn't saved
#   })
#   
#   # Filter out NULLs and bind
#   attr_df <- do.call(rbind, attr_list)
#   
#   if (!is.null(attr_df)) {
#     return(cbind(params_df, attr_df))
#   } else {
#     return(params_df)
#   }
# }

extract_posterior <- function(accepted_idx, phase2_obj, phase4_results) {
  # 1. Extract parameters from the Phase 2 design
  params_df <- phase2_obj$particles_dataframe[accepted_idx, ]
  
  # 2. Extract attribution from Phase 4
  attr_list <- lapply(phase4_results[accepted_idx], function(res) {
    if (is.list(res)) return(res$attribution)
    return(NULL) 
  })
  
  # Filter out NULLs and bind
  attr_df <- do.call(rbind, attr_list)
  
  if (!is.null(attr_df)) {
    # DEFENSIVE GUARD: Force names onto the bound matrix rows/columns
    attr_df <- as.data.frame(attr_df)
    colnames(attr_df) <- c("prop_local", "prop_long", "prop_net")
    
    # Reset row names to avoid index matching clutter
    rownames(attr_df) <- NULL
    rownames(params_df) <- NULL
    
    return(cbind(params_df, attr_df))
  } else {
    return(params_df)
  }
}