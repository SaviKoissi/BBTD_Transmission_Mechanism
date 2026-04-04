# ==========================================================
# BIAS-ADJUSTED PREVALENCE (BAYESIAN MISCLASSIFICATION MODEL)
# ==========================================================

source("R/libraries.R")


# ==========================================================
# PREPARE DATA
# ==========================================================
prepare_stan_data <- function(data) {
  
  df <- data %>%
    st_drop_geometry() %>%
    mutate(
      y = as.integer(NumbDiseasedPlant_Farm),
      n = as.integer(TotalPlant_Farm),
      # 'field' matches the Stan data block variable name
      field_idx = as.integer(as.factor(Unique_Field_ID)) 
    )
  
  list(
    N = nrow(df),
    # 'K' matches the Stan data block variable name
    K = length(unique(df$field_idx)), 
    y = df$y,
    n = df$n,
    # 'field' matches the Stan data block variable name
    field = df$field_idx 
  )
}

# ==========================================================
# PRIOR CALIBRATION (Se / Sp)
# ==========================================================
beta_from_quantiles <- function(q_lo, q_hi, p_lo = 0.025, p_hi = 0.975){
  fn <- function(par){
    a <- par[1]; b <- par[2]
    if(a <= 0 || b <= 0) return(1e6)
    (qbeta(p_lo, a, b) - q_lo)^2 + (qbeta(p_hi, a, b) - q_hi)^2
  }
  opt <- optim(c(2, 2), fn, method = "L-BFGS-B", lower = c(1e-6, 1e-6))
  list(alpha = opt$par[1], beta = opt$par[2])
}

# Priors
Se_beta <- beta_from_quantiles(0.40, 0.90)
Sp_beta <- beta_from_quantiles(0.97, 0.999)
# ==========================================================
# BUILD STAN DATA
# ==========================================================
build_data <- function(data) {
  
  stan_base <- prepare_stan_data(data)
  
  stan_base$Se_a <- Se_beta$alpha
  stan_base$Se_b <- Se_beta$beta
  stan_base$Sp_a <- Sp_beta$alpha
  stan_base$Sp_b <- Sp_beta$beta
  
  return(stan_base)
}

# ==========================================================
# RUN MODEL
# ==========================================================
run_model <- function(data) {
  
  stan_data <- build_data(data)
  
  mod <- cmdstan_model(config$model$stan_file)
  
  fit <- mod$sample(
    data = stan_data,
    seed = config$model$seed,
    chains = config$model$chains,
    parallel_chains = config$model$chains,
    iter_warmup = config$model$iter_warmup,
    iter_sampling = config$model$iter_sampling
  )
  
  return(list(fit = fit, data = data))
}

# ==========================================================
# POSTERIOR EXTRACTION
# ==========================================================

extract_prevalence <- function(fit_obj, original_sf_data) {
  
  fit <- fit_obj$fit
  
  # 1. Extract the 'field_prev' parameter from Stan
  # These are the true prevalence estimates for each unique field
  draws <- fit$draws("field_prev", format = "df")
  
  # 2. Summarize the posterior for each field index
  posterior_summary <- draws %>%
    pivot_longer(cols = starts_with("field_prev"),
                 names_to = "field_idx",
                 values_to = "prev") %>%
    mutate(field_idx = as.integer(gsub("field_prev\\[|\\]", "", field_idx))) %>%
    group_by(field_idx) %>%
    summarise(
      true_prev_median = median(prev),
      true_prev_lower  = quantile(prev, 0.025),
      true_prev_upper  = quantile(prev, 0.975),
      .groups = "drop"
    )
  
  # 3. Create a mapping key from the original data
  # This must match exactly how 'field' was created in prepare_stan_data
  mapping_key <- original_sf_data %>%
    st_drop_geometry() %>%
    mutate(field_idx = as.integer(as.factor(Unique_Field_ID))) %>%
    select(Unique_Field_ID, field_idx) %>%
    distinct()
  
  # 4. Join the summary to the mapping key
  posterior_results <- posterior_summary %>%
    left_join(mapping_key, by = "field_idx") %>%
    select(-field_idx) # Remove index, keep Unique_Field_ID
  
  # 5. Merge back into the original SF object
  final_data <- original_sf_data %>%
    left_join(posterior_results, by = "Unique_Field_ID") %>%
    mutate(
      # Optional: Create a column for the bias (Difference)
      bias_correction = true_prev_median - (NumbDiseasedPlant_Farm / TotalPlant_Farm)
    )
  final_data <- final_data %>%
    mutate(
      true_prev_pct = true_prev_median * 100,
      obs_prev_pct = (NumbDiseasedPlant_Farm / TotalPlant_Farm) * 100
    )
  
  return(final_data)
}

# data_fullData = your merged dataset
fit_obj <- run_model(data_fullData)

posterior_prev <- extract_prevalence(fit_obj, data_fullData)
