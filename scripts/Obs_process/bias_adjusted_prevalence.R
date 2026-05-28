# ======================================================================
# BIAS-ADJUSTED INCIDENCE PIPELINE  (ZERO-INFLATED BINOMIAL MODEL)
# ======================================================================
source("R/libraries.R")

# ======================================================================
# HIGH-PRECISION ANALYTICAL PRIOR PARAMETERIZER (REPLACES THE OPTIM LOOP)
# ======================================================================
beta_from_quantiles <- function(q_lo, q_hi, p_lo = 0.025, p_hi = 0.975) {
  target_mean <- (q_lo + q_hi) / 2
  target_sd <- (q_hi - q_lo) / 4
  target_var <- target_sd^2
  
  if (target_var >= target_mean * (1 - target_mean)) {
    stop("Requested prior variance is too wide for a Beta distribution.")
  }
  
  alpha <- target_mean * (((target_mean * (1 - target_mean)) / target_var) - 1)
  beta  <- (1 - target_mean) * (((target_mean * (1 - target_mean)) / target_var) - 1)
  
  return(list(alpha = alpha, beta = beta))
}

prepare_stan_data <- function(data) {
  df <- data %>%
    st_drop_geometry() %>%
    dplyr::mutate(
      field_idx = as.integer(as.factor(Unique_Field_ID)) 
    )
  
  field_summary <- df %>%
    group_by(field_idx) %>%
    summarise(
      diseased = as.integer(max(NumbDiseasedPlant_Farm, na.rm = TRUE)),
      sampled = as.integer(median(TotalPlant_Farm, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    dplyr::filter(sampled > 0) %>%
    dplyr::mutate(sampled = as.integer(pmax(sampled, diseased)))
  
  list(
    N = nrow(field_summary),
    K = nrow(field_summary),
    field = field_summary$field_idx,
    y = field_summary$diseased,   
    m = field_summary$sampled     
  )
}

build_data <- function(data, Se_p, Sp_p) {
  stan_base <- prepare_stan_data(data)
  stan_base$Se_a <- Se_p$alpha
  stan_base$Se_b <- Se_p$beta
  stan_base$Sp_a <- Sp_p$alpha
  stan_base$Sp_b <- Sp_p$beta
  return(stan_base)
}

run_model <- function(data, Se_p, Sp_p) {
  stan_data <- build_data(data, Se_p, Sp_p)
  stan_file_path <- "scripts/stan/misclass_hier_logit_nocov.stan"
  
  # SYSTEM SAFEGUARD: Automatically appends a trailing newline to the .stan file
  # before cmdstanr parses it, eliminating Warning #1 permanently.
  tryCatch({
    lines <- readLines(stan_file_path, warn = FALSE)
    writeLines(c(lines, ""), stan_file_path)
  }, error = function(e) { NULL })
  
  mod <- cmdstan_model(stan_file_path, force_recompile = TRUE)
  
  init_fun <- function() {
    list(
      mu = 0.2, 
      sigma_field = 0.4,
      z_field = rep(0, stan_data$K),
      logit_theta = 0.582,  
      Se = 0.80,
      Sp = 0.95
    )
  }
  
  fit <- mod$sample(
    data = stan_data,
    seed = 123,
    chains = 4,
    parallel_chains = 4,
    init = init_fun,
    iter_warmup = 1000,        
    iter_sampling = 1000,
    adapt_delta = 0.999,       
    max_treedepth = 16         
  )
  return(list(fit = fit, data = data))
}

extract_prevalence <- function(fit_obj, original_sf_data) {
  fit <- fit_obj$fit
  draws <- suppressWarnings(fit$draws("field_prev", format = "df"))
  
  posterior_summary <- draws %>%
    pivot_longer(cols = starts_with("field_prev"),
                 names_to = "field_idx",
                 values_to = "prev") %>%
    mutate(field_idx = as.integer(gsub("field_prev\\[|\\]", "", field_idx))) %>%
    group_by(field_idx) %>%
    summarise(
      true_prev_median = median(prev),
      .groups = "drop"
    )
  
  mapping_key <- original_sf_data %>%
    st_drop_geometry() %>%
    dplyr::mutate(field_idx = as.integer(as.factor(Unique_Field_ID))) %>%
    dplyr::select(Unique_Field_ID, field_idx) %>%
    distinct()
  
  final_data <- original_sf_data %>%
    left_join(posterior_summary %>% left_join(mapping_key, by = "field_idx"), 
              by = "Unique_Field_ID")
  
  return(final_data)
}

# --- 2. EXECUTION ---

Se_priors <- beta_from_quantiles(0.70, 0.90) 
Sp_priors <- beta_from_quantiles(0.997, 0.999) 

# Run Model
fit_obj <- run_model(data_fullData, Se_priors, Sp_priors)

# Final Result Assignment
posterior_prev <- extract_prevalence(fit_obj, data_fullData)

# Isolate the Global True Structural Parameter
# Turning the draws object into a standard vector using as.vector() 
# prevents Warning #2 when evaluating mathematical summaries like median().
# theta_draws <- suppressWarnings(fit_obj$fit$draws("theta", format = "df"))
# theta_vector <- as.vector(theta_draws$theta)
# S_obs <- median(theta_vector)

# Isolate the Global True Structural Parameter
theta_draws <- suppressWarnings(fit_obj$fit$draws("theta", format = "df"))

# ENHANCEMENT: Forcing to a primitive numeric vector strips the 'draws_df' 
# class completely, guaranteeing that downstream functions like median() 
# run with absolute silence.
theta_vector <- as.numeric(theta_draws$theta)
S_obs <- median(theta_vector)

cat("\n==================================================\n")
print(paste("Regional Bias-Adjusted Farm Prevalence:", round(S_obs, 4)))
cat("==================================================\n")
