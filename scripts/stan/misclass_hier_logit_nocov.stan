data {
  int<lower=1> N;                         // Number of observations
  int<lower=1> K;                         // Number of fields (groups)

  // NEW ARRAY SYNTAX
  array[N] int<lower=1, upper=K> field;   // Field index mapping
  array[N] int<lower=0> y;                // Number of observed infected plants
  array[N] int<lower=1> n;                // Total plants sampled per observation

  // Priors for sensitivity and specificity (Hyperparameters)
  real<lower=0> Se_a;
  real<lower=0> Se_b;
  real<lower=0> Sp_a;
  real<lower=0> Sp_b;
}

parameters {
  real mu;                                // Global intercept (logit scale)
  real<lower=0> sigma_field;              // SD of field-level random effects
  vector[K] z_field;                      // Non-centered field effects

  real<lower=0, upper=1> Se;              // Sensitivity
  real<lower=0, upper=1> Sp;              // Specificity
}

transformed parameters {
  vector[K] alpha_field;                  
  vector<lower=0, upper=1>[N] p_obs;      // The "Misclassified" probability

  // Non-centered parameterization to improve convergence on HPC
  alpha_field = mu + (sigma_field * z_field);

  for (i in 1:N) {
    real p_true;
    p_true = inv_logit(alpha_field[field[i]]);
    
    // Core misclassification logic
    p_obs[i] = Se * p_true + (1 - Sp) * (1 - p_true);
  }
}

model {
  // Priors
  mu ~ normal(0, 2);
  sigma_field ~ normal(0, 1);
  z_field ~ normal(0, 1); 

  Se ~ beta(Se_a, Se_b);
  Sp ~ beta(Sp_a, Sp_b);

  // Vectorized likelihood (faster than a loop)
  y ~ binomial(n, p_obs);
}

generated quantities {
  vector[N] log_lik;
  vector[K] field_prev;                   // "True" prevalence per field

  for (i in 1:N) {
    log_lik[i] = binomial_lpmf(y[i] | n[i], p_obs[i]);
  }

  for (k in 1:K) {
    field_prev[k] = inv_logit(alpha_field[k]);
  }
}