data {
  int<lower=1> N;                         // Number of farms
  int<lower=1> K;                         // Total unique fields
  array[N] int<lower=1, upper=K> field;   // Field mapping index
  array[N] int<lower=0> y;                // Number of observed diseased plants per farm
  array[N] int<lower=1> m;                // Total number of plants inspected per farm

  // Hyperparameters for Se and Sp priors
  real<lower=0> Se_a;
  real<lower=0> Se_b;
  real<lower=0> Sp_a;
  real<lower=0> Sp_b;
}

parameters {
  real mu;                                // Log-odds of plant infection inside INFECTED fields
  real<lower=0> sigma_field;              // Variance between truly infected fields
  vector[K] z_field;                      // Non-centered spatial field effects

  real logit_theta;                       // Global farm occupancy log-odds
  real<lower=0.5, upper=1.0> Se;          // Sensitivity
  real<lower=0.5, upper=1.0> Sp;          // Specificity
}

transformed parameters {
  vector[K] alpha_field = mu + (sigma_field * z_field);
  real<lower=0.0, upper=1.0> theta;       
  theta = inv_logit(logit_theta);
}

model {
  // PRIORS - Highly stable, standard weakly-informative layout
  mu ~ normal(0.0, 1.0);                  
  sigma_field ~ exponential(2.0);         
  z_field ~ normal(0, 1);                 
  
  // Directly targets your >62% operational expectation baseline
  logit_theta ~ normal(0.55, 0.25);       

  Se ~ beta(Se_a, Se_b);
  Sp ~ beta(Sp_a, Sp_b);

  // LATENT STRATIFIED MIXTURE ENGINE
  {
    vector[N] p_inf = inv_logit(alpha_field[field]);
    
    for (i in 1:N) {
      // Probability profiles for both potential states
      real log_prob_if_infected = log(theta) + binomial_lpmf(y[i] | m[i], Se * p_inf[i] + (1.0 - Sp) * (1.0 - p_inf[i]));
      real log_prob_if_healthy  = log1m(theta) + binomial_lpmf(y[i] | m[i], 1.0 - Sp);
      
      if (y[i] == 0) {
        // A zero-count field could be a clean field OR an infected field with missed cases
        target += log_sum_exp(log_prob_if_healthy, log_prob_if_infected);
      } else {
        // HARDENED LINE: If sick plants exist, it CANNOT be structurally healthy.
        // It belongs strictly to the infected distribution pool.
        target += log_prob_if_infected;
      }
    }
  }
}

generated quantities {
  vector[K] field_prev;                   
  field_prev = inv_logit(alpha_field);
}















