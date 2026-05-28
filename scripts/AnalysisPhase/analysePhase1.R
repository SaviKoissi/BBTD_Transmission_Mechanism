
# ======================================================================
# PHASE 0: TABLE - POSTERIOR ESTIMATES
# ======================================================================
fit <- fit_obj$fit

# Extract parameters safely
draws_param <- suppressWarnings(fit$draws(c("theta", "Se", "Sp", "mu", "sigma_field"), format = "df"))

table_phase1 <- draws_param %>%
  pivot_longer(cols = c(theta, Se, Sp, mu, sigma_field),
               names_to = "Parameter", values_to = "Value") %>%
  group_by(Parameter) %>%
  summarise(
    Median = round(median(Value), 4),
    CrI_Lower = round(quantile(Value, 0.025), 4),
    CrI_Upper = round(quantile(Value, 0.975), 4),
    .groups = "drop"
  )

print("--- PHASE 0 TABLE: POSTERIOR SUMMARIES ---")
print(table_phase1)

# ======================================================================
# PHASE 0: VISUALIZATION - PRIOR VS POSTERIOR
# ======================================================================
# Generate analytical prior shapes from your pipeline variables to overlay
prior_theta <- data.frame(Value = extras::inv_logit(rnorm(4000, 0.55, 0.25)), 
                          Type = "Prior", Parameter = "theta")

post_theta  <- data.frame(Value = as.numeric(draws_param$theta), 
                          Type = "Posterior", Parameter = "theta")

prior_Se    <- data.frame(Value = rbeta(4000, Se_priors$alpha, Se_priors$beta), 
                          Type = "Prior", Parameter = "Se")

post_Se     <- data.frame(Value = as.numeric(draws_param$Se), 
                          Type = "Posterior", Parameter = "Se")

prior_Sp    <- data.frame(Value = rbeta(4000, Sp_priors$alpha, Sp_priors$beta), 
                          Type = "Prior", Parameter = "Sp")

post_Sp     <- data.frame(Value = as.numeric(draws_param$Sp), 
                          Type = "Posterior", Parameter = "Sp")

plot_data <- rbind(prior_theta, post_theta, prior_Se, 
                   post_Se, prior_Sp, post_Sp)

plot_phase1 <- ggplot(plot_data, aes(x = Value, fill = Type)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~Parameter, scales = "free") +
  scale_fill_manual(values = c("Prior" = "grey50", "Posterior" = "#2b8cbe")) +
  theme_minimal() +
  labs(title = "Prior vs. Posterior Density Overlays", y = "Density", x = "Parameter Value")

print(plot_phase1)
