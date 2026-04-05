# ==========================================================
# PARAMETER TRANSFORMATIONS
# ==========================================================

compute_beta_eff <- function(beta, r, tau) {
  beta * exp(-r * tau)
}

transform_parameters <- function(params) {
  
  params$beta_eff <- compute_beta_eff(
    beta = params$beta,
    r = params$r,
    tau = params$tau
  )
  
  return(params)
}