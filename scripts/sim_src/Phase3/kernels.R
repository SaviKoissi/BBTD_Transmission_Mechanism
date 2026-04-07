# ==========================================================
# KERNEL FUNCTIONS
# ==========================================================

# gaussian_kernel <- function(d, sigma) {
#   exp(-(d^2) / (2 * sigma^2))
# }
# 
# exponential_kernel <- function(d, sigma) {
#   exp(-d / sigma)
# }
# 
# cauchy_kernel <- function(d, kappa) {
#   1 / (1 + (d / kappa)^2)
# }
# 
# powerlaw_kernel <- function(d, kappa, p) {
#   (1 + d / kappa)^(-p)
# }
# 
# levy_kernel <- function(d, kappa) {
#   # (1 + d) ensures the denominator is never zero
#   (1 + d)^(-(1 + kappa))
# }
# 
# # Kernel selector
# select_kernel <- function(type) {
#   switch(type,
#          gaussian = gaussian_kernel,
#          exponential = exponential_kernel,
#          cauchy = cauchy_kernel,
#          powerlaw = powerlaw_kernel,
#          levy = levy_kernel,
#          stop("Unknown kernel type"))
# }

# --- Enhanced kernels.R ---

# 1. Add Normalization to ensure area under curve = 1
# This ensures that 'w1' actually controls the weight, not the kernel math.
gaussian_kernel <- function(d, sigma) {
  norm_const <- 1 / (sigma * sqrt(2 * pi))
  norm_const * exp(-(d^2) / (2 * sigma^2))
}

exponential_kernel <- function(d, sigma) {
  norm_const <- 1 / sigma
  norm_const * exp(-d / sigma)
}

# 2. Unified Heavy-Tail Kernel (Power Law / Levy / Cauchy)
# p = 2 is Cauchy-like
# 1 < p < 2 is Levy-like
# p > 3 starts behaving like Exponential
powerlaw_kernel <- function(d, kappa, p) {
  if (p <= 1) return(rep(0, length(d))) # Prevent divergent integrals
  norm_const <- (p - 1) / kappa
  norm_const * (1 + d / kappa)^(-p)
}

# 3. Enhanced Selector that handles parameter requirements
apply_kernel <- function(type, d, params) {
  kernel_func <- switch(type,
                        gaussian    = gaussian_kernel(d, params$sigma),
                        exponential = exponential_kernel(d, params$sigma),
                        cauchy      = powerlaw_kernel(d, params$kappa, p=2), # Cauchy is p=2
                        levy        = powerlaw_kernel(d, params$kappa, params$p), # p between 1 and 2
                        powerlaw    = powerlaw_kernel(d, params$kappa, params$p),
                        stop("Unknown kernel type"))
  return(kernel_func)
}
# Kernel selector
select_kernel <- function(type) {
  switch(type,
         gaussian = gaussian_kernel,
         exponential = exponential_kernel,
         cauchy = cauchy_kernel,
         powerlaw = powerlaw_kernel,
         levy = levy_kernel,
         stop("Unknown kernel type"))
}
