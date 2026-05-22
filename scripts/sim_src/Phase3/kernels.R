# ==========================================================
# KERNEL FUNCTIONS
# ==========================================================

# Standardize all kernels to return a vector based on distance 'd'
gaussian_kernel <- function(d, sigma) { (1/(sigma*sqrt(2*pi))) * exp(-(d^2)/(2*sigma^2)) }
exponential_kernel <- function(d, sigma) { (1/sigma) * exp(-d/sigma) }
powerlaw_kernel <- function(d, kappa, p) { 
  if (p <= 1) return(rep(0, length(d)))
  ((p - 1) / kappa) * (1 + d / kappa)^(-p) 
}

# select_kernel <- function(type) {
#   switch(type,
#          gaussian = gaussian_kernel,
#          exponential = exponential_kernel,
#          powerlaw = powerlaw_kernel,
#          cauchy = function(d, kappa) powerlaw_kernel(d, kappa, p=2),
#          levy = function(d, kappa, p) powerlaw_kernel(d, kappa, p),
#          stop("Unknown kernel type"))
# }

select_kernel <- function(type) {
  switch(type,
         gaussian    = gaussian_kernel,
         exponential = exponential_kernel,
         powerlaw    = powerlaw_kernel,
         # Standardize signatures so any extra vector parameters flow smoothly
         cauchy      = function(d, kappa, ...) powerlaw_kernel(d, kappa, p = 2),
         levy        = function(d, kappa, p, ...) powerlaw_kernel(d, kappa, p),
         stop("Unknown kernel type requested"))
}