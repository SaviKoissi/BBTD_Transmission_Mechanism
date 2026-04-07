# ==========================================================
# STATE TRANSITION
# ==========================================================

update_state <- function(X, lambda, dt = 1) {
  
  p_infect <- 1 - exp(-lambda * dt)
  
  new_infections <- rbinom(length(X), 1, p_infect)
  
  # SI model: once infected, always infected
  X_new <- pmax(X, new_infections)
  
  return(X_new)
}

