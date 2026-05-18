# # ==========================================================
# # WAVEFRONT VELOCITY
# # ==========================================================

# compute_wavefront_velocity <- function(history_list, coords, times) {
#   # 1. Identify the origin (Akpro-Missérété)
#   # history_list[[1]] is the state at t_start (2018)
#   origin_idx <- which(as.numeric(unlist(history_list[[1]])) == 1)[1]
#   
#   # Safety: if simulation failed to seed, return 0 velocity
#   if (is.na(origin_idx)) return(0)
#   origin_coords <- coords[origin_idx, ]
#   
#   # 2. Calculate expansion over time
#   max_distances <- sapply(history_list, function(X) {
#     X_vec <- as.numeric(unlist(X))
#     infected_idx <- which(X_vec == 1)
#     
#     if (length(infected_idx) <= 1) return(0)
#     
#     # Euclidean distance of all infected points from the 2018 origin
#     dists <- sqrt((coords[infected_idx, 1] - origin_coords[1])^2 + 
#                     (coords[infected_idx, 2] - origin_coords[2])^2)
#     return(max(dists))
#   })
#   
#   # 3. Type-Fix: Force to numeric vector for the linear model
#   dist_vec <- as.numeric(unlist(max_distances))
#   time_vec <- as.numeric(times)
#   
#   if (length(unique(dist_vec)) < 2) return(0)
#   
#   # slope = km/year
#   fit <- lm(dist_vec ~ time_vec)
#   return(as.numeric(coef(fit)[2])) 
# }

# Below the code wavefront that generate 0 as the velocity
# 
# compute_wavefront_velocity <- function(history_list, coords, times) {
#   # 1. FIX THE ORIGIN: Identify the very first farm infected in the simulation
#   # We look at the first state that has at least one '1'
#   first_infected_step <- which(sapply(history_list, function(x) sum(as.numeric(x))) > 0)[1]
#   
#   if (is.na(first_infected_step)) return(0)
#   
#   origin_idx <- which(as.numeric(history_list[[first_infected_step]]) == 1)[1]
#   origin_coords <- coords[origin_idx, ]
#   
#   # 2. TRACK THE DISTANCE: How far is the furthest infection from THAT origin?
#   max_dists <- sapply(history_list, function(X) {
#     inf_idx <- which(as.numeric(X) == 1)
#     if (length(inf_idx) == 0) return(0)
#     
#     # Distance from FIXED origin to all current infections
#     dists <- sqrt((coords[inf_idx, 1] - origin_coords[1])^2 + 
#                     (coords[inf_idx, 2] - origin_coords[2])^2)
#     return(max(dists))
#   })
#   
#   # 3. REGRESSION: Velocity is the slope (km/year)
#   d <- as.numeric(max_dists)
#   t <- as.numeric(times)
#   
#   if (length(unique(d)) < 2) return(0)
#   
#   # Fit line: distance ~ time
#   fit <- lm(d ~ t)
#   return(as.numeric(coef(fit)[2])) 
# }



compute_wavefront_velocity <- function(history_list, coords, times) {
  
  # 1. FIX THE ORIGIN (Θ) based on the paper's baseline
  # The paper defines the origin as the 2018 focus. 
  # We extract the coordinates of the cells infected at time step 1.
  first_step_infected <- which(as.numeric(unlist(history_list[[1]])) == 1)
  
  if (length(first_step_infected) == 0) return(0) # Safety: no seeding
  
  # Calculate the centroid of the initial 2018 focus as origin (Theta)
  origin_coords <- colMeans(matrix(coords[first_step_infected, ], ncol = 2))
  
  # 2. TRACK MEAN BOUNDARY DISTANCE (D_t) per the paper's formula
  mean_dists <- sapply(history_list, function(X) {
    inf_idx <- which(as.numeric(unlist(X)) == 1)
    
    # If no infections, distance from origin is 0
    if (length(inf_idx) == 0) return(0)
    
    # Calculate distance from origin to ALL infected points at time t
    dists <- sqrt((coords[inf_idx, 1] - origin_coords[1])^2 + 
                    (coords[inf_idx, 2] - origin_coords[2])^2)
    
    # PAPER ALIGNMENT: Return the MEAN distance (D_t), not the MAX distance
    return(mean(dists))
  })
  
  # 3. REGRESSION ANALYSIS (D_t = beta_0 + beta_1 * t)
  d <- as.numeric(mean_dists)
  t <- as.numeric(times)
  
  # If the mean distance doesn't change across time steps, velocity is 0
  if (length(unique(d)) < 2) return(0)
  
  # Fit the linear model to extract the average rate of spread (beta_1)
  fit <- lm(d ~ t)
  velocity <- as.numeric(coef(fit)[2])
  
  # Ensure we don't pass back a negative slope artifact if things fluctuate
  return(max(0, velocity)) 
}
