# ==========================================================
# INITIAL STATE
# ==========================================================


# scripts/phase1/initial_state.R

# create_initial_state <- function(data,
#                                  year = 2012,
#                                  id_col = "Unique_Field_ID",
#                                  infected_col = "NumbDiseasedPlant_Farm") {
#   
#   baseline <- data %>%
#     dplyr::filter(year == !!year) %>%
#     dplyr::select(
#       farm_id = all_of(id_col),
#       infected = all_of(infected_col)
#     ) %>%
#     distinct()
#   
#   # Convert to binary state
#   baseline$state <- ifelse(baseline$infected > 0, 1, 0)
#   
#   X0 <- baseline$state
#   names(X0) <- baseline$farm_id
#   
#   return(X0)
# }
create_initial_state <- function(data_full, year = 2018) {
  # 1. Filter for the start year
  initial_data <- data_full %>% filter(year == !!year)
  
  # 2. Check if the initial data is empty
  if(nrow(initial_data) == 0) {
    stop("Error: No farm data found for the year 2012. Check your dataset filter.")
  }
  
  # 3. Extract Coordinates safely from SF geometry
  # This works even if columns aren't named 'longitude/latitude'
  coords <- sf::st_coordinates(initial_data)
  lon_vals <- coords[,1]
  lat_vals <- coords[,2]
  
  # 4. Attempt to find observed infections
  x0_vector <- if_else(initial_data$NumbDiseasedPlant_Farm > 0, 1L, 0L)
  
  # 5. IF NO INFECTIONS FOUND (Force the Southern Origin):
  if(sum(x0_vector) == 0) {
    origin_lon <- 2.580
    origin_lat <- 6.575
    
    # Euclidean distance to the Akpro-Missérété point
    dists <- sqrt((lon_vals - origin_lon)^2 + (lat_vals - origin_lat)^2)
    
    patient_zero_idx <- which.min(dists)
    x0_vector[patient_zero_idx] <- 1L
    
    message(paste("Manual Seeding: Farm at index", patient_zero_idx, 
                  "set as Patient Zero near Akpro-Missérété."))
  }
  
  return(x0_vector)
}