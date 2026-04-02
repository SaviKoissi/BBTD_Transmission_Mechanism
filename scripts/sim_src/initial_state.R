# ==========================================================
# INITIAL STATE
# ==========================================================


# scripts/phase1/initial_state.R

create_initial_state <- function(data,
                                 year = 2012,
                                 id_col = "Unique_Field_ID",
                                 infected_col = "NumbDiseasedPlant_Farm") {
  
  baseline <- data %>%
    dplyr::filter(year == !!year) %>%
    dplyr::select(
      farm_id = all_of(id_col),
      infected = all_of(infected_col)
    ) %>%
    distinct()
  
  # Convert to binary state
  baseline$state <- ifelse(baseline$infected > 0, 1, 0)
  
  X0 <- baseline$state
  names(X0) <- baseline$farm_id
  
  return(X0)
}