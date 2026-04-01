# ==========================================================
# MERGE HISTORICAL + CURRENT PREVALENCE DATA
# ==========================================================


merge_prevalence <- function(current, historical) {
  
  # Ensure same column names & types
  common_cols <- intersect(names(current), names(historical))
  
  current <- current %>% select(all_of(common_cols))
  historical <- historical %>% select(all_of(common_cols))
  
  # Bind rows (stack datasets)
  merged <- bind_rows(current, historical) %>%
    
    # Optional: remove duplicates if same farm-year appears twice
    distinct(Unique_Field_ID, year, .keep_all = TRUE)
  
  return(merged)
}