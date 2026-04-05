# ========================================================== 
# MERGE SURVEY DATA WITH HISTORICAL DATA
# ========================================================== 

merge_prevalence <- function(current, historical) {
  
  # Harmonize ID type
  current <- current %>%
    mutate(Unique_Field_ID = as.character(Unique_Field_ID))
  
  historical <- historical %>%
    mutate(Unique_Field_ID = as.character(Unique_Field_ID))
  
  # Keep common columns
  common_cols <- intersect(names(current), names(historical))
  
  current <- current %>% dplyr::select(all_of(common_cols))
  historical <- historical %>% dplyr::select(all_of(common_cols))
  
  # Merge
  merged <- bind_rows(current, historical) %>%
    distinct(Unique_Field_ID, year, .keep_all = TRUE)
  
  return(merged)
}