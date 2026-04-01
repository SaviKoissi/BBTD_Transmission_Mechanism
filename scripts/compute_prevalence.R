# ==========================================================
# COMPUTE PREVALENCE
# ==========================================================

compute_prevalence <- function(data, config) {
  data %>%
    dplyr::select(
      all_of(c(
        config$columns$id,
        config$columns$lon,
        config$columns$lat,
        "Departement",
        config$columns$disease,
        "Location", "Fieldsize", "PlantationAge"
      ))
    ) %>%
    dplyr::group_by(
      across(c(
        all_of(config$columns$id),
        all_of(config$columns$lon),
        all_of(config$columns$lat),
        Departement, Location, Fieldsize, PlantationAge
      ))
    ) %>%
    dplyr::summarise(
      NumbDiseasedPlant_Farm = sum(.data[[config$columns$disease]] == 1, na.rm = TRUE),
      TotalPlant_Farm = sum(.data[[config$columns$disease]] %in% c(0, 1), na.rm = TRUE),
      year = config$settings$year,
      prevalence = 100 * NumbDiseasedPlant_Farm / TotalPlant_Farm,
      .groups = "drop"
    )
}
