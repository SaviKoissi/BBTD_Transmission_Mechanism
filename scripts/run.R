# ==========================================================
# RUN THE PIPELINE
# ==========================================================

source("R/libraries.R")
config <- yaml::read_yaml("config/config.yml")

source("scripts/cleaning.R")
source("scripts/compute_prevalence.R")

prevalence_per_farm <- compute_prevalence(data, config)
write.csv(prevalence_per_farm, config$paths$output_prevalence,
          row.names = FALSE)
source("scripts/hist_data.R")
source("scripts/merge_prevalence.R")
data_fullData <- merge_prevalence(prevalence_per_farm, 
                                  prevalence_per_farm_hist)
source("scripts/fill_department.R")

data_fullData <- fill_missing_department(
  data_fullData,
  benin_shp,
  "adm1_name"
)

write.csv(data_fullData, config$paths$output_prevalence,
          row.names = FALSE)

source("scripts/bias_adjusted_prevalence.R")

# data_fullData = your merged dataset
fit_obj <- run_model(data_fullData)

posterior_prev <- extract_prevalence(fit_obj)

write.csv(data_with_results, "output/bayesian_prevalence.csv", row.names = FALSE)
