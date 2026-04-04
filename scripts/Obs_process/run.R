# ==========================================================
# RUN DATA RELATED PIPELINE
# ==========================================================

source("R/libraries.R")
config <- yaml::read_yaml("config/config.yml")

source("scripts/Obs_process/cleaning.R")
source("scripts/Obs_process/compute_prevalence.R")

prevalence_per_farm <- compute_prevalence(data, config)
write.csv(prevalence_per_farm, config$paths$output_prevalence,
          row.names = FALSE)
source("scripts/Obs_process/hist_data.R")
source("scripts/Obs_process/merge_prevalence.R")
data_fullData <- merge_prevalence(prevalence_per_farm, 
                                  prevalence_per_farm_hist)
source("scripts/Obs_process/fill_department.R")

write.csv(data_fullData, config$paths$output_prevalence,
          row.names = FALSE)

source("scripts/Obs_process/bias_adjusted_prevalence.R")
write.csv(posterior_prev, "output/bayesian_prevalence.csv", row.names = FALSE)
