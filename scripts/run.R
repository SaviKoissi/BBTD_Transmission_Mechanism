# ==========================================================
# RUN THE PIPELINE
# ==========================================================


source("R/libraries.R"); library(yaml)
config <- yaml::read_yaml("config/config.yml")

source("scripts/cleaning.R")
source("scripts/compute_prevalence.R")

prevalence_per_farm <- compute_prevalence(data, config)

write.csv(prevalence_per_farm, config$paths$output_prevalence, row.names = FALSE)
