# ==========================================================
# RUN THE SIMMULATION
# ==========================================================

config <- yaml::read_yaml("scripts/sim_src/config/config.yml")

source("scripts/run.R") # This script will run all the analyses related to the data
source("scripts/sim_src/spatial_embedding.R")
source("scripts/sim_src/distance_matrix.R")
source("scripts/sim_src/network_adjacency.R")
source("scripts/sim_src/initial_state.R")
source("scripts/sim_src/observed_metrics.R")
source('scripts/sim_src/build_phase1.R')
source("scripts/sim_src/phase1.R")


