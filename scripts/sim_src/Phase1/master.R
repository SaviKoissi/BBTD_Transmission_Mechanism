# ==========================================================
# RUN THE SIMMULATION
# ==========================================================

config <- yaml::read_yaml("scripts/sim_src/Phase1/config/config.yml")

source("scripts/Obs_process/run.R") # This script will run all the analyses related to the data
source("scripts/sim_src/Phase1/spatial_embedding.R")
source("scripts/sim_src/Phase1/distance_matrix.R")
source("scripts/sim_src/Phase1/network_adjacency.R")
source("scripts/sim_src/Phase1/initial_state.R")
source("scripts/sim_src/Phase1/observed_metrics.R")
source('scripts/sim_src/Phase1/build_phase1.R')
source("scripts/sim_src/Phase1/phase1.R")


