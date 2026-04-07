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

source("scripts/sim_src/Phase2/prior.R")
source("scripts/sim_src/Phase2/sampling.R")
source("scripts/sim_src/Phase2/transformation.R")
source("scripts/sim_src/Phase2/particules.R")
source("scripts/sim_src/Phase2/init_simulation.R")
source("scripts/sim_src/Phase2/run_phase2.R")
source("scripts/sim_src/Phase2/ph2_exe.R")

source("scripts/sim_src/Phase3/normalization.R")
source("scripts/sim_src/Phase3/kernels.R")
source("scripts/sim_src/Phase3/infection_pressure.R") # Load this AFTER kernels
source("scripts/sim_src/Phase3/bias_injection.R")
source("scripts/sim_src/Phase3/simulation_engine.R")
source("scripts/sim_src/Phase3/abc_particle_run.R")
source("scripts/sim_src/Phase3/run_phase3.R")
source("scripts/sim_src/Phase3/phase3.R")
