# ==========================================================
# LIBRARIES REQUIRED
# ==========================================================

# Core
library(tidyverse)
library(sf)
library(readxl)
library(yaml)

# Spatial
library(terra)
library(ggspatial)
library(spatstat)

# Bayesian
library(cmdstanr)
library(posterior)
library(brms)   # optional but you already use it

# Utilities
library(stringr)
library(stringdist)
library(ggrepel)
library(viridis)

# Simulations
library(spdep)
library(vegan)
library(MCMCpack)
