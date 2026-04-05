
# Phase 1: Constructing the "Target Truth" ($S_{obs}$) Landscape & Network Initialization

* Spatial Embedding: Map the 176 farms onto a 2D coordinate system using the Longitude and Latitude from your cleaned data_fullData.
* Distance Matrix Computation: Generate a global Euclidean distance matrix ($d_{jk}$) representing all pairwise distances between farms.
* Network Adjacency: Initialize the $A_{jk}$ matrix representing seed exchange links. If explicit link data is unavailable, this is modeled as a topological connectivity matrix independent of Euclidean distance.
* Temporal Calibration: Set the starting point to $t=2012$ and define the initial state vector $X_j(0)$, marking farms as Infected ($1$) or Susceptible ($0$) based on archival surveillance.

## Architecture of files 

```bash
scripts/
├── sim_src/phase1/
│   ├── spatial_embedding.R
│   ├── distance_matrix.R
│   ├── network_adjacency.R
│   ├── initial_state.R
│   ├── observed_metrics.R
│   └── build_phase1.R   # orchestrator
```