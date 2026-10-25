# Phase 6: PNAS Analytical Metrics & Network Topology Selection

## Overview

Phase 6 provides analytical post-processing and model selection framework for the **Banana Bunchy Top Disease (BBTD)** spatial transmission modeling pipeline. It translates ABC posterior sampling outputs into epidemiological metrics (comparable to PNAS-style multi-scale epidemic analyses) and evaluates competing seed-exchange network topologies.

Specifically, Phase 6 addresses a key data-scarcity challenge: **evaluating candidate human-mediated seed exchange network structures in the absence of explicit, empirical trading logs.**

---

## Key Metrics & Analytical Outputs

Phase 6 computes four primary analytical outputs:

| Metric | Name | Analytical Objective | Mathematical Formulation |
| :--- | :--- | :--- | :--- |
| **Metric A** | **Pathway-Specific $R_{0,k}$** | Decomposes total effective reproductive number into local aphid, wind vector, and human network components. | $R_{0,k} = R_{0,\text{global}} \times w_k$ |
| **Metric B** | **Spatial Wavefront Elasticity ($E_v$)** | Quantifies the sensitivity of wave propagation velocity ($v_{\text{obs}}$) to changes in local ($\sigma$) and long-range ($\kappa$) dispersal kernels via finite differences. | $E_{v,x} = \frac{\partial v}{\partial x} \frac{x}{v_{\text{obs}}}$ |
| **Metric C** | **Network-Landscape Congruence Index ($\Gamma$)** | Evaluates Spearman correlation between network connectivity ($A_{jk}$) and physical distance decay kernels ($K_{\text{spatial}}$). | $\Gamma = \text{cor}_{\text{spearman}}(v_A, v_K)$ |
| **Metric D** | **Network Topology Selection** | Evaluates ABC posterior support across candidate network hypotheses using forward simulation or spatial distance discrepancies. | $P(M_m \mid S_{\text{obs}}) = \frac{\sum I(d_i \le \epsilon)}{N_{\text{accepted}}}$ |

---

## Evaluated Network Topologies

The network topology selection engine (`network_selection.R`) evaluates three competing structural hypotheses:

1. **Topological / Mean-Field (`topological`):**
   * *Formal Graph Type:* Complete Graph ($K_n$) / All-to-All.
   * *Hypothesis:* Unrestricted, panmictic seed exchange across the entire region regardless of spatial proximity.
2. **Spatial Threshold (`spatial_threshold`):**
   * *Formal Graph Type:* Random Geometric Graph (RGG).
   * *Hypothesis:* Strict distance-bounded seed trading within a uniform spatial radius (e.g., $15\text{ km}$ cutoff).
3. **$k$-Nearest Neighbors (`knn`):**
   * *Formal Graph Type:* Directed $k$-NN Spatial Graph.
   * *Hypothesis:* Preferential, clustered seed exchange between each farm and its $k$ closest neighboring agricultural partners.

---

## Directory Structure & Modules

```text
scripts/sim_src/Phase6/
├── README.md                 # Module documentation (this file)
├── network_selection.R       # ABC network topology choice engine
└── run_phase6.R              # Main orchestration function for Phase 6 metrics
```

---

## Function Reference

### 1. `run_phase6()`
Main entry point for Phase 6 analytical metric calculations.

```R
results <- run_phase6(
  posterior_df    = posterior_particles, # Dataframe of ABC posterior parameters
  network_matrix  = adjacency_matrix,    # Optional: Farm adjacency matrix (NxN)
  distance_matrix = dist_matrix,         # Optional: Spatial Euclidean distance matrix (NxN)
  phase2_obj      = phase2_runtime_env,  # Phase 1/2 spatial context object
  S_obs           = summary_stats_obs    # Observed target metrics (e.g., epidemic velocity)
)
```

**Returned List Components:**
* `results$R0_decomposition`: Dataframe of $R_0$ split across pathways with 95% credible intervals.
* `results$elasticity`: Dataframe of $E_v$ elasticity indices for $\sigma$ (local) and $\kappa$ (wind).
* `results$congruence`: List with Spearman's $\Gamma$ index and biological interpretation.
* `results$network_selection`: Summary table and posterior probabilities for network topologies.

### 2. `evaluate_network_topologies()`
Internal engine executing ABC model choice across candidate graph topologies.

```R
selection_out <- evaluate_network_topologies(
  phase2_obj         = phase2_obj,
  posterior_df       = posterior_df,
  S_obs              = S_obs,
  candidate_modes    = c("topological", "spatial_threshold", "knn"),
  n_particles        = 100,
  tolerance_quantile = 0.10
)
```

---

## Usage Example

```R
library(dplyr)

# Source required Phase 6 scripts
source("scripts/sim_src/Phase6/network_selection.R")
source("scripts/sim_src/Phase6/run_phase6.R")

# Example: Run Phase 6 evaluation
phase6_output <- run_phase6(
  posterior_df = abc_posterior_samples,
  phase2_obj   = phase2_context,
  S_obs        = list(epidemic_velocity = 8.4) # Velocity in km/year
)

# Print Summary Table for Network Selection
print(phase6_output$network_selection$summary_table)

# Access R0 Decomposition
print(phase6_output$R0_decomposition)
```

---

## Input Dependencies

To ensure error-free execution, ensure the following parameter/column names or fallbacks exist in `posterior_df` or `phase2_obj`:

* **Posterior DataFrame (`posterior_df`):** `prop_local`, `prop_long`, `prop_net`, `sigma` (or `beta_local`), `kappa` (or `beta_long`), `r0`.
* **Spatial Context (`phase2_obj`):** Spatial coordinates (`coords` / `spatial_df`) and distance matrix (`distance_matrix` / `dist`).