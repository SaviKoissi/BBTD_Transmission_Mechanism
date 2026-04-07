# Phase 5: Model Selection & Posterior Inference

* Euclidean Distance Comparison: Evaluate $\rho(S_{sim}, S_{obs})$ against the tolerance threshold $\epsilon$.
* Kernel Selection: Compare different kernel formulations (e.g., Gaussian vs. Exponential) to identify which most accurately replicates the observed $37.8$ km/yr wavefront velocity.
* Weight Inference: Derive the 95% Credible Intervals for $w_1, w_2, w_3$ to determine the dominant transmission mechanism (Natural vs. Human-mediated).


```bash
scripts/simulation/phase5/
├── distance.R
├── acceptance.R
├── weighting.R
├── resampling.R
│
├── posterior/
│   ├── extract_posterior.R
│   ├── summary_tables.R
│   ├── ternary_plot.R
│   ├── posterior_plots.R
│   └── model_comparison.R
│
├── diagnostics.R
└── run_phase5.R
```