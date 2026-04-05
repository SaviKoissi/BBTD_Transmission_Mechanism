# Phase 2: Parameter Sampling (ABC-SMC Particle Generation)

* Latent Variable Initialization: Define prior distributions for unknown parameters:
	* Transmission Weights: $w_1, w_2, w_3$ (via a Dirichlet distribution to ensure $\sum w = 1$).
	* Growth Rates: Intrinsic growth rate $r$ and latent phase duration $\tau$.
	* Spatial Scales: $\sigma$ (local dispersal) and $\kappa, p$ (long-range scales).
* Analytical Transformation: Compute the effective transmission rate $\beta_{eff} = \beta e^{-r\tau}$ for each particle to linearize the latent period delay.


```bash
scripts/sim_src/phase2
├── priors.R
├── sampling.R
├── transformations.R
├── particles.R
├── init_simulation.R
└── run_phase2.R
```