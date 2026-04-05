# Phase 3: Stochastic IBM Simulation Engine
* Kernel Selection Module: For each simulation run, activate one specific kernel type per class:
	* Local: Gaussian vs. Exponential.
	* Long-range: Cauchy vs. Lévy vs. Power Law.
* Force of Infection Calculation: Compute $\lambda_j(t)$ for every susceptible farm by summing the weighted contributions of all currently infected farms $k$ through the spatial and network kernels.
* Probabilistic State Transition: Update farm status using the Bernoulli trial: $P(S \to I) = 1 - \exp(-\Lambda_j \Delta t)$.
* Normalization Step: Apply discrete summation normalization $\sum K(x) = 1$ to ensure total transmission potential is conserved across different kernels and landscape geometries.




```bash
scripts/sim_src/phase3/
├── kernels.R
├── normalization.R
├── infection_pressure.R
├── state_update.R
├── simulation_engine.R
├── bias_injection.R
├── abc_particle_run.R
└── run_phase3.R

```