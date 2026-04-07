# Phase 4: Observational Bias & Summary Statistics

* Misclassification Filtering: Pass the "true" simulated states through the Bayesian misclassification filter ($Se = 0.78, Sp = 0.92$) to generate "observed" simulated data.
* Metric Extraction: Calculate the synthetic summary statistics vector ($S_{sim}$):
	* Wavefront Velocity: Slope of the infection distance over time.
	* Clustering: Moran’s I and Ripley’s K-function.
	* Prevalence: Overall proportion of infected farms.


```bash
scripts/simulation/phase4/
├── misclassification.R
├── wavefront_velocity.R
├── clustering_stats.R
├── prevalence_stats.R
├── summary_stats.R
├── distance_metrics.R   # (for Phase 5 later)
└── run_phase4.R

```