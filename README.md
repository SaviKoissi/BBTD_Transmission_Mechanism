# Tracing the Pathways of Banana Bunchy Top Disease in Benin
<u> </u>

## 1. Overview

This project investigates the dominant transmission mechanisms of Banana Bunchy Top Disease (BBTD) in Benin using a spatially explicit stochastic metapopulation framework.


The analysis is built as a modular pipeline that integrates:

* Field-level observational data
* Bias-adjusted prevalence estimation
* Spatial data processing
* Bayesian inference via Stan
* Simulation-based inference (SMC-ABC) for transmission dynamics

The goal is to infer population-level transmission processes, not individual infection histories.

## 2. Scope of This Repository

This repository contains the data processing and observational inference layer of the broader modeling framework.

It represents the data-driven foundation upon which the simulation and ABC-SMC inference pipeline is built.

⚠️ The simulation and model calibration components (IBM, SMC-ABC) are implemented in a separate computational workflow and are not fully contained in this repository.

## 3. Project Philosophy

The workflow is designed to be:

* Modular → each stage is independent and reusable
* Reproducible → all steps are script-based and traceable
* Scalable → compatible with high-performance computing workflows
* Statistically rigorous → integrates Bayesian inference with uncertainty quantification

## 4. Data Pipeline Overview

The pipeline processes raw survey and spatial data into structured inputs suitable for downstream modeling.

* Core Steps
* Data Cleaning
	* Standardization of field identifiers
	* Handling missing values
    * Harmonization of survey formats
* Historical Data Integration
	* Incorporation of archival surveillance records
	* Alignment of temporal references
* Spatial Processing
	* Mapping farms to administrative boundaries
	* Linking geospatial shapefiles with field data
* Prevalence Estimation
	* Bayesian estimation of field-level prevalence
	* Adjustment for diagnostic misclassification (sensitivity/specificity)
* Data Merging
	* Integration of contemporary and historical datasets
	* Construction of unified analytical dataset

## 5. Directory Structure

```bash
project/
├── input/                 # Raw datasets and geospatial files
├── output/                # Processed data and results
│   ├── prevalence_*.csv
│   └── maps/
│
├── scripts/               # Modular R scripts for each pipeline step
│   ├── cleaning.R
│   ├── hist_data.R
│   ├── compute_prevalence.R
│   ├── merge.R
│   ├── fill_department.R
│   ├── spatial_utils.R
│   ├── bayesian_prevalence.R
│   ├── plots.R
│   ├── run.R
│   └── stan/
│       └── prevalence_model.stan
│
├── config/                # Configuration files (paths, parameters)
├── R/                     # Shared utilities and libraries
├── docs/                  # Documentation
├── logs/                  # Execution logs
└── README.md
```

## 6. Key Components of the Pipeline

### 6.1 Data Cleaning ( `scripts/cleaning.R`)

* Standardizes raw survey data
* Ensures consistency across datasets
* Prepares inputs for downstream analysis

### 6.2 Historical Data Processing (scripts/hist_data.R)

* Processes archival datasets
* Aligns temporal and spatial identifiers
* Enables longitudinal comparisons

### 6.3 Spatial Utilities (scripts/spatial_utils.R)

* Handles shapefile operations
* Maps farms to geographic regions
* Supports spatial joins and projections

## 6.4 Prevalence Estimation (`scripts/bayesian_prevalence.R`)

* Implements Bayesian inference using Stan
* Adjusts observed infection data for:
	* Sensitivity (Se)
	* Specificity (Sp)
* Produces posterior distributions of true prevalence

### 6.5 Data Merging (`scripts/merge.R`)

* Combines historical and contemporary datasets
* Produces unified analytical tables

### 6.6 Pipeline Execution (`scripts/run.R`)

* Orchestrates the full workflow
* Executes all modular steps in sequence

## 7. Statistical Modeling

Field-level prevalence is estimated using a Bayesian hierarchical model that accounts for:

* Sampling variability
* Field-level heterogeneity
* Diagnostic misclassification

The model is implemented in:

```R
scripts/stan/prevalence_model.stan
```

### 8. Outputs

The pipeline produces:

* `prevalence_per_farm.csv` → estimated prevalence per field
* `prevalence_per_farm_hist.csv` → historical estimates
* `prevalence_merged.csv `→ unified dataset
* Posterior summaries of prevalence with uncertainty

### 9. Connection to the Full Simulation Framework

This repository provides the empirical foundation for the broader modeling framework, which includes:

* Spatially explicit Individual-Based Models (IBM)
* Metapopulation dynamics on a farm network
* Sequential Monte Carlo Approximate Bayesian Computation (SMC-ABC)
* Transmission kernel selection (Gaussian, exponential, power-law, Lévy, etc.)
* Network-mediated dispersal mechanisms

The outputs from this pipeline are used to:

* Initialize simulations
* Calibrate transmission parameters
* Compare simulated vs observed spatial statistics
* Evaluate model fit via summary statistics (e.g., Moran’s I, Ripley’s K)

### 10. Reproducibility

To reproduce the pipeline:

Configure paths in:

```R
config/config.yml
```

Install required libraries:

```R
source("R/libraries.R")
```

Run the full pipeline:

```R
source("scripts/run.R")
```

### 11. Notes on Modularity

Each script is designed to:

* Perform a single, well-defined task
* Be executable independently
* Interact via standardized `input/output` files
* Allow substitution or extension without breaking the pipeline
