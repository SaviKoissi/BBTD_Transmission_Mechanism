


## Project Structure

```bash
project/
├── input/
│   ├── BBTD_transformed_06_2025_v2_complet.xlsx
│   ├── banana_wave_2018.csv
│   └── shapefiles/
│       └── BEN/
│           ├── ben_admbnda_adm2_1m_salb_20190816.shp
│           ├── ben_admbnda_adm2_1m_salb_20190816.dbf
│           ├── ben_admbnda_adm2_1m_salb_20190816.shx
│           ├── ben_admbnda_adm2_1m_salb_20190816.prj
│           └── ... (other associated files)
│
├── output/
│   ├── prevalence_per_farm.csv
│   ├── prevalence_per_farm_hist.csv
│   ├── prevalence_merged.csv
│   └── maps/
│
├── scripts/
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
├── config/
│   └── config.yml
│
├── R/
│   ├── libraries.R
│   └── utils.R
│
├── docs/
├── logs/
├── .gitignore
└── README.md
```
