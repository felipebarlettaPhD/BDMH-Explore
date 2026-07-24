# BDMH-Explore

Interactive Shiny dashboard for exploring hospital admissions, mortality, disease severity, length of stay, and population-based hospitalization rates in Portugal.

![R](https://img.shields.io/badge/R-%3E%3D4.3-blue)
![Shiny](https://img.shields.io/badge/Shiny-App-blue)
![License](https://img.shields.io/badge/License-GPLv3-green)

---

## Overview

**BDMH-Explore** is an interactive Shiny dashboard developed as part of the PhD thesis of **Felipe Barletta**.

The application allows users to explore nationwide hospital admissions in mainland Portugal using dynamic maps, interactive graphics and summary statistics.

The dashboard was designed to facilitate exploratory analyses of hospitalization patterns, mortality, disease severity and length of stay.

---

## Features

- Interactive municipality maps
- Hospital admission rates per 1,000 inhabitants
- Disease severity distribution
- In-hospital mortality
- Length of stay summaries
- Temporal trends (2010–2018)
- Dynamic filtering by:
  - Municipality
  - Year
  - Major Diagnostic Category (MDC)

---

## Data

The application uses anonymized data obtained from the Portuguese Hospital Morbidity Database (BDMH).

**Important**

The original hospitalization database is **not included** in this repository due to data protection and confidentiality agreements.

Only auxiliary files required to run the application (population tables and shapefiles) are distributed.

---

## Repository structure

```
BDMH-Explore/
│
├── app.R
├── .gitignore
├── gadm36_PRT_shp/
├── pop2010_2012.csv
├── pop2013_2018.csv
└── LICENSE
```

---

## Requirements

Required R packages include

```r
shiny
sf
leaflet
dplyr
ggplot2
plotly
DT
```

---

## Running the application

Clone the repository

```bash
git clone git@github.com:felipebarlettaPhD/BDMH-Explore.git
```

Open R and run

```r
shiny::runApp()
```

---

## Citation

If you use this software in academic work, please cite:

> Barletta, F. (2026). *BDMH-Explore: Interactive dashboard for Portuguese hospital admissions.* PhD Thesis, Instituto Superior Técnico, University of Lisbon.

---

## Author

**Felipe Barletta**

PhD Candidate in Statistics

Instituto Superior Técnico

University of Lisbon

LinkedIn

https://www.linkedin.com/in/felipe-emanoel-barletta-mendes-b54914b2/

---

## License

This project is licensed under the GNU General Public License v3.0 (GPL-3.0).
