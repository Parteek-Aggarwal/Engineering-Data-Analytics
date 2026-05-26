\# Greenhouse Gas Concentrations \& Temperature Variation in Tasmania

\*\*Hippogriff Group | Adelaide University | ENGI 5006\*\*



A Comparative Analysis of CO₂, CH₄ \& N₂O (1996–2025)



\*\*Authors:\*\* Parteek Aggarwal, Vaibhavi Bhosale, Ishmam Salim, Sinsha, Shahid Afridi



\---



\## Repository Structure



```

├── data/               # Raw CSV datasets

│   ├── CO2.csv

│   ├── CH4.csv

│   ├── N2O.csv

│   └── Temp.csv

│

├── analytics/          # R analysis script

│   └── hippogriff\_analysis.R

│

├── figures/            # All plots (auto-generated)

│   ├── trend\_\*.png

│   ├── qq\_\*.png

│   └── regression\_\*.png

│

└── outputs/            # Stats and model results (auto-generated)

&#x20;   ├── descriptive\_statistics.csv

&#x20;   ├── pearson\_correlation\_matrix.csv

&#x20;   ├── pearson\_correlation\_pvalues.csv

&#x20;   ├── simple\_regression\_results.csv

&#x20;   └── multiple\_regression\_results.csv

```



\## How to Run



1\. Clone the repository

2\. Open R or RStudio and set the working directory to the \*\*root\*\* of the repo

3\. Install required packages if not already installed:

```r

install.packages(c("ggplot2", "car"))

```

4\. Run the script:

```r

source("analytics/hippogriff\_analysis.R")

```

All figures will be saved to `figures/` and all results to `outputs/` automatically.



\## Data Sources



\- \*\*Greenhouse Gases:\*\* CSIRO – Cape Grim Baseline Air Pollution Station (1996–2025)

\- \*\*Temperature:\*\* Australian Bureau of Meteorology – Bushy Park Station, Tasmania (1996–2025)



