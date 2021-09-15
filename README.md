The `R` package, `COVIDYPLL`, aims to calculate COVID-19 death rates and years of potential life lost (YPLL) due to COVID-19 using the county-level death data by age and quarter in the United States in 2020 coupled with 1,000 posterior sample sets for the suppressed COVID-19 death counts ranging 1-9 in the county-level dataset. 

# Background

This package calculates COVID-19 death rates and YPLL rates due to COVID-19 in 2020 by various county-level characteristics in the United States by aggregating the county-level COVID-19 deaths by age and quarter in 2020. The county-level COVID-19 deaths by age and quarter for 2020 are summarized by the National Center for Health Statistics (NCHS), accessed [here](https://data.cdc.gov/NCHS/AH-Provisional-COVID-19-Deaths-by-Quarter-County-a/ypxr-mz8e). Due to the NCHS confidentiality standard, counts (COVID-19 death counts and total death counts) between 1 and 9 are suppressed. We provided 1,000 posterior sample sets for each suppressed COVID-19 death count in the county-level dataset using Bayesian imputation methods. The Bayesian imputation is described [elsewhere](). 

# Install package

The following `R` code installs the package from this repository. 

```
remote::install_github("syzoekao/COVIDYPLL")
```






