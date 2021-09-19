The `R` package, `COVIDYPLL`, aims to calculate COVID-19 death rates and years of potential life lost (YPLL) due to COVID-19 using the county-level death data by age and quarter in the United States in 2020 coupled with 1,000 posterior sample sets for the suppressed COVID-19 death counts ranging 1-9 in the county-level dataset. 

# Background

This package calculates COVID-19 death rates and YPLL rates due to COVID-19 in 2020 by various county-level characteristics in the United States by aggregating the county-level COVID-19 deaths by age and quarter in 2020. The county-level COVID-19 deaths by age and quarter for 2020 were summarized by the National Center for Health Statistics (NCHS), accessed [here](https://data.cdc.gov/NCHS/AH-Provisional-COVID-19-Deaths-by-Quarter-County-a/ypxr-mz8e). Due to the NCHS confidentiality standard, COVID-19 death counts between 1 and 9 are suppressed in the county-level dataset. We provided 1,000 posterior sample sets for each suppressed COVID-19 death count in the county-level dataset using Bayesian imputation methods. The Bayesian imputation method is described [elsewhere](). 

# Install package

The following `R` code is used to install the package from this repository. 

```
remote::install_github("syzoekao/COVIDYPLL")
```

# Workflow 

Here are the steps of using this package. 

1. Created a `data.table` that contains 1,000 posterior sample sets for suppressed data combined with unsuppressed data in the county-level COVID-19 death dataset. 

    ```
    death_samp <- bind1000samples()
    ```
    
    The output `data.table` inclueds the following information. 
    
    ```
    > head(death_samp)
       age_group row_ix fips urban_rural_code quarter covid_19_deaths state svi_cate pop_size   le2020
    1:     18-29      1 1001     Medium metro       1               0    AL moderate     8410 59.72679
    2:     18-29      2 1001     Medium metro       2               0    AL moderate     8410 59.72679
    3:     18-29      3 1001     Medium metro       3               0    AL moderate     8410 59.72679
    4:     18-29      4 1001     Medium metro       4               0    AL moderate     8410 59.72679
    5:     30-39      5 1001     Medium metro       1               0    AL moderate     7369 48.50004
    6:     30-39      6 1001     Medium metro       2               1    AL moderate     7369 48.50004
         le2018   le2017   avg_le std_pop_wgt simno      rle
    1: 61.42574 61.31019 61.36797   0.2157466     1 61.36797
    2: 61.42574 61.31019 61.36797   0.2157466     1 61.36797
    3: 61.42574 61.31019 61.36797   0.2157466     1 61.36797
    4: 61.42574 61.31019 61.36797   0.2157466     1 61.36797
    5: 50.06246 49.97609 50.01927   0.2045175     1 50.01927
    6: 50.06246 49.97609 50.01927   0.2045175     1 50.01927
    ```
    
    Here is the explanation of the columns: 
    
      * `age_group`: contains 7 age groups, including 18-29, 30-39, 40-49, 50-64, 65-74, 75-84, 85+. 
      * `row_ix`: row indices. 
      * `fips`: county fips code. 
      * `urban_rural_code`: urbanicity of each county provided in the NCSH dataset that summarized the county-level COVID-19 deaths by age and quarter for 2020. 
      * `quarter`: quarter of the row record in 2020
      * `covid_19_deaths`: contained COVID-19 death count for each row. The death count could be either the unsuppressed data from the NCHS dataset or imputed posterior sample set from the Bayesian imputation method. 
      * `state`: abbreviation of U.S. states or localities, including 50 states, Washington DC, and New York City (NYC).  
      * `svi_cate`: County social vulnerability, including low, moderate, and high. 
      * `pop_size`: Population size by county and age group. Data from [ACS](https://data.census.gov/cedsci/table?t=Populations%20and%20People&g=0100000US.050000_0500000US22033,22071,22087,22095&tid=ACSST5Y2019.S0101&hidePreview=true). 
      * `avg_le2020`: Remaining life expectancy by age group, estimated using death data in 2020. 
      * `avg_le2018`: Remaining life expectancy by age group in 2018. 
      * `avg_le2017`: Remaining life expectancy by age group in 2017.
      * `avg_le`: 

2. 









