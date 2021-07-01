rm(list = ls())

library(COVIDYPLL)
library(data.table)

std_pop_wgt <- dl_us_standard_population()
# le <- get_provisional_le() #  data calculated based on https://www.cdc.gov/nchs/data/vsrr/VSRR10-508.pdf
le <- calculate_provisional_le() # life expectancy in 2018 and 2020
county_pop <- get_county_pop_size()
covid19d_cty <- get_covid_death_cty()
mort2020 <- get_mort_nation_state()
uninsure <- get_uninsure_pop()
impute_sample <- readRDS("inst/impute/bayes_impute_agg2.RDS") # aggregate results

# FIPS 2270 (Wade Hampton Census Area, AK) and and 46113 (Shannon County, SD)
# are not found in the census (county_pop) data
covid19d_cty[, `:=` (county = NULL, state = NULL)]
covid19d_cty <- merge(covid19d_cty, county_pop, by = c("fips", "age_group"), all.x = T)
# removing the two FIPS that had no population information
covid19d_cty <- covid19d_cty[!fips %in% c(2270, 46113)]
covid19d_cty[, row_ix := c(1:.N)]
covid19d_cty <- merge(covid19d_cty, uninsure, by = c("fips"), all.x = T)
covid19d_cty <- covid19d_cty[order(row_ix)]
covid19d_cty[, row_ix := NULL]

usethis::use_data(std_pop_wgt, overwrite = T)
usethis::use_data(le, overwrite = T)
usethis::use_data(covid19d_cty, overwrite = T)
usethis::use_data(mort2020, overwrite = T)
usethis::use_data(impute_sample, overwrite = T)

devtools::document()
package_loc <- devtools::build()
install.packages(package_loc, repos = NULL)
file.remove("/Users/zoekao/Documents/COVIDYPLL_0.0.0.9000.tar.gz")

