rm(list = ls())

library(COVIDYPLL)
library(data.table)

std_pop_wgt <- dl_us_standard_population()
le2020 <- get_provisional_le()
county_pop <- get_county_pop_size()
mort2020 <- get_covid_death()
impute_sample <- readRDS("inst/impute/bayes_impute.RDS")
impute_sample_qr <- readRDS("inst/impute/bayes_impute_qr.RDS")

# FIPS 2270 (Wade Hampton Census Area, AK) and and 46113 (Shannon County, SD)
# are not found in the census (county_pop) data
mort2020 <- get_covid_death()
mort2020[, `:=` (county = NULL, state = NULL)]
mort2020 <- merge(mort2020, county_pop, by = c("fips", "age_group"), all.x = T)
# removing the two FIPS that had no population information
mort2020 <- mort2020[!fips %in% c(2270, 46113)]

usethis::use_data(std_pop_wgt, overwrite = T)
usethis::use_data(le2020, overwrite = T)
usethis::use_data(mort2020, overwrite = T)
usethis::use_data(impute_sample, overwrite = T)
usethis::use_data(impute_sample_qr, overwrite = T)

devtools::document()
package_loc <- devtools::build()
install.packages(package_loc, repos = NULL)
file.remove("/Users/zoekao/Documents/COVIDYPLL_0.0.0.9000.tar.gz")

