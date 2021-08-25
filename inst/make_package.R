rm(list = ls())

library(COVIDYPLL)
library(data.table)
library(parallel)

std_pop_wgt <- dl_us_standard_population()
# le <- get_provisional_le() #  data calculated based on https://www.cdc.gov/nchs/data/vsrr/VSRR10-508.pdf
le <- calculate_provisional_le() # life expectancy in 2018 and 2020
county_pop <- get_county_pop_size()
covid19d_cty <- get_covid_death_cty()
mort2020 <- get_mort_nation_state()
uninsure <- get_uninsure_pop()
impute_sample <- readRDS("inst/impute/bayes_impute_agg8.RDS") # aggregate results

# FIPS 2270 (Wade Hampton Census Area, AK) and and 46113 (Shannon County, SD)
# are not found in the census (county_pop) data
covid19d_cty[, `:=` (county = NULL, state = NULL)]
covid19d_cty <- merge(covid19d_cty, county_pop, by = c("fips", "age_group"), all.x = T)
# removing the two FIPS that had no population information
covid19d_cty <- covid19d_cty[!fips %in% c(2270, 46113)]
covid19d_cty[, row_ix := c(1:.N)]
covid19d_cty <- merge(covid19d_cty, uninsure, by = c("fips"), all.x = T)
covid19d_cty <- covid19d_cty[order(row_ix)]

# Clean policy data
policy <- readRDS("/Users/zoekao/Documents/CDC COVID/YPLLproj/Data/policy_dt.RDS")
keep_col <- c("fips", "quarter",
              "mask_acc1", "sah_acc1", "GB_acc1",
              "svi_overall", "svi_ses", "svi_household", "svi_minority", "svi_housing",
              # "ep_pov", "ep_unemp", "ep_pci", "ep_nohsdp", "ep_age65", "ep_age17", "ep_disabl",
              # "ep_sngpnt", "ep_minrty", "ep_limeng", "ep_munit", "ep_mobile", "ep_crowd", "ep_noveh",
              # "ep_groupq",
              "svi_overall_ter", "svi_ses_ter", "svi_household_ter", "svi_minority_ter", "svi_housing_ter")
policy <- policy[, ..keep_col]
setnames(policy, c("mask_acc1", "sah_acc1", "GB_acc1",
                   "svi_overall", "svi_ses", "svi_household", "svi_minority", "svi_housing",
                   "svi_overall_ter", "svi_ses_ter", "svi_household_ter", "svi_minority_ter", "svi_housing_ter"),
         c("Mask_acc", "SAH_acc", "GB_acc",
           "svi_num", paste0("theme", c(1:4), "_num"),
           "svi_cate", paste0("theme", c(1:4), "_cate")))


covid19d_cty <- merge(covid19d_cty, unique(policy[, .(fips, svi_cate)]), by = "fips", all.x = T)
covid19d_cty <- covid19d_cty[order(row_ix)]

policy[, svi_cate := NULL]

usethis::use_data(std_pop_wgt, overwrite = T)
usethis::use_data(le, overwrite = T)
usethis::use_data(covid19d_cty, overwrite = T)
usethis::use_data(mort2020, overwrite = T)
usethis::use_data(impute_sample, overwrite = T)
usethis::use_data(policy, overwrite = T)

devtools::document()
package_loc <- devtools::build()
install.packages(package_loc, repos = NULL)
file.remove("/Users/zoekao/Documents/COVIDYPLL_0.0.0.9000.tar.gz")

