context("bind1000samples")

library(COVIDYPLL)
library(data.table)

death_samp <- bind1000samples()

# test the class
test_that("output from bind1000samples should be a data.table", {
  expect_equal(is.data.table(death_samp), TRUE)
})

year_rle <- NA
test_that("output from calculate_ypll", {
  system.time(county_ypll <- calculate_ypll(dt = death_samp, byvar = "fips", year_rle = year_rle,
                                age_adjusted_output = TRUE, export_data_by_simno = TRUE))
  expect_equal(length(county_ypll), 2)
  expect_equal(max(county_ypll$sim_dt$simno), 1000)
})

# system.time(county_ypll_quarter <- calculate_ypll(dt = death_samp, byvar = c("fips", "quarter"),
#                                                   year_rle = year_rle,
#                                       age_adjusted_output = TRUE, export_data_by_simno = TRUE))
# system.time(county_ypll_age <- calculate_ypll(dt = death_samp, byvar = c("fips", "quarter"),
#                                               year_rle = year_rle, age_adjusted_output = FALSE,
#                                               export_data_by_simno = TRUE))


