#' @title colors
#' @export
set_colors <- function(n = 2) {
  color_vec <- c("deepskyblue", "yellowgreen", "darkgoldenrod1", "tomato", "plum3")
  color_vec[1:n]
}

#' @title Get the total number of counties in the US (excluding Puerto Rico)
#' @export
get_county_info <- function() {
  n_cnty <- 3142
  return(list(n_cnty = n_cnty))
}

#' @title Specify the age breaks for the population
#' @param age_cut A vector of the numeric lower bounds of all age breaks.
#'        The default is `c(18, 30, 40, 50, 65, 75, 85)`
#' @param max_age The maximum age allowed (numeric). The default is infinite (`Inf`).
#' @return The output of this function is a `data.table` with the lower and upper bounds of the age break,
#'         and a column of string age breaks.
#' @import data.table
#' @export
set_age_breaks <- function(age_cut = c(18, 30, 40, 50, 65, 75, 85),
                           max_age = Inf,
                           return_cut = TRUE) {
  if (!all(unlist(lapply(age_cut, is.numeric)))) stop("Vector of age_cut has non-numeric elements")
  if (max_age < age_cut[length(age_cut)]) stop("max_age has to be bigger than any of the value in age_cut")

  age_lb <- age_cut
  age_ub <- c(age_cut[2:length(age_cut)] - 1, max_age)

  if (return_cut) {
    age_breaks <- c(age_cut, max_age)
    age_labs <- ifelse(is.infinite(age_ub), paste0(age_lb, "+"), paste0(age_lb, "-", age_ub))

    age_breaks <- list(breaks = age_breaks, labels = age_labs)
  } else {
    age_breaks <- data.table::data.table(lb = age_lb, ub = age_ub)
    age_breaks[, brks := ifelse(is.infinite(ub), paste0(lb, "+"), paste0(lb, "-", ub))]
  }
  return(age_breaks)
}

#' @title Match the name of the states and abbreviation
#' @import data.table
#' @export
get_states <- function() {
  state_dt <- data.table(state_name = c(state.name, "District of Columbia"),
                         state = c(state.abb, "DC"))
  state_dt <- state_dt[order(state)]
  state_dt
}

#' @title Vector of NYC fips code (5 counties)
#' @import data.table
#' @export
set_nyc_fips <- function() {
  # information from https://simple.wikipedia.org/wiki/List_of_counties_in_New_York
  # and https://guides.newman.baruch.cuny.edu/nyc_data
  # counties include Bronx County, Kings County, New York County, Queens County, Richmond County
  c(36005, 36047, 36061, 36081, 36085)
}

#' @title Calculate YPLL
#' @import data.table
#' @export
calculate_ypll <- function(dt) {
  if (!is.data.table(dt)) stop("This is not data.table")
  calc_columns <- c("covid_19_deaths", "avg_le2020", "pop_size", "std_pop_wgt")
  if (!all(calc_columns %in% colnames(dt))) stop("check whether the columns has \'covid_19_deaths\', \'avg_le2020\', \'pop_size\', \'std_pop_wgt\'")
  dt[, `:=` (covid19_death_rate = (covid_19_deaths / pop_size) * 100000,
             covid19_death_rate_age_adjusted = (covid_19_deaths / pop_size) * 100000 * std_pop_wgt, # age_adjusted death rate https://www.cdc.gov/nchs/data/statnt/statnt06rv.pdf
             tot_ypll = covid_19_deaths * avg_le2020,
             ypll_rate = ((covid_19_deaths / pop_size) * 100000) * avg_le2020,
             ypll_rate_age_adjusted = ((covid_19_deaths / pop_size) * 100000) * avg_le2020 * std_pop_wgt)]

  dt[pop_size == 0]$covid19_death_rate <- 0
  dt[pop_size == 0]$covid19_death_rate_age_adjusted <- 0
  dt[pop_size == 0]$tot_ypll <- 0
  dt[pop_size == 0]$ypll_rate <- 0
  dt[pop_size == 0]$ypll_rate_age_adjusted <- 0
  dt
}

#' @title Calculate YPLL v2
#' @import data.table
#' @export
calculate_ypll2 <- function(dt, byvar = NULL) {
  if (!is.data.table(dt)) stop("This is not data.table")
  calc_columns <- c("covid_19_deaths", "avg_le2020", "pop_size", "std_pop_wgt")
  if (!all(calc_columns %in% colnames(dt))) stop("check whether the columns has \'covid_19_deaths\', \'avg_le2020\', \'pop_size\', \'std_pop_wgt\'")

  out_dt <- copy(dt)

  out_dt[, `:=` (covid19_death_rate = (covid_19_deaths / pop_size) * 100000,
             covid19_death_rate_age_adjusted = (covid_19_deaths / pop_size) * 100000 * std_pop_wgt, # age_adjusted death rate https://www.cdc.gov/nchs/data/statnt/statnt06rv.pdf
             tot_ypll = covid_19_deaths * avg_le2020,
             ypll_rate = ((covid_19_deaths / pop_size) * 100000) * avg_le2020,
             ypll_rate_age_adjusted = ((covid_19_deaths / pop_size) * 100000) * avg_le2020 * std_pop_wgt)]
  out_dt[pop_size == 0]$covid19_death_rate <- 0
  out_dt[pop_size == 0]$covid19_death_rate_age_adjusted <- 0
  out_dt[pop_size == 0]$tot_ypll <- 0
  out_dt[pop_size == 0]$ypll_rate <- 0
  out_dt[pop_size == 0]$ypll_rate_age_adjusted <- 0

  if (is.null(by)) {
    out_dt[, `:=` (covid19_death_rate_age_adjusted = NULL,
                   ypll_rate_age_adjusted = NULL)]
  } else {
    out_dt <- out_dt[, list(tot_covid_d = sum(covid_19_deaths),
                            covid19_death_rate_age_adjusted = sum(covid19_death_rate_age_adjusted),
                            ypll_rate_age_adjusted = sum(ypll_rate_age_adjusted)),
                     by = byvar]
  }
  out_dt
}

