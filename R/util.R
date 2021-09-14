#' @title Common colors
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
#' @param return_cut Whehter to return the cut labels
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

#' @title Combine covid19d_cty and 1000 imputation samples
#' @import data.table
#' @export
bind1000samples <- function() {
  death_dt <- copy(covid19d_cty)
  death_dt <- death_dt[, .(row_ix, fips, age_group, urban_rural_code, quarter, covid_19_deaths,
                           state, svi_cate, pop_size)]
  death_dt <- merge(death_dt, le, by = c("age_group"), all.x = T)
  death_dt <- merge(death_dt, std_pop_wgt[, .(age_group, std_pop_wgt)], by = c("age_group"), all.x = T)
  death_dt <- death_dt[order(row_ix)]

  ix_miss <- impute_sample$ix_miss
  ymis <- impute_sample$ymis_draws

  out_ls <- lapply(c(1:nrow(ymis)), function(x) {
    tmp_dt <- copy(death_dt)
    tmp_dt$covid_19_deaths[ix_miss] <- ymis[x, ]
    tmp_dt[, simno := x]
    return(tmp_dt)
  })
  out_dt <- rbindlist(out_ls)

  return(out_dt)
}

#' @title Calculate YPLL
#' @import data.table
#' @export
calculate_ypll_old <- function(dt) {
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

#' @title Summarize variables to get mean or the 95% intervals
#' @param x A vector of values
#' @export
summarize_vars <- function(x) {
  list(mean = mean(x, na.rm = T),
       lb = quantile(x, prob = 0.025, na.rm = T),
       ub = quantile(x, prob = 0.975, na.rm = T))
}

#' @title Calculate and aggregate COVID-19 death (rate) and years of potential life lost (rate) by county characteristics
#' @param dt A `data.table` include the following columns: `covid_19_deaths`, `pop_size`, `std_pop_wgt`, `avg_leXXXX` and other variables that could be used for summary statistics
#' @param byvar The variable used for aggregating deaths, YPLL, and population size
#' @param age_adjusted_output Whether to get age adjusted death rate or YPLL rate
#' @param year_rle Specify the year of remaining life expectancy data. The default is the average remaining life expectancy between 2017 and 2018.
#' @param export_data_by_simno Whether to export simulation data (1,000 datasets).
#' @import data.table dplyr
#' @return If `export_data_by_simno` is set to `FALSE`, this function returns a `data.table`.
#'         If `export_data_by_simno` is set to `TRUE`, this function returns a list of two `data.table`.
#'         The first `data.table` named `agg_sum` summarizes mean and interval estimates of
#'         each statistics of interest. The second `data.table` named `sim_dt` provides all statistics
#'         calculated for each of the 1,000 datasets based on the 1,000 posterior samples. In general,
#'         the `data.table` returned includes the total COVID-19 deaths (`covid_19_deaths`),
#'         COVID-19 death rate (`covid19_death_rate`), YPLL rate (`ypll_rate`),
#'         and total YPLL (`tot_ypll`). If the input argument, `age_adjusted_output`, is set to
#'         `TRUE`, the `data.table` returned includes columns `covid19_death_rate_aa` and `ypll_rate_aa`,
#'         which are age adjusted (`_aa`) rates. If the input argument, `age_adjusted_output`, is set to
#'         `FALSE`, the `data.table` returned includes columns `covid19_death_rate_agewt` and
#'         `ypll_rate_agewt`, which are age weights (`_agewt`) that can be used for
#'         further calculation.
#' @export
calculate_ypll <- function(dt,
                           byvar = NULL,
                           age_adjusted_output = TRUE,
                           year_rle = NA, # Default using average LE between 2017 and 2018
                           export_data_by_simno = FALSE) {
  if (!is.data.table(dt)) stop("This is not data.table")
  calc_columns <- c("covid_19_deaths", "pop_size", "std_pop_wgt")
  criteria <- all(calc_columns %in% colnames(dt)) & (length(grep("avg_le", c("avg_le2020", "avg_le2018", "avg_le2017"))) > 0)
  if (!criteria) {
    stop("check whether the columns has \'covid_19_deaths\', \'pop_size\', \'std_pop_wgt\', or any of the \'avg_leXXXX\'")
  }

  if (is.na(year_rle)) {
    dt[, rle := avg_le]
  } else {
    if (year_rle == 2017) dt[, rle := avg_le2017]
    if (year_rle == 2018) dt[, rle := avg_le2018]
    if (year_rle == 2020) dt[, rle := avg_le2020]
  }

  if (!is.null(byvar)) {
    if ("age_group" %in% byvar) {
      age_adjusted_output <- FALSE
      warnings("No age adjusted results are produced")
    }
  }

  by_vars <- unique(c("simno", "fips", byvar, "age_group"))

  pop_size <- unique(dt[simno == 1, .(fips, age_group, pop_size)])
  sum_dt <- dt[, list(covid_19_deaths = sum(covid_19_deaths),
                      rle = mean(rle),
                      std_pop_wgt = mean(std_pop_wgt)),
               by = by_vars]
  sum_dt <- merge(sum_dt, pop_size, by = c("fips", "age_group"), all.x = T)

  by_vars <- unique(c("simno", "age_group", byvar))

  sum_dt <- sum_dt[, list(covid_19_deaths = sum(covid_19_deaths),
                          pop_size = sum(pop_size),
                          rle = mean(rle),
                          std_pop_wgt = mean(std_pop_wgt)),
                   by = c(by_vars)]

  sum_dt[, `:=` (covid19_death_rate = ((covid_19_deaths / ((pop_size > 0) * pop_size +
                                                             (pop_size == 0) * 1)) * 100000),
                 tot_ypll = covid_19_deaths * rle,
                 ypll_rate = ((covid_19_deaths / ((pop_size > 0) * pop_size +
                                                    (pop_size == 0) * 1)) * 100000)  * rle)]
  sum_dt[, `:=` (covid19_death_rate_agewt = covid19_death_rate * std_pop_wgt,
                 ypll_rate_agewt = ypll_rate * std_pop_wgt)]

  if (isTRUE(age_adjusted_output)) {
    sum_vars <- c("covid_19_deaths", "covid19_death_rate_agewt", "tot_ypll", "ypll_rate_agewt")
    mean_vars <- c("covid19_death_rate", "ypll_rate")
    by_vars <- unique(c("simno", byvar))
    agg_dt0 <- sum_dt[, lapply(.SD, sum), by = by_vars, .SDcols = sum_vars]
    agg_dt1 <- sum_dt[, lapply(.SD, mean), by = by_vars, .SDcols = mean_vars]

    agg_dt <- merge(agg_dt0, agg_dt1, by = by_vars)

    out_vars <- c(sum_vars, mean_vars)
    agg_sum <- agg_dt[, as.list(unlist(lapply(.SD, summarize_vars))), by = byvar, .SDcols = out_vars]
    sum_dt <- agg_dt
    colnames(sum_dt) <- gsub("agewt", "aa", colnames(sum_dt))
  } else {
    out_vars <- c("covid_19_deaths", "covid19_death_rate",
                  "covid19_death_rate_agewt", "tot_ypll", "ypll_rate", "ypll_rate_agewt")
    by_vars <- by_vars[!by_vars %in% c("simno")]
    agg_sum <- sum_dt[, as.list(unlist(lapply(.SD, summarize_vars))), by = by_vars, .SDcols = out_vars]
  }
  colnames(agg_sum) <- gsub(".97.5%", "", colnames(agg_sum))
  colnames(agg_sum) <- gsub(".2.5%", "", colnames(agg_sum))
  colnames(agg_sum) <- gsub("\\.", "_", colnames(agg_sum))
  colnames(agg_sum) <- gsub("agewt", "aa", colnames(agg_sum)) # age adjusted

  if (isTRUE(export_data_by_simno)) {
    out_dt <- list(agg_sum = agg_sum, sim_dt = sum_dt)
    return(out_dt)
  } else {
    return(agg_sum)
  }
}
