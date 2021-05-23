rm(list = ls())

library(COVIDYPLL)
library(data.table)
library(ggplot2)
library(rstan)
library(MASS)
library(tidyverse)
library(brms)
library(ggpubr)

# ### Using brms to get the stan code to base on
# mort2020$state_num <- as.numeric(as.factor(mort2020$state))
# mort2020$fips_num <- as.numeric(as.factor(mort2020$fips))
# mort2020[, l_pop_size := log(pop_size + 1)]
# mort2020 <- mort2020[!is.na(covid_19_deaths)]
#
# data_ls <- list(
#   y = mort2020$covid_19_death,
#   q2 = 1 * (mort2020$quarter == 2), # quarters
#   q3 = 1 * (mort2020$quarter == 3), # quarters
#   q4 = 1 * (mort2020$quarter == 4), # quarters
#   state = mort2020$state,
#   county = mort2020$fips,
#   l_pop_size = mort2020$l_pop_size
# )
# b_model <- bf(y ~ 1 + q2 + q3 + q4 + l_pop_size + (1 | state/county),
#               hu ~ 1 + q2 + q3 + q4 + (1 | state/county))
#
# stan_data <- make_standata(b_model,
#                            data = data_ls,
#                            family = Gamma(link = "log"),
#                            prior = c(prior(normal(0,10), class = "Intercept"),
#                                      prior(normal(0,1), class = "b"),
#                                      prior(gamma(0.01, 0.01), class = "shape")))
#
#
# # fit the model
# time_begin <- Sys.time()
# m2 <- brm(data = data_ls,
#           family = hurdle_gamma,
#           b_model,
#           prior = c(prior(normal(0,10), class = "Intercept"),
#                     prior(normal(0,1), class = "b"),
#                     prior(gamma(0.01, 0.01), class = "shape")),
#           iter = 10, chains = 1, cores = 1,
#           seed = 20200518)
# print(Sys.time() - time_begin)
# # saveRDS(m2, "m2.RDS")
#
# stancode(m2)
#
# stan_data <- standata(m2)


#### Create stan model
mort2020$state_num <- as.numeric(as.factor(mort2020$state))
mort2020$fips_num <- as.numeric(as.factor(mort2020$fips))

mort2020$urban_rural_code <- factor(mort2020$urban_rural_code,
                                    levels = c("Noncore", "Medium metro", "Small metro",
                                               "Large fringe metro", "Micropolitan",
                                               "Large central metro"))
mort2020$quarter <- factor(mort2020$quarter)
mort2020[, l_pop_size := log(pop_size + 1)]

X <- model.matrix( ~ age_group * quarter + urban_rural_code + l_pop_size, data = mort2020)

mort2020[, y := copy(covid_19_deaths)]

data_ls <- list(
  N = nrow(mort2020),
  Y = ifelse(is.na(mort2020$y), Inf, mort2020$y),
  Jmi = which(is.na(mort2020$y)),
  Nmi = length(which(is.na(mort2020$y))),
  K = ncol(X),
  X = X,
  Z_1_1 = rep(1, nrow(mort2020)),
  Z_2_1 = rep(1, nrow(mort2020)),
  K_hu = ncol(X),
  X_hu = X,
  Z_3_hu_1 = rep(1, nrow(mort2020)),
  Z_4_hu_1 = rep(1, nrow(mort2020)),
  J_1 = mort2020$state_num,
  J_2 = mort2020$fips_num,
  J_3 = mort2020$state_num,
  J_4 = mort2020$fips_num,
  N_1 = length(unique(mort2020$state_num)),
  M_1 = 1,
  N_2 = length(unique(mort2020$fips_num)),
  M_2 = 1,
  N_3 = length(unique(mort2020$state_num)),
  M_3 = 1,
  N_4 = length(unique(mort2020$fips_num)),
  M_4 = 1
)

begin_time <- Sys.time()
fit_hurdle <- stan(
  file = "inst/impute/stan/impute_hurdle.stan",  # Stan program
  data = data_ls,         # named list of data
  chains = 4,             # number of Markov chains
  warmup = 1000,          # number of warmup iterations per chain
  iter = 3000,            # total number of iterations per chain
  cores = 4,              # number of cores (could use one per chain)
  refresh = 10,
  seed = 20210519
)
print(Sys.time() - begin_time)

saveRDS(fit_hurdle, "inst/impute/results/fit_hurdle.RDS")




