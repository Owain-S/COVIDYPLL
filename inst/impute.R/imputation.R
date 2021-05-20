rm(list = ls())

library(COVIDYPLL)
library(data.table)
library(ggplot2)
library(rstan)
library(MASS)
library(tidyverse)
library(brms)

#### Using brms to get the stan code to base on
# mort2020$state_num <- as.numeric(as.factor(mort2020$state))
# mort2020$fips_num <- as.numeric(as.factor(mort2020$fips))
#
# data_ls <- list(
#   y = mort2020$covid_19_death + 1,
#   q2 = 1 * (mort2020$quarter == 2), # quarters
#   q3 = 1 * (mort2020$quarter == 3), # quarters
#   q4 = 1 * (mort2020$quarter == 4), # quarters
#   state = mort2020$state,
#   county = mort2020$fips
# )
# b_model <- bf(y | mi() ~ 1 + q2 + q3 + q4 + (1 | state/county))
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
#           family = Gamma(link = "log"),
#           b_model,
#           prior = c(prior(normal(0,10), class = "Intercept"),
#                     prior(normal(0,10), class = "b"),
#                     prior(gamma(0.01, 0.01), class = "shape")),
#           iter = 1000, chains = 2, cores = 2,
#           # backend = "cmdstanr", threads = threading(2),
#           seed = 20200518)
# print(Sys.time() - time_begin)
# saveRDS(m2, "m2.RDS")
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

X <- model.matrix( ~ age_group * quarter + urban_rural_code, data = mort2020)

mort2020[, y := covid_19_deaths + 1]

data_ls <- list(
  N = nrow(mort2020),
  Y = ifelse(is.na(mort2020$y), Inf, mort2020$y),
  Jmi = which(is.na(mort2020$y)),
  Nmi = length(which(is.na(mort2020$y))),
  K = ncol(X),
  X = X,
  Z_1_1 = rep(1, nrow(mort2020)),
  Z_2_1 = rep(1, nrow(mort2020)),
  J_1 = mort2020$state_num,
  J_2 = mort2020$fips_num,
  N_1 = length(unique(mort2020$state_num)),
  M_1 = 1,
  N_2 = length(unique(mort2020$fips_num)),
  M_2 = 1,
  prior_only = 0
)

begin_time <- Sys.time()
fit1 <- stan(
  file = "inst/impute.R/stan/impute_mlm.stan",  # Stan program
  data = data_ls,         # named list of data
  chains = 4,             # number of Markov chains
  warmup = 2000,          # number of warmup iterations per chain
  iter = 10000,            # total number of iterations per chain
  cores = 4,              # number of cores (could use one per chain)
  refresh = 100,
  seed = 20210519
)
print(Sys.time() - begin_time)

saveRDS(fit1, "fit1.RDS")

pars <- c("b", "Intercept", "b_Intercept", "shape") #, "sd_1", "z_1", "sd_2", "z_2", "r_1_1", "r_2_1")
# print(fit1, pars = fit1@model_pars)
print(fit1, pars = pars, probs = c(0.025, 0.5, 0.975))

ix_ymis <- grep("Ymi", names(fit1@sim$samples[[1]]))
samp_ymis <- unlist(lapply(ix_ymis, function(x) {
  y <- fit1@sim$samples[[1]][[x]]
  x_samp <- sample(c(1:length(y)), 1)
  y[x_samp]
}))
mean(samp_ymis)
quantile(samp_ymis, prob = c(0.1, 0.5, 0.9))

mort2020[, y_new := y - 1]
tmp_y <- ceiling(samp_ymis) - 1
tmp_y[tmp_y == 0] <- 1
tmp_y[tmp_y > 10] <- 9
mort2020$y_new[is.na(mort2020$covid_19_deaths)] <- tmp_y

ggplot(data = mort2020[y_new <= 50]) +
  geom_histogram(aes(x = y_new, y = ..density..),
                 fill = set_colors(1)) +
  geom_vline(xintercept = 10, color = "gray20", size = 0.3) +
  scale_x_continuous(breaks = c(0, 10, seq(20, 200, 20)), labels = c(0, 10, seq(20, 200, 20))) +
  facet_wrap(.~quarter, scale = "free_y") +
  ggtitle("Distribution of COVID-19 deaths (<= 200 cases)") +
  theme_bw() +
  theme(plot.title = element_text(size = 18, hjust = 0.5),
        strip.text.x = element_text(size = 12, colour = "gray20"),
        strip.background = element_rect(colour = NA, fill = "white"),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = "bottom")

traceplot(fit1, pars = c("b", "Intercept", "b_Intercept", "shape"))




post <- posterior_samples(fit1, add_chain = T)

print(fit1, pars=c("theta", "mu", "tau", "lp__"), probs=c(.1,.5,.9))



