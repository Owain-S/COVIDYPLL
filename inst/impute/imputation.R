rm(list = ls())

library(COVIDYPLL)
library(data.table)
library(ggplot2)
library(rstan)
library(MASS)
library(tidyverse)
library(brms)

# ### Using brms to get the stan code to base on
# mort2020$state_num <- as.numeric(as.factor(mort2020$state))
# mort2020$fips_num <- as.numeric(as.factor(mort2020$fips))
# mort2020[, l_pop_size := log(pop_size + 1)]
#
# data_ls <- list(
#   y = mort2020$covid_19_death + 1,
#   q2 = 1 * (mort2020$quarter == 2), # quarters
#   q3 = 1 * (mort2020$quarter == 3), # quarters
#   q4 = 1 * (mort2020$quarter == 4), # quarters
#   state = mort2020$state,
#   county = mort2020$fips,
#   l_pop_size = mort2020$l_pop_size
# )
# b_model <- bf(y | mi() ~ 1 + q2 + q3 + q4 + l_pop_size + (1 | state/county))
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
#           iter = 100, chains = 1, cores = 1,
#           # backend = "cmdstanr", threads = threading(2),
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
  M_2 = 1
)

begin_time <- Sys.time()
fit1 <- stan(
  file = "inst/impute/stan/impute_mlm.stan",  # Stan program
  data = data_ls,         # named list of data
  chains = 4,             # number of Markov chains
  warmup = 1000,          # number of warmup iterations per chain
  iter = 2000,            # total number of iterations per chain
  cores = 4,              # number of cores (could use one per chain)
  refresh = 10,
  seed = 20210519
)
print(Sys.time() - begin_time)

saveRDS(fit1, "inst/impute/results/fit1.RDS")


pars <- c("b", "b_Intercept", "shape")
sum_estimates <- round(summary(fit1, pars = pars, probs = c(0.025, 0.975))$summary, 3)
# saveRDS(sum_estimates, "inst/impute/results/sum_estimates.RDS")

traceplot(fit1, pars = c("b", "b_Intercept", "shape"))
# ggsave("inst/impute/results/traceplot of fixed effects.tiff", device = "tiff",
#        height = 12, width = 14)


library(parallel)

ix_ymis <- grep("Ymi", names(fit1@sim$samples[[1]]))
ymis_draws <- mclapply(ix_ymis, function(x) {
  y <- c(fit1@sim$samples[[1]][[x]][2001:fit1@sim$iter],
         fit1@sim$samples[[2]][[x]][2001:fit1@sim$iter],
         fit1@sim$samples[[3]][[x]][2001:fit1@sim$iter],
         fit1@sim$samples[[4]][[x]][2001:fit1@sim$iter])
  round(y)
}, mc.cores = 6)

set.seed(521)
x_samp <- sample(c(1:length(ix_ymis)), 1000, rep = F)

ymis_draws <- mclapply(ymis_draws, function(x) {
  x[x_samp]
}, mc.cores = 6)
ymis_draws <- do.call(cbind, ymis_draws)

# Calculate 95% credible intervals for the proportion of counties with 1-9 COVID-19 deaths
mort2020[, suppress := ifelse(is.na(covid_19_deaths), "suppressed", "non-suppressed")]
mort2020[, y_new := y - 1]

covid19deaths_dist <- mclapply(c(1:nrow(ymis_draws)), function(x) {
  tmp_y <- ymis_draws[x, ] - 1
  mort2020$y_new[is.na(mort2020$covid_19_deaths)] <- tmp_y

  mort2020[, y_cate := as.character(y_new)]
  mort2020[, y_cate := ifelse(y_new >= 20, "20+", y_cate)]
  mort2020$y_cate <- factor(mort2020$y_cate, levels = paste0(c(0:19, "20+")))

  out <- mort2020[, list(N = .N), by = .(y_cate, quarter)]
  out[, pct := N / sum(N), by = .(quarter)]
  out[, suppress := ifelse(y_cate %in% paste0(c(1:9)), "suppressed", "non-suppressed")]
  out[order(y_cate, quarter)]
}, mc.cores = 6)

covid19deaths_dist <- rbindlist(covid19deaths_dist)
covid19deaths_sum <- covid19deaths_dist[, list(mean_pct = mean(pct),
                                               lb = quantile(pct, prob = 0.025),
                                               ub = quantile(pct, prob = 0.975)),
                                        by = .(y_cate, quarter)]
covid19deaths_sum[, suppress := ifelse(y_cate %in% paste0(c(1:9)), "imputed", "non-suppressed")]

g1 <- ggplot(data = covid19deaths_sum) +
  geom_bar(aes(x = y_cate, y = mean_pct, fill = suppress),
           stat = "identity", color = "gray30") +
  facet_wrap(.~quarter, scale = "free_y") +
  xlab("number of COVID-19 deaths") +
  ylab("proportion of counties") +
  ggtitle("(A) Distribution of COVID-19 deaths by quarters") +
  scale_fill_manual(values = c("deepskyblue", "gray")) +
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

g2 <- ggplot(data = covid19deaths_sum[y_cate != "0"]) +
  geom_bar(aes(x = y_cate, y = mean_pct, fill = suppress),
           stat = "identity", color = "gray30") +
  geom_errorbar(aes(x = y_cate, ymin = lb, ymax = ub),
                width = 0, color = "gray30", size = 0.5) +
  facet_wrap(.~quarter, scale = "free_y") +
  xlab("number of COVID-19 deaths") +
  ylab("proportion of counties") +
  ggtitle("(B) Distribution of COVID-19 deaths by quarters (death counts >0)") +
  scale_fill_manual(values = c("deepskyblue", "gray")) +
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
ggarrange(g1, g2, nrow = 2, common.legend = T)
ggsave("inst/impute/results/impute distribution.tiff", device = "tiff", height = 12, width = 10)


# Calculate 95% credible intervals for the proportion of counties with 1-9 COVID-19 deaths
covid19deaths_dist <- mclapply(c(1:nrow(ymis_draws)), function(x) {
  tmp_y <- ymis_draws[x, ] - 1
  mort2020$y_new[is.na(mort2020$covid_19_deaths)] <- tmp_y

  mort2020[, y_cate := as.character(y_new)]
  mort2020[, y_cate := ifelse(y_new >= 20, "20+", y_cate)]
  mort2020$y_cate <- factor(mort2020$y_cate, levels = paste0(c(0:19, "20+")))

  out <- mort2020[, list(N = .N), by = .(y_cate, age_group)]
  out[, pct := N / sum(N), by = .(age_group)]
  out[, suppress := ifelse(y_cate %in% paste0(c(1:9)), "suppressed", "non-suppressed")]
  out[order(y_cate, age_group)]
}, mc.cores = 6)

covid19deaths_dist <- rbindlist(covid19deaths_dist)
covid19deaths_sum <- covid19deaths_dist[, list(mean_pct = mean(pct),
                                               lb = quantile(pct, prob = 0.025),
                                               ub = quantile(pct, prob = 0.975)),
                                        by = .(y_cate, age_group)]
covid19deaths_sum[, suppress := ifelse(y_cate %in% paste0(c(1:9)), "imputed", "non-suppressed")]

g1 <- ggplot(data = covid19deaths_sum) +
  geom_bar(aes(x = y_cate, y = mean_pct, fill = suppress),
           stat = "identity", color = "gray30") +
  facet_wrap(.~age_group, scale = "free_y") +
  xlab("number of COVID-19 deaths") +
  ylab("proportion of counties") +
  ggtitle("(A) Distribution of COVID-19 deaths by age group") +
  scale_fill_manual(values = c("deepskyblue", "gray")) +
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

g2 <- ggplot(data = covid19deaths_sum[y_cate != "0"]) +
  geom_bar(aes(x = y_cate, y = mean_pct, fill = suppress),
           stat = "identity", color = "gray30") +
  geom_errorbar(aes(x = y_cate, ymin = lb, ymax = ub),
                width = 0, color = "gray30", size = 0.5) +
  facet_wrap(.~age_group) + #, scale = "free_y") +
  xlab("number of COVID-19 deaths") +
  ylab("proportion of counties") +
  ggtitle("(B) Distribution of COVID-19 deaths by age group (death counts >0)") +
  scale_fill_manual(values = c("deepskyblue", "gray")) +
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
ggarrange(g1, g2, nrow = 2, common.legend = T)
ggsave("inst/impute/results/impute distribution_age.tiff", device = "tiff", height = 12, width = 10)


# Calculate 95% credible intervals for the proportion of counties with 1-9 COVID-19 deaths
covid19deaths_dist <- mclapply(c(1:nrow(ymis_draws)), function(x) {
  tmp_y <- ymis_draws[x, ] - 1
  mort2020$y_new[is.na(mort2020$covid_19_deaths)] <- tmp_y

  mort2020[, y_cate := as.character(y_new)]
  mort2020[, y_cate := ifelse(y_new >= 20, "20+", y_cate)]
  mort2020$y_cate <- factor(mort2020$y_cate, levels = paste0(c(0:19, "20+")))

  out <- mort2020[, list(N = .N), by = .(y_cate, urban_rural_code)]
  out[, pct := N / sum(N), by = .(urban_rural_code)]
  out[, suppress := ifelse(y_cate %in% paste0(c(1:9)), "suppressed", "non-suppressed")]
  out[order(y_cate, urban_rural_code)]
}, mc.cores = 6)

covid19deaths_dist <- rbindlist(covid19deaths_dist)
covid19deaths_sum <- covid19deaths_dist[, list(mean_pct = mean(pct),
                                               lb = quantile(pct, prob = 0.025),
                                               ub = quantile(pct, prob = 0.975)),
                                        by = .(y_cate, urban_rural_code)]
covid19deaths_sum[, suppress := ifelse(y_cate %in% paste0(c(1:9)), "imputed", "non-suppressed")]

g1 <- ggplot(data = covid19deaths_sum) +
  geom_bar(aes(x = y_cate, y = mean_pct, fill = suppress),
           stat = "identity", color = "gray30") +
  facet_wrap(.~urban_rural_code, scale = "free_y") +
  xlab("number of COVID-19 deaths") +
  ylab("proportion of counties") +
  ggtitle("(A) Distribution of COVID-19 deaths by urban rural characteristics") +
  scale_fill_manual(values = c("deepskyblue", "gray")) +
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

g2 <- ggplot(data = covid19deaths_sum[y_cate != "0"]) +
  geom_bar(aes(x = y_cate, y = mean_pct, fill = suppress),
           stat = "identity", color = "gray30") +
  geom_errorbar(aes(x = y_cate, ymin = lb, ymax = ub),
                width = 0, color = "gray30", size = 0.5) +
  facet_wrap(.~urban_rural_code) + #, scale = "free_y") +
  xlab("number of COVID-19 deaths") +
  ylab("proportion of counties") +
  ggtitle("(B) Distribution of COVID-19 deaths by urban rural characteristics\n(death counts >0)") +
  scale_fill_manual(values = c("deepskyblue", "gray")) +
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
ggarrange(g1, g2, nrow = 2, common.legend = T)
ggsave("inst/impute/results/impute distribution_rucc.tiff", device = "tiff", height = 12, width = 10)








