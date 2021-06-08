rm(list = ls())

library(COVIDYPLL)
library(ggplot2)
library(data.table)
library(parallel)

dat_ls <- data(package = "COVIDYPLL")
dat_ls <- dat_ls$results[, "Item"]
data(list = dat_ls, package = "COVIDYPLL")

ix_miss <- impute_sample_qr$ix_miss
ymis <- impute_sample_qr$ymis_draws

calculate_ypll <- function(dt) {
  if (!is.data.table(dt)) stop("This is not data.table")
  calc_columns <- c("covid_19_deaths", "avg_le2020", "pop_size", "std_pop_wgt")
  if (!all(calc_columns %in% colnames(sum_dt))) stop("check whether the columns has \'covid_19_deaths\', \'avg_le2020\', \'pop_size\', \'std_pop_wgt\'")
  dt[, ypll := (((covid_19_deaths * avg_le2020) / pop_size) * 100000) * std_pop_wgt]
  dt[pop_size == 0]$ypll <- 0
  dt
}


## YPLL1: ignore NAs
sum_dt <- mort2020[, list(covid_19_deaths = sum(covid_19_deaths, na.rm = T)),
                   by = .(fips, age_group, pop_size)]
sum_dt <- merge(sum_dt, le2020[raceth == "All" & sex == "total", .(age_group, avg_le2020)],
                by = c("age_group"), all.x = T)
sum_dt <- merge(sum_dt, std_pop_wgt[, .(age_group, std_pop_wgt)],
                by = c("age_group"), all.x = T)

ypll1_dt <- calculate_ypll(sum_dt)
ypll1_dt <- ypll1_dt[, list(sum_covid19_deaths = sum(covid_19_deaths),
              sum_ypll = sum(ypll)), by = .(age_group)]
ypll1_dt[, type := "No imputation"]


## YPLL2: Bayesian impute
ypll2_dt <- mclapply(c(1:nrow(ymis)), function(x) {
  mort2020$covid_19_deaths[ix_miss] <- ymis[x, ]
  sum_dt <- mort2020[, list(covid_19_deaths = sum(covid_19_deaths, na.rm = T)),
                     by = .(fips, age_group, pop_size)]
  sum_dt <- merge(sum_dt, le2020[raceth == "All" & sex == "total", .(age_group, avg_le2020)],
                  by = c("age_group"), all.x = T)
  sum_dt <- merge(sum_dt, std_pop_wgt[, .(age_group, std_pop_wgt)],
                  by = c("age_group"), all.x = T)

  sum_dt <- calculate_ypll(sum_dt)
  sum_dt[, list(sum_covid19_deaths = sum(covid_19_deaths),
                sum_ypll = sum(ypll)), by = .(age_group)][, sim := x]
}, mc.cores = 6)

ypll2_dt <- rbindlist(ypll2_dt)
ypll2_dt <- ypll2_dt[, list(sum_covid19_deaths = mean(sum_covid19_deaths),
                            lb_d = quantile(sum_covid19_deaths, 0.025),
                            ub_d = quantile(sum_covid19_deaths, 0.975),
                            sum_ypll = mean(sum_ypll),
                            lb_ypll = quantile(sum_ypll, 0.025),
                            ub_ypll = quantile(sum_ypll, 0.975)),
                     by = .(age_group)]
ypll2_dt[, type := "Bayesian imputation"]


## YPLL3: Uniform distribution
ypll3_dt <- mclapply(c(1:nrow(ymis)), function(x) {
  mort2020$covid_19_deaths[ix_miss] <- sample(c(1:9), length(ix_miss), rep = T)
  sum_dt <- mort2020[, list(covid_19_deaths = sum(covid_19_deaths, na.rm = T)),
                     by = .(fips, age_group, pop_size)]
  sum_dt <- merge(sum_dt, le2020[raceth == "All" & sex == "total", .(age_group, avg_le2020)],
                  by = c("age_group"), all.x = T)
  sum_dt <- merge(sum_dt, std_pop_wgt[, .(age_group, std_pop_wgt)],
                  by = c("age_group"), all.x = T)

  sum_dt <- calculate_ypll(sum_dt)
  sum_dt[, list(sum_covid19_deaths = sum(covid_19_deaths),
                sum_ypll = sum(ypll)), by = .(age_group)][, sim := x]
}, mc.cores = 6)

ypll3_dt <- rbindlist(ypll3_dt)
ypll3_dt <- ypll3_dt[, list(sum_covid19_deaths = mean(sum_covid19_deaths),
                            lb_d = quantile(sum_covid19_deaths, 0.025),
                            ub_d = quantile(sum_covid19_deaths, 0.975),
                            sum_ypll = mean(sum_ypll),
                            lb_ypll = quantile(sum_ypll, 0.025),
                            ub_ypll = quantile(sum_ypll, 0.975)),
                     by = .(age_group)]
ypll3_dt[, type := "Uniform imputation"]


ypll_all <- rbindlist(list(ypll1_dt, ypll2_dt, ypll3_dt), use.names = TRUE, fill = TRUE)
ypll_all[, type := factor(type,
                          levels = c("No imputation", "Bayesian imputation", "Uniform imputation"))]

ggplot(data = ypll_all) +
  geom_bar(aes(x = age_group, y = round(sum_covid19_deaths), fill = type),
           stat = "identity", position = "dodge2", size = 0.8) +
  scale_fill_manual(values = c("gray50", "darkgoldenrod1", "deepskyblue")) +
  scale_y_continuous(breaks = seq(0, 120000, 20000), labels = scales::comma) +
  xlab("Age Group") +
  ylab("Number of COVID-19 Deaths") +
  ggtitle("Total number of COVID-19 deaths by age group") +
  theme_bw() +
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        strip.text.x = element_text(size = 12, colour = "gray20"),
        strip.text.y = element_text(size = 12, colour = "gray20"),
        strip.background = element_rect(colour = NA, fill = "white"),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = "right")
# ggsave(paste0("inst/impute/results/total covid19 deaths_age.png"),
#        device = "png", height = 5, width = 10)

ggplot(data = ypll_all) +
  geom_bar(aes(x = age_group, y = round(sum_ypll), fill = type),
           stat = "identity", position = "dodge2", size = 0.8) +
  scale_fill_manual(values = c("gray50", "darkgoldenrod1", "deepskyblue")) +
  scale_y_continuous(breaks = seq(0, 3000000, 500000), labels = scales::comma) +
  xlab("Age Group") +
  ylab("Years of Potential Life Lost") +
  ggtitle("Total years of potential life loss due to COVID-19 by age group") +
  theme_bw() +
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        strip.text.x = element_text(size = 12, colour = "gray20"),
        strip.text.y = element_text(size = 12, colour = "gray20"),
        strip.background = element_rect(colour = NA, fill = "white"),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = "right")
# ggsave(paste0("inst/impute/results/total YPLL_age.png"),
#        device = "png", height = 5, width = 10)






