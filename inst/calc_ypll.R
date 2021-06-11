rm(list = ls())

library(COVIDYPLL)
library(ggplot2)
library(data.table)
library(parallel)
library(openxlsx)

dat_ls <- data(package = "COVIDYPLL")
dat_ls <- dat_ls$results[, "Item"]
data(list = dat_ls, package = "COVIDYPLL")

ix_miss <- impute_sample_agg$ix_miss
ymis <- impute_sample_agg$ymis_draws


set.seed(20210610)

## YPLL1: ignore NAs
sum_dt <- covid19d_cty[, list(covid_19_deaths = sum(covid_19_deaths, na.rm = T)),
                   by = .(fips, age_group, pop_size)]
sum_dt <- merge(sum_dt, le[, .(age_group, avg_le2020)],
                by = c("age_group"), all.x = T)
sum_dt <- merge(sum_dt, std_pop_wgt[, .(age_group, std_pop_wgt)],
                by = c("age_group"), all.x = T)

ypll1_dt <- calculate_ypll(sum_dt)
ypll1_dt <- ypll1_dt[, list(sum_covid19_deaths = sum(covid_19_deaths),
              sum_ypll = sum(ypll)), by = .(age_group)]
ypll1_dt[, type := "No imputation"]


## YPLL2: Bayesian impute
ypll2_dt <- mclapply(c(1:nrow(ymis)), function(x) {
  covid19d_cty$covid_19_deaths[ix_miss] <- ymis[x, ]
  sum_dt <- covid19d_cty[, list(covid_19_deaths = sum(covid_19_deaths, na.rm = T)),
                     by = .(fips, age_group, pop_size)]
  sum_dt <- merge(sum_dt, le[, .(age_group, avg_le2020)],
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
  covid19d_cty$covid_19_deaths[ix_miss] <- sample(c(1:9), length(ix_miss), rep = T)
  sum_dt <- covid19d_cty[, list(covid_19_deaths = sum(covid_19_deaths, na.rm = T)),
                     by = .(fips, age_group, pop_size)]
  sum_dt <- merge(sum_dt, le[, .(age_group, avg_le2020)],
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
ggsave(paste0("inst/impute/results/total covid19 deaths_age_agg.png"),
       device = "png", height = 5, width = 10)

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
ggsave(paste0("inst/impute/results/total YPLL_age_agg.png"),
       device = "png", height = 5, width = 10)


ypll_all <- merge(ypll_all, mort2020[state == "US", .(age_group, covid_19_deaths)],
                  by = c("age_group"), all.x = T)
ypll_all <- ypll_all[order(type, age_group)]
ypll_all[, pct_diff := round((sum_covid19_deaths - covid_19_deaths) / covid_19_deaths * 100, 2)]

write.xlsx(ypll_all, "inst/impute/results/covid19deaths_impute_age_group.xlsx", row.names = F)

# % overestimation
(sum(ypll_all$sum_covid19_deaths[ypll_all$type == "Bayesian imputation"]) -
    sum(mort2020[state == "US"]$covid_19_deaths)) / sum(mort2020[state == "US"]$covid_19_deaths)


## State level distribution
state_sum_dt <- mclapply(c(1:nrow(ymis)), function(x) {
  covid19d_cty$covid_19_deaths[ix_miss] <- ymis[x, ]
  sum_dt <- covid19d_cty[, list(covid_19_deaths = sum(covid_19_deaths, na.rm = T)),
                         by = .(state, age_group)]
  return(sum_dt)
}, mc.cores = 6)

state_sum_dt <- rbindlist(state_sum_dt)
state_sum_dt <- state_sum_dt[, list(m = mean(covid_19_deaths),
                                    lb_d = quantile(covid_19_deaths, 0.025),
                                    ub_d = quantile(covid_19_deaths, 0.975)),
                     by = .(state, age_group)]

state_sum_dt <- merge(state_sum_dt, mort2020[, .(state, age_group, covid_19_deaths)],
                      by = c("state", "age_group"), all.x = T)

cor(state_sum_dt$m[state_sum_dt$age_group %in% c("18-29", "30-39")],
    state_sum_dt$covid_19_deaths[state_sum_dt$age_group %in% c("18-29", "30-39")])
plot(state_sum_dt$m[state_sum_dt$age_group %in% c("18-29", "30-39")],
     state_sum_dt$covid_19_deaths[state_sum_dt$age_group %in% c("18-29", "30-39")])


setnames(state_sum_dt, c("m", "covid_19_deaths"), c("predicted", "data"))

state_sum_dt_long <- melt(state_sum_dt, id.vars = c("state", "age_group"),
                          variable.name = "type",
                          measure.vars = c("predicted", "data"))

state_sum_dt[, type := "predicted"]

state_sum_dt_long <- merge(state_sum_dt_long,
                           state_sum_dt[, .(state, age_group, type, lb_d, ub_d)],
                           by = c("state", "age_group", "type"),
                           all.x = T)
state_sum_dt[, type := NULL]


ggplot(data = state_sum_dt_long) +
  geom_bar(aes(x = age_group, y = value, fill = type),
           stat = "identity", color = "gray30",
           position = position_dodge(width = 0.9), size = 0.3) +
  scale_fill_manual(values = c("gray", "deepskyblue")) +
  facet_wrap(~ state, scales = "free_y", ncol = 8) +
  ylab("number of COVID-19 deaths") +
  theme_bw() +
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        strip.text = element_text(size = 12, colour = "gray20"),
        strip.background = element_rect(colour = NA, fill = "white"),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        axis.text.x = element_text(size = 8, angle = 90, vjust = 0.5),
        axis.text.y = element_text(size = 8),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14),
        legend.position = "bottom")
ggsave(paste0("inst/impute/results/state_covid19d_dist_agg.png"),
       device = "png", height = 8, width = 14)



