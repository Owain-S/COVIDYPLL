rm(list = ls())

library(COVIDYPLL)
library(ggplot2)
library(data.table)

dat_ls <- data(package = "COVIDYPLL")
dat_ls <- dat_ls$results[, "Item"]
data(list = dat_ls, package = "COVIDYPLL")


# Calculate suppress values in each quarter
mort2020[, suppress_covid_19_deaths := ifelse(is.na(covid_19_deaths), "suppressed",
                                             ifelse(covid_19_deaths == 0, "non-suppressed: 0", "non-suppressed: positive"))]
mort2020[, quarter := paste0("Q", quarter)]

sum_suppress <- mort2020[, list(N = .N), by = .(quarter, suppress_covid_19_deaths)]
sum_suppress[, pct := round(N / sum(N) * 100, 1), by = .(quarter)]
sum_suppress[, label := ifelse(pct < 5, NA, paste0(pct, "%"))]
sum_suppress$label[sum_suppress$quarter == 1 & sum_suppress$suppress_covid_19_deaths == "non-suppressed: positive"] <- NA
sum_suppress <- sum_suppress[order(quarter, suppress_covid_19_deaths)]

ggplot(data = sum_suppress, aes(x = quarter, y = pct)) +
  geom_bar(aes(fill = suppress_covid_19_deaths),
           color = "gray30", stat = "identity", position = "stack", size = 0.5) +
  geom_text(aes(x = quarter, y = pct, label = label,
                group = suppress_covid_19_deaths),
            stat = "identity", position = "stack", vjust = 1.5, size = 6) +
  scale_fill_manual(values = set_colors(3)) +
  ylab("% of Data") +
  xlab("Quarter") +
  ggtitle("Distribution of suppressed and non-suppressed COVID-19 deaths") +
  theme_bw() +
  theme(plot.title = element_text(size = 18, hjust = 0.5),
        strip.text.x = element_text(size = 14, colour = "gray20"),
        strip.background = element_rect(colour = NA, fill = "white"),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.position = "bottom")
ggsave("inst/impute/results/pre_impute/dist_supp_non_supp.png", height = 6, width = 8)


mort2020[, suppress_covid_19_deaths := ifelse(is.na(covid_19_deaths), "suppressed",
                                             ifelse(covid_19_deaths == 0, "non-suppressed: 0", "non-suppressed: positive"))]
sum_suppress <- mort2020[, list(N = .N), by = .(state, quarter, suppress_covid_19_deaths)]
sum_suppress[, pct := round(N / sum(N) * 100, 1), by = .(state, quarter)]
sum_suppress[, label := paste0(pct, "%")]
sum_suppress <- sum_suppress[order(quarter, suppress_covid_19_deaths)]

ggplot(data = sum_suppress, aes(x = quarter, y = pct)) +
  geom_bar(aes(fill = suppress_covid_19_deaths),
           color = "gray30", stat = "identity", position = "stack", size = 0.5) +
  scale_fill_manual(values = set_colors(3)) +
  facet_wrap(.~state, scale = "free", ncol = 10) +
  ylab("% of Data") +
  xlab("Quarter") +
  ggtitle("State distribution of suppressed and non-suppressed COVID-19 deaths") +
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
ggsave("inst/impute/results/pre_impute/dist_supp_non_supp_state.png", height = 8, width = 14)


mort2020[, suppress_covid_19_deaths := ifelse(is.na(covid_19_deaths), "suppressed",
                                              ifelse(covid_19_deaths == 0, "non-suppressed: 0", "non-suppressed: positive"))]
sum_suppress <- mort2020[, list(N = .N), by = .(urban_rural_code, quarter, suppress_covid_19_deaths)]
sum_suppress[, pct := round(N / sum(N) * 100, 1), by = .(urban_rural_code, quarter)]
sum_suppress[, label := ifelse(pct < 10, NA, paste0(pct, "%"))]
sum_suppress <- sum_suppress[order(quarter, suppress_covid_19_deaths)]

ggplot(data = sum_suppress, aes(x = quarter, y = pct)) +
  geom_bar(aes(fill = suppress_covid_19_deaths),
           color = "gray30", stat = "identity", position = "stack", size = 0.5) +
  geom_text(aes(x = quarter, y = pct, label = label,
                group = suppress_covid_19_deaths),
            stat = "identity", position = "stack", vjust = 1.5) +
  scale_fill_manual(values = set_colors(3)) +
  facet_wrap(.~urban_rural_code, scale = "free") +
  ylab("% of Data") +
  xlab("Quarter") +
  ggtitle("Urban-rural code: distribution of suppressed and non-suppressed COVID-19 deaths") +
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
ggsave("inst/impute/results/pre_impute/dist_supp_non_supp_rucc.png", height = 6, width = 10)


sum_suppress <- mort2020[, list(N = .N), by = .(urban_rural_code, age_group, quarter, suppress_covid_19_deaths)]
sum_suppress[, pct := round(N / sum(N) * 100, 1), by = .(urban_rural_code, age_group, quarter)]
sum_suppress[, label := ifelse(pct < 10, NA, paste0(pct, "%"))]
sum_suppress <- sum_suppress[order(quarter, suppress_covid_19_deaths)]

ggplot(data = sum_suppress, aes(x = quarter, y = pct)) +
  geom_bar(aes(fill = suppress_covid_19_deaths),
           color = "gray30", stat = "identity", position = "stack", size = 0.5) +
  scale_fill_manual(values = set_colors(3)) +
  facet_wrap(.~urban_rural_code + age_group, scale = "free") +
  ylab("% of Data") +
  xlab("Quarter") +
  ggtitle("Distribution of suppressed and non-suppressed COVID-19 deaths") +
  theme_bw() +
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        strip.text.x = element_text(size = 12, colour = "gray20"),
        strip.background = element_rect(colour = NA, fill = "white"),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        legend.position = "bottom")
ggsave("inst/impute/results/pre_impute/dist_supp_non_supp_rucc_age.png", height = 10, width = 14)


# Calculate suppress values in each quarter by age group
mort2020[, suppress_covid_19_deaths := ifelse(is.na(covid_19_deaths), "suppressed",
                                              ifelse(covid_19_deaths == 0, "non-suppressed: 0", "non-suppressed: positive"))]
sum_suppress <- mort2020[, list(N = .N), by = .(quarter, age_group, suppress_covid_19_deaths)]
sum_suppress[, pct := round(N / sum(N) * 100, 1), by = .(quarter, age_group)]
sum_suppress[, label := ifelse(pct < 10, NA, paste0(pct, "%"))]
sum_suppress <- sum_suppress[order(quarter, suppress_covid_19_deaths)]

ggplot(data = sum_suppress, aes(x = quarter, y = pct)) +
  geom_bar(aes(fill = suppress_covid_19_deaths),
           color = "gray30", stat = "identity", position = "stack", size = 0.5) +
  geom_text(aes(x = quarter, y = pct, label = label,
                group = suppress_covid_19_deaths),
            stat = "identity", position = "stack", vjust = 1.5) +
  scale_fill_manual(values = set_colors(3)) +
  facet_wrap(.~age_group) +
  ylab("% of Data") +
  xlab("Quarter") +
  ggtitle("Age group: distribution of suppressed and non-suppressed COVID-19 deaths") +
  theme_bw() +
  theme(plot.title = element_text(size = 18, hjust = 0.5),
        strip.text.x = element_text(size = 14, colour = "gray20"),
        strip.background = element_rect(colour = NA, fill = "white"),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.position = "bottom")
ggsave("inst/impute/results/pre_impute/dist_supp_non_supp_age.png", height = 8, width = 10)


# Distribution by quarters and age group
master_dt <- data.table(expand.grid(y_cate = paste0(c(0:19, "20+")),
                                    age_group = levels(mort2020$age_group),
                                    quarter = c(1:4)))
master_dt$y_cate <- factor(master_dt$y_cate, levels = paste0(c(0:19, "20+")))
mort2020[, y_cate := as.character(covid_19_deaths)]
mort2020[, y_cate := ifelse(covid_19_deaths >= 20, "20+", y_cate)]
mort2020$y_cate <- factor(mort2020$y_cate, levels = paste0(c(0:19, "20+")))
sum_dist <- mort2020[, list(N = .N), by = .(age_group, quarter, y_cate)]
sum_dist[, pct := N/sum(N), by = .(age_group, quarter)]
sum_dist <- sum_dist[!is.na(y_cate)]
master_dt <- merge(master_dt, sum_dist, by = c("age_group", "quarter", "y_cate"), all.x = T)
master_dt[, `:=` (quarter = paste0("Q", quarter),
                  age_group = paste0("Age", age_group))]


ggplot(data = master_dt[age_group %in% c("Age30-39", "Age75-84")]) +
  geom_bar(aes(x = y_cate, y = pct),
           stat = "identity", fill = "gray", color = "gray30") +
  facet_grid(age_group ~ quarter, scale = "free") +
  xlab("Number of COVID-19 Deaths") +
  ylab("Proportion of Counties") +
  ggtitle("Distribution of county-level COVID-19 deaths (selected age groups)") +
  theme_bw() +
  theme(plot.title = element_text(size = 18, hjust = 0.5),
        strip.text = element_text(size = 12, colour = "gray20"),
        strip.background = element_rect(colour = NA, fill = "white"),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = "bottom")
ggsave("inst/impute/results/pre_impute/dist_covid19deaths_quarter_age.png",
       height = 5, width = 18)



ggplot(data = master_dt[y_cate != "0" &
                          age_group %in% c("Age30-39", "Age75-84")]) +
  geom_bar(aes(x = y_cate, y = pct),
           stat = "identity", fill = "gray", color = "gray30") +
  facet_grid(age_group ~ quarter, scale = "free") +
  xlab("Number of COVID-19 Deaths") +
  ylab("Proportion of Counties") +
  ggtitle("Distribution of county-level COVID-19 deaths (positive only; selected age groups)") +
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
ggsave("inst/impute/results/pre_impute/dist_covid19deaths_quarter_age (pos only).png",
       height = 5, width = 18)




# Distribution by quarters and urban rural characteristics
master_dt <- data.table(expand.grid(y_cate = paste0(c(0:19, "20+")),
                                    urban_rural_code = unique(mort2020$urban_rural_code)))
master_dt$y_cate <- factor(master_dt$y_cate, levels = paste0(c(0:19, "20+")))
mort2020[, y_cate := as.character(covid_19_deaths)]
mort2020[, y_cate := ifelse(covid_19_deaths >= 20, "20+", y_cate)]
mort2020$y_cate <- factor(mort2020$y_cate, levels = paste0(c(0:19, "20+")))
sum_dist <- mort2020[, list(N = .N), by = .(urban_rural_code, y_cate)]
sum_dist[, pct := N/sum(N), by = .(urban_rural_code)]
sum_dist <- sum_dist[!is.na(y_cate)]
master_dt <- merge(master_dt, sum_dist, by = c("urban_rural_code", "y_cate"), all.x = T)

ggplot(data = master_dt) +
  geom_bar(aes(x = y_cate, y = pct),
           stat = "identity", fill = "gray", color = "gray30") +
  facet_wrap(.~urban_rural_code, scale = "free_y") +
  xlab("Number of COVID-19 Deaths") +
  ylab("Proportion of Counties") +
  ggtitle("Urban rural code: Distribution of county-level COVID-19 deaths by quarters") +
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
ggsave("inst/impute/results/pre_impute/dist_covid19deaths_quarter_rucc.png", height = 6, width = 14)

ggplot(data = master_dt[y_cate != "0"]) +
  geom_bar(aes(x = y_cate, y = pct),
           stat = "identity", fill = "gray", color = "gray30") +
  facet_wrap(.~urban_rural_code, scale = "free_y") +
  xlab("Number of COVID-19 Deaths") +
  ylab("Proportion of Counties") +
  ggtitle("Type of county: Distribution ofcounty-level COVID-19 deaths by quarters (positive only)") +
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
ggsave("inst/impute/results/pre_impute/dist_covid19deaths_quarter_rucc (pos only).png", height = 6, width = 14)



