rm(list = ls())

library(COVIDYPLL)
library(ggplot2)

dat_ls <- data(package = "COVIDYPLL")
dat_ls <- dat_ls$results[, "Item"]
data(list = dat_ls, package = "COVIDYPLL")

# Calculate suppress values in each quarter
mort2020[, suppress_covid_19_deaths := ifelse(is.na(covid_19_deaths), "suppressed",
                                             ifelse(covid_19_deaths == 0, "non-suppressed: 0", "non-suppressed: positive"))]
sum_suppress <- mort2020[, list(N = .N), by = .(quarter, suppress_covid_19_deaths)]
sum_suppress[, pct := round(N / sum(N) * 100, 1), by = .(quarter)]
sum_suppress[, label := paste0(pct, "%")]
sum_suppress$label[sum_suppress$quarter == 1 & sum_suppress$suppress_covid_19_deaths == "non-suppressed: positive"] <- NA
sum_suppress <- sum_suppress[order(quarter, suppress_covid_19_deaths)]

ggplot(data = sum_suppress, aes(x = quarter, y = pct)) +
  geom_bar(aes(fill = suppress_covid_19_deaths),
           color = "gray30", stat = "identity", position = "stack", size = 0.5) +
  geom_text(aes(x = quarter, y = pct, label = label,
                group = suppress_covid_19_deaths),
            stat = "identity", position = "stack", vjust = 1.5) +
  scale_fill_manual(values = set_colors(3)) +
  ylab("%") +
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
  ylab("%") +
  ggtitle("Distribution of suppressed and non-suppressed COVID-19 deaths") +
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


mort2020[, suppress_covid_19_deaths := ifelse(is.na(covid_19_deaths), "suppressed",
                                              ifelse(covid_19_deaths == 0, "non-suppressed: 0", "non-suppressed: positive"))]
sum_suppress <- mort2020[, list(N = .N), by = .(urban_rural_code, quarter, suppress_covid_19_deaths)]
sum_suppress[, pct := round(N / sum(N) * 100, 1), by = .(urban_rural_code, quarter)]
sum_suppress[, label := ifelse(pct < 4, NA, paste0(pct, "%"))]
sum_suppress <- sum_suppress[order(quarter, suppress_covid_19_deaths)]

ggplot(data = sum_suppress, aes(x = quarter, y = pct)) +
  geom_bar(aes(fill = suppress_covid_19_deaths),
           color = "gray30", stat = "identity", position = "stack", size = 0.5) +
  geom_text(aes(x = quarter, y = pct, label = label,
                group = suppress_covid_19_deaths),
            stat = "identity", position = "stack", vjust = 1.5) +
  scale_fill_manual(values = set_colors(3)) +
  facet_wrap(.~urban_rural_code, scale = "free") +
  ylab("%") +
  ggtitle("Distribution of suppressed and non-suppressed COVID-19 deaths") +
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



mort2020[, l_covid_19_deaths := log(covid_19_deaths)]

ggplot(data = mort2020) +
  geom_histogram(aes(x = l_covid_19_deaths, y = ..density..),
                 fill = set_colors(1)) +
  geom_vline(xintercept = log(9), color = "gray20", size = 0.3) +
  scale_x_continuous(breaks = c(0:8), labels = c(0:8)) +
  facet_wrap(.~quarter, scale = "free_y") +
  ggtitle("Distribution of COVID-19 deaths, log transformed") +
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


ggplot(data = mort2020[covid_19_deaths <= 200 & covid_19_deaths > 0]) +
  geom_histogram(aes(x = covid_19_deaths, y = ..density..),
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

ggplot(data = mort2020) +
  geom_point(aes(y = covid_19_deaths, x = total_deaths)) +
  facet_wrap(.~quarter, scale = "free") +
  ggtitle("Relationship between total deaths and COVID-19 deaths") +
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


