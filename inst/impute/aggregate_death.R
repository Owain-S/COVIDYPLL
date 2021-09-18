rm(list = ls())

library(COVIDYPLL)
library(ggplot2)
library(data.table)
library(parallel)
library(openxlsx)
library(rgdal)
library(rgeos)
library(viridis)
library(grid)
library(maptools)
library(broom)
library(mapproj)
library(ggthemes)
library(raster)

# models 2, 3, 7, 8, 9, 11, 12, 14, 15, 16, 17, 18
i <- 18

sum_est <- readRDS(paste0("inst/impute/results/sum_estimates_hurdle_agg", i,".RDS"))

temp <- readRDS(paste0("inst/impute/bayes_impute_agg", i,".RDS"))

ix_miss <- temp$ix_miss
ymis <- temp$ymis_draws

set.seed(20210621)

mort_all <- mort2020[state == "US", .(age_group, covid_19_deaths)]
mort_usa <- mort_all[, list(covid_19_deaths = sum(covid_19_deaths))]

death_dt <- mclapply(c(1:nrow(ymis)), function(x) {
  covid19d_cty$covid_19_deaths[ix_miss] <- ymis[x, ]
  sum_dt <- covid19d_cty[, list(covid_19_deaths = sum(covid_19_deaths, na.rm = T)),
                     by = .(fips, age_group, pop_size)]
  sum_dt[, list(sum_covid19_deaths = sum(covid_19_deaths)), by = .(age_group)][, sim := x]
}, mc.cores = 6)

death_dt <- rbindlist(death_dt)
death_dt <- merge(death_dt, mort_all, by = "age_group", all.x = T)

death_dt <- death_dt[, list(sum_covid19_deaths = mean(sum_covid19_deaths),
                            lb_d = quantile(sum_covid19_deaths, 0.025),
                            ub_d = quantile(sum_covid19_deaths, 0.975),
                            pct_diff = mean(round((sum_covid19_deaths - covid_19_deaths)/covid_19_deaths* 100, 2)),
                            pct_lb = quantile(round((sum_covid19_deaths - covid_19_deaths)/covid_19_deaths* 100, 2), 0.025),
                            pct_ub = quantile(round((sum_covid19_deaths - covid_19_deaths)/covid_19_deaths* 100, 2), 0.975)),
                     by = .(age_group)]

death_usa_dt <- mclapply(c(1:nrow(ymis)), function(x) {
  covid19d_cty$covid_19_deaths[ix_miss] <- ymis[x, ]
  sum_dt <- covid19d_cty[, list(covid_19_deaths = sum(covid_19_deaths, na.rm = T)),
                         by = .(fips, age_group, pop_size)]
  sum_dt[, list(sum_covid19_deaths = sum(covid_19_deaths))][, sim := x]
}, mc.cores = 6)

death_usa_dt <- rbindlist(death_usa_dt)
death_usa_dt[, covid_19_deaths := mort_usa[["covid_19_deaths"]]]
death_usa_dt <- death_usa_dt[, list(sum_covid19_deaths = mean(sum_covid19_deaths),
                            lb_d = quantile(sum_covid19_deaths, 0.025),
                            ub_d = quantile(sum_covid19_deaths, 0.975),
                            pct_diff = mean(round((sum_covid19_deaths - covid_19_deaths)/covid_19_deaths* 100, 2)),
                            pct_lb = quantile(round((sum_covid19_deaths - covid_19_deaths)/covid_19_deaths* 100, 2), 0.025),
                            pct_ub = quantile(round((sum_covid19_deaths - covid_19_deaths)/covid_19_deaths* 100, 2), 0.975))]
death_usa_dt[, age_group := "Total"]


death_all <- rbindlist(list(death_dt, death_usa_dt), use.names = TRUE, fill = TRUE)

death_all[, `:=` (covid19death = format(round(sum_covid19_deaths), big.mark = ","),
                  covid19death_ci = paste0("[", format(round(lb_d), big.mark = ","), ", ",
                                    format(round(ub_d), big.mark = ","), "]"),
                  pct_diff = paste0(round(pct_diff, 2), "%"),
                  pct_diff_ci = paste0("[", paste0(round(pct_lb, 2), "%"), ", ",
                                  paste0(round(pct_ub, 2), "%"), "]"))]
death_all <- death_all[, .(age_group, covid19death, covid19death_ci, pct_diff, pct_diff_ci)]

death_all <- melt(death_all, measure = list(c("covid19death", "covid19death_ci"),
                                            c("pct_diff", "pct_diff_ci")),
                  value.name = c("covid19deaths", "pct_diff"))
death_all[, variable := ifelse(variable == 1, "posterior mean", "credible CI")]
death_all[, variable := factor(variable, levels = c("posterior mean", "credible CI"))]
death_all <- death_all[order(age_group, variable)]

## State level distribution
state_sum_dt_all <- mclapply(c(1:nrow(ymis)), function(x) {
  covid19d_cty$covid_19_deaths[ix_miss] <- ymis[x, ]
  sum_dt <- covid19d_cty[, list(covid_19_deaths = sum(covid_19_deaths, na.rm = T)),
                         by = .(state, age_group)]
  sum_dt[, simno := x]
  return(sum_dt)
}, mc.cores = 6)

state_sum_dt_all <- rbindlist(state_sum_dt_all)
state_sum_dt <- state_sum_dt_all[, list(m = mean(covid_19_deaths),
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
state_sum_dt[, hit_target := ifelse(data >= lb_d & data <= ub_d, TRUE, FALSE)]

state_sum_dt_long <- merge(state_sum_dt_long,
                           state_sum_dt[, .(state, age_group, type, lb_d, ub_d)],
                           by = c("state", "age_group", "type"),
                           all.x = T)
state_sum_dt[, type := NULL]
state_sum_dt[, ratio := predicted / data]


state_sum_dt_long$type <- factor(state_sum_dt_long$type, levels = c("predicted", "data"))

g2 <- ggplot(data = state_sum_dt_long) +
  geom_errorbar(aes(x = age_group, ymin = lb_d, ymax = ub_d),
                width = 0.3, color = "gray30", alpha = 0.7) +
  geom_point(aes(x = age_group, y = value, color = type, shape = type, alpha = type),
             size = 1.5, stroke = 1, fill = "white") +
  scale_color_manual(values = c("gray30", "firebrick")) +
  scale_shape_manual(values = c(18, 21)) +
  scale_alpha_manual(values = c(0.7, 1)) +
  facet_wrap(~ state, scales = "free_y", ncol = 8) +
  ylab("Number of COVID-19 Deaths") +
  labs(title = "Number of COVID-19 Deaths by Age and State/Locality (Predicted vs Data)") +
  theme_bw() +
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
        strip.text = element_text(size = 12, colour = "gray20"),
        strip.background = element_rect(colour = NA, fill = "white"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        axis.text.x = element_text(size = 8, angle = 90, vjust = 0.5),
        axis.text.y = element_text(size = 8),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 18),
        legend.position = "bottom")
ggsave(paste0("inst/impute/results/state_covid19d_dist_agg", i, ".png"),
       plot = g2, device = "png", height = 8, width = 14)



## State distribution
setnames(state_sum_dt_all, "covid_19_deaths", "predicted")
state_sum_dt_all <- merge(state_sum_dt_all, mort2020[, .(state, age_group, covid_19_deaths)],
                      by = c("state", "age_group"), all.x = T)
setnames(state_sum_dt_all, "covid_19_deaths", "data")

corr <- paste0(round(cor(state_sum_dt_all$data, state_sum_dt_all$predicted), 3) * 100, "%")

# g1 <- ggplot(data = state_sum_dt_all) +
g1 <- ggplot(data = state_sum_dt) +
  geom_abline(intercept = 0, slope = 1, alpha = 0.8,
              color = "gray30", linetype = 1, size = 0.8) +
  geom_point(aes(x = data, y = predicted), shape = 21,
             size = 1.5, stroke = 1, fill = "white", color = "royalblue") +
  scale_x_continuous(label = scales::comma) +
  scale_y_continuous(label = scales::comma) +
  labs(title = "Correlation between Predicted and Observed\nAnnual COVID-19 Deaths by Age, State or Locality",
       caption = "Note: The predicted COVID-19 deaths are the predicted mean from 1,000 posterior samples.") +
  xlab("\nReported COVID-19 Deaths by State/Locality") +
  ylab("Predicted COVID-19 Deaths by State/Locality\n") +
  geom_label(
    label = paste0("x = y"),
    x = 8500,
    y = 7500,
    label.size = 0.35
  ) +
  geom_label(
    label = paste0("\u03C1", " = ", corr),
    x = 1000,
    y = 8500,
    label.size = 0,
    size = 5
  ) +
  theme_bw() +
  theme(plot.title = element_text(size = 14, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        strip.text = element_text(size = 12, colour = "gray20"),
        strip.background = element_rect(colour = NA, fill = "white"),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        legend.position = "bottom")
ggsave(paste0("inst/impute/results/corr_state_predicted_data", i, ".png"),
       device = "png", plot = g1, height = 5, width = 6)


## State level statistics accuracy
state_sum_dt_all[, sqdiff := (predicted - data)^2] # square difference
state_sum_dt_all[, N := .N, by = .(simno)]
state_sum_dt_all[, rmse := sqrt(sum(sqdiff) / N),
                 by = .(simno)]
accuracy_measure_state <- unique(state_sum_dt_all[, .(simno, rmse, N)])

print("Accuracy measure")
accuracy_measure_state[, mean(rmse)]
accuracy_measure_state[, sd(rmse)]
mean(state_sum_dt$hit_target)

added_dt <- data.table(age_group = c("", "mean of accuracy measure by state",
                                     "sd of accuracy measure by state",
                                     "pct hit state-level targets"),
                       covid19deaths = c(NA, round(accuracy_measure_state[, mean(rmse)], 2),
                                              round(accuracy_measure_state[, sd(rmse)], 2),
                                              round(mean(state_sum_dt$hit_target), 2)))

death_all <- rbindlist(list(death_all, added_dt), use.name = T, fill = T)
write.xlsx(death_all, paste0("inst/impute/results/covid19deaths_impute_age_group", i, ".xlsx"), row.names = F)




## National level distribution
nat_sum_dt_all <- mclapply(c(1:nrow(ymis)), function(x) {
  covid19d_cty$covid_19_deaths[ix_miss] <- ymis[x, ]
  sum_dt <- covid19d_cty[, list(covid_19_deaths = sum(covid_19_deaths, na.rm = T)),
                         by = .(age_group)]
  return(sum_dt)
}, mc.cores = 6)

nat_sum_dt_all <- rbindlist(nat_sum_dt_all)
nat_sum_dt <- nat_sum_dt_all[, list(m = mean(covid_19_deaths),
                                        lb_d = quantile(covid_19_deaths, 0.025),
                                        ub_d = quantile(covid_19_deaths, 0.975)),
                                 by = .(age_group)]

nat_sum_dt <- merge(nat_sum_dt, mort2020[state == "US", .(age_group, covid_19_deaths)],
                      by = c("age_group"), all.x = T)

nat_sum_dt_all <- merge(nat_sum_dt_all, mort2020[state == "US", .(age_group, covid_19_deaths)],
                        by = c("age_group"), all.x = T)

nat_sum_dt_all[, small := covid_19_deaths.x <= covid_19_deaths.y]
nat_sum_dt_all[, list(p = mean(small)), by = .(age_group)]

## Calculate annual distribution of the Bayesian simulation data

death_yr <- mclapply(c(1:nrow(ymis)), function(x) {
  covid19d_cty$covid_19_deaths[ix_miss] <- ymis[x, ]
  sum_dt <- covid19d_cty[, list(covid_19_deaths = sum(covid_19_deaths, na.rm = T)),
                         by = .(fips)]
  sum_dt[, cate := ifelse(covid_19_deaths >= 10, "10+", covid_19_deaths)]
  out_dt <- sum_dt[, list(sum_N = .N), by = .(cate)]
  out_dt[, pct := round(sum_N / sum(sum_N), 4)]
  out_dt[, cate := factor(cate, levels = c(0:9, "10+"))]
  out_dt <- out_dt[order(cate)]
}, mc.cores = 6)

death_yr <- rbindlist(death_yr)

death_yr[, list(m = round(mean(pct), 4)), by = .(cate)]



#### Create Maps
set.seed(504)

ix_miss <- impute_sample$ix_miss
ymis <- impute_sample$ymis_draws

death2020 <- copy(covid19d_cty)
death2020[, row_ix := c(1:.N)]
death2020[, quarter := paste0("Q", quarter)]

death2020 <- merge(death2020, le[, .(age_group, avg_le2020)],
                   by = c("age_group"), all.x = T)
death2020 <- merge(death2020, std_pop_wgt[, .(age_group, std_pop_wgt)],
                   by = c("age_group"), all.x = T)
death2020 <- death2020[order(row_ix)]

death2020[, urban_rural_code := factor(urban_rural_code,
                                       levels = c("Noncore",
                                                  "Micropolitan",
                                                  "Small metro",
                                                  "Medium metro",
                                                  "Large fringe metro",
                                                  "Large central metro"))]

county_state <- unique(death2020[, .(fips, county_name, state, urban_rural_code)])




sum_dt <- mclapply(c(1:dim(ymis)[1]), function(x) {
  tmp_dt <- copy(death2020)
  tmp_dt$covid_19_deaths[ix_miss] <- ymis[x, ]

  tmp_dt <- tmp_dt[, list(covid_19_deaths = sum(covid_19_deaths),
                          pop_size = sum(pop_size),
                          avg_le = mean(avg_le2020),
                          std_pop_wgt = mean(std_pop_wgt)),
                   by = c("fips", "age_group")]

  agg_dt <- calculate_ypll(tmp_dt, byvar = "fips")

  agg_dt
}, mc.cores = 6)

sum_dt <- rbindlist(sum_dt)
sum_dt <- sum_dt[, list(tot_covid_d = mean(tot_covid_d),
                        covid19_death_rate_age_adjusted = mean(covid19_death_rate_age_adjusted),
                        ypll_rate_age_adjusted = mean(ypll_rate_age_adjusted)),
                 by = .(fips)]

sum_dt <- merge(county_state, sum_dt, by = c("fips"))

sum_dt_na <- death2020[, list(tot_covid_d_suppress = sum(covid_19_deaths, na.rm = T),
                              covid_19_na = ifelse(is.na(sum(covid_19_deaths)), 1, 0)),
                       by = .(fips)]

sum_dt <- merge(sum_dt, sum_dt_na, by = "fips", all.x = T)

tmp_brk <- round(quantile(sum_dt$tot_covid_d, prob = c(0.2, 0.4, 0.6, 0.8, 0.95)))
sum_dt[, `COVID-19 Death Counts` := cut(tot_covid_d,
                                        breaks = c(-Inf, tmp_brk, Inf),
                                        labels = paste0(c("0-15", "16-30", "30-54",
                                                          "55-108", "109-479", ">=480"),
                                                        " deaths"))]
sum_dt[, `total COVID-19 deaths suppressed` := cut(tot_covid_d_suppress,
                                        breaks = c(-Inf, tmp_brk, Inf),
                                        labels = paste0(c("0-15", "16-30", "30-54",
                                                          "55-108", "109-479", ">=480"),
                                                        " deaths"))]



## Code from here: https://mathewkiang.com/2017/01/16/using-histogram-legend-choropleths/
## and here: https://rud.is/b/2014/11/16/moving-the-earth-well-alaska-hawaii-with-r/
## Lower 48 states ----
lower_48 <-  c("28", "37", "40", "51", "54", "22", "26", "25", "16", "12", "31", "53", "35",
               "46", "48", "06", "01", "13", "42", "29", "08", "49", "47", "56", "36", "20",
               "32", "17", "50", "30", "19", "45", "33", "04", "11", "34", "24", "23",
               "10", "44", "21", "39", "55", "41", "38", "05", "18", "27", "09") #, "02", "15") # "02" Alaska; "15" Hawaii


##  Import US states and counties from 2018 CB shapefile ----
##  Data from: https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html
##  We could use map_data() -- but want this to be generalizable to all shp.
allcounties <- readOGR(dsn = "inst/extdata/cb_2018_us_county_500k",
                       layer = "cb_2018_us_county_500k")
allstates   <- readOGR(dsn = "inst/extdata/cb_2018_us_state_500k",
                       layer = "cb_2018_us_state_500k")

allcounties <- spTransform(allcounties,
                           CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
allstates <- spTransform(allstates,
                         CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))


## A little munging and subsetting for maps ----
allcounties@data$fips <- as.numeric(as.character(allcounties@data$GEOID))
allcounties@data$stateid <- as.character(allcounties@data$STATEFP)
allstates@data$stateid <- as.character(allstates@data$STATEFP)

## Alaska and Hawaii
akcounties <- allcounties[allcounties$STATEFP == "02", ]
akcounties <- elide(akcounties, rotate=-45)
akcounties <- elide(akcounties, scale=max(apply(bbox(akcounties), 1, diff)) / 2.3)
akcounties <- elide(akcounties, shift=c(-2400000, -2500000))
proj4string(akcounties) <- proj4string(allcounties)

alaska <- allstates[allstates$STATEFP == "02", ]
alaska <- elide(alaska, rotate=-45)
alaska <- elide(alaska, scale=max(apply(bbox(alaska), 1, diff)) / 2.3)
alaska <- elide(alaska, shift=c(-2400000, -2500000))
proj4string(alaska) <- proj4string(allstates)

hicounties <- allcounties[allcounties$STATEFP == "15", ]
hicounties <- elide(hicounties, rotate=-35)
hicounties <- elide(hicounties, shift=c(5300000, -1700000)) # c(5000000, -1500000)
proj4string(hicounties) <- proj4string(allcounties)

hawaii <- allstates[allstates$STATEFP == "15", ]
hawaii <- elide(hawaii, rotate=-35)
hawaii <- elide(hawaii, shift=c(5300000, -1700000)) # c(5000000, -1500000)
proj4string(hawaii) <- proj4string(allstates)

## Only use lower 48 states
subcounties <- subset(allcounties, allcounties@data$state %in% lower_48)
substates <- subset(allstates, allstates@data$state %in% lower_48)

uscounties <- rbind(subcounties, akcounties, hicounties)
usstates <- rbind(substates, alaska, hawaii)

## Fortify into dataframes
uscounties_df <- tidy(uscounties, region = "GEOID")
uscounties_df$id <- as.numeric(uscounties_df$id)
usstates_df <- tidy(usstates, region = "GEOID")

### Suppressed deaths
map_supp <- ggplot(data = sum_dt) +
  geom_map(aes(map_id = fips, fill = `total COVID-19 deaths suppressed`),
           map = uscounties_df, color = "gray80", size = 0.05)  +
  expand_limits(x = uscounties_df$long, y = uscounties_df$lat) +
  scale_fill_viridis(discrete = TRUE, direction = -1, alpha = 0.8,
                     begin = 0.05, end = 0.95, option = "magma") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_path(data = usstates_df, aes(long, lat, group = group),
            color = "gray30", size = .2, alpha = .8) +
  coord_equal() +
  theme_map() +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 14)) +
  labs(title = "(A) COVID-19 Death Counts with Suppressed Data",
       subtitle = "Suppressed data set to 0") #,
       #caption = paste0('Source: https://github.com/syzoekao/COVIDYPLL'))

ggsave(map_supp, file = 'inst/deaths suppressed.tiff', device = "tiff", width = 10, height = 6)


### imputed deaths
map_imputed <- ggplot(data = sum_dt) +
  geom_map(aes(map_id = fips, fill = `COVID-19 Death Counts`),
           map = uscounties_df, color = "gray80", size = 0.05)  +
  expand_limits(x = uscounties_df$long, y = uscounties_df$lat) +
  scale_fill_viridis(discrete = TRUE, direction = -1, alpha = 0.8,
                     begin = 0.05, end = 0.95, option = "magma") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_path(data = usstates_df, aes(long, lat, group = group),
            color = "gray30", size = .2, alpha = .8) +
  coord_equal() +
  theme_map() +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "right",
        # legend.title = element_text(size = 14),
        legend.title = element_blank(),
        legend.text = element_text(size = 14)) +
  labs(title = "(B) COVID-19 Death Counts with Imputed Data",
       subtitle = "Suppressed data were replaced with imputed COVID-19 death counts") #,
       #caption = paste0('Source: https://github.com/syzoekao/COVIDYPLL'))

ggsave(map_imputed, file = 'inst/death map.tiff', device = "tiff", width = 10, height = 6)

ggarrange(map_supp, map_imputed, nrow = 1, common.legend = T, legend = "bottom")
# ggsave(file = 'inst/death maps combined.tiff', device = "tiff", width = 20, height = 10)
ggsave(file = 'inst/death maps combined.png', device = "png", width = 20, height = 10)


