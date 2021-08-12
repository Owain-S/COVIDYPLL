rm(list = ls())

library(COVIDYPLL)
library(data.table)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(openxlsx)
library(parallel)
library(rgdal)
library(rgeos)
library(viridis)
library(grid)
library(maptools)
library(broom)
library(mapproj)
library(ggthemes)


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
                          avg_le2020 = mean(avg_le2020),
                          std_pop_wgt = mean(std_pop_wgt)),
                   by = c("fips", "age_group")]

  # agg_dt <- calculate_ypll(tmp_dt)
  #
  # agg_dt <- agg_dt[, list(tot_covid_d = sum(covid_19_deaths),
  #                         covid19_death_rate_age_adjusted = sum(covid19_death_rate_age_adjusted),
  #                         ypll_rate_age_adjusted = sum(ypll_rate_age_adjusted)),
  #                  by = .(fips)]

  agg_dt <- calculate_ypll2(tmp_dt, byvar = "fips")

  agg_dt
}, mc.cores = 6)

sum_dt <- rbindlist(sum_dt)
sum_dt <- sum_dt[, list(tot_covid_d = mean(tot_covid_d),
                        covid19_death_rate_age_adjusted = mean(covid19_death_rate_age_adjusted),
                        ypll_rate_age_adjusted = mean(ypll_rate_age_adjusted)),
                 by = .(fips)]

sum_dt <- merge(county_state, sum_dt, by = c("fips"))

tmp_brk <- round(quantile(sum_dt$ypll_rate_age_adjusted, prob = c(0.2, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95)))
sum_dt[, `Age-adjusted YPLL Rate\nPer 100,000 People` := cut(ypll_rate_age_adjusted, breaks = c(-Inf, tmp_brk, Inf),
                                          labels = paste0(paste0(c(20, 40, 50, 60, 70, 80, 90, 95, ">95"),
                                                                 "-th percentile"), ": ",
                                                          c("0-320", "320-474", "474-547", "547-628", "628-730", "730-883", "883-1172", "1172-1503", ">=1503")))]


tmp_brk <- round(quantile(sum_dt$tot_covid_d, prob = c(0.2, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95)))
sum_dt[, `COVID-19 Deaths` := cut(tot_covid_d, breaks = c(-Inf, tmp_brk, Inf),
                                  labels = paste0(paste0(c(20, 40, 50, 60, 70, 80, 90, 95, ">95"),
                                                         "-th percentile"), ": ",
                                                  c("0-15", "15-30", "30-40", "40-54", "54-71", "71-108", "108-220", "220-479", ">=479")))]

tmp_brk <- round(quantile(sum_dt$covid19_death_rate_age_adjusted,
                          prob = c(0.2, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95)))
sum_dt[, `Age-adjusted COVID-19 Death Rate\nPer 100,000 People` := cut(covid19_death_rate_age_adjusted,
                                  breaks = c(-Inf, tmp_brk, Inf),
                                  labels = paste0(paste0(c(20, 40, 50, 60, 70, 80, 90, 95, ">95"),
                                                         "-th percentile"), ": ",
                                                  c("0-23", "23-33", "33-37", "37-42", "42-48", "48-56", "56-74", "74-91", ">=91")))]


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

### YPLL
map_ypll <- ggplot(data = sum_dt) +
  geom_map(aes(map_id = fips, fill = `Age-adjusted YPLL Rate\nPer 100,000 People`),
           map = uscounties_df, color = "gray80", size = 0.05)  +
  expand_limits(x = uscounties_df$long, y = uscounties_df$lat) +
  scale_fill_viridis(discrete = TRUE, direction = -1, alpha = 0.8, option = "magma") +
  # scale_fill_brewer(palette = "RdBu", direction = -1) + #, alpha = 0.8) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_path(data = usstates_df, aes(long, lat, group = group),
            color = "gray30", size = .4, alpha = .8) +
  coord_equal() +
  theme_map() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "right") +
  labs(title = "Age-adjusted Years of Potential Life Lost Rate due to COVID-19 in 2020",
       caption = paste0('Source: https://github.com/syzoekao/COVIDYPLL'))

ggsave(map_ypll, file = 'inst/ypll map.tiff', device = "tiff", width = 10, height = 6)



map_death <- ggplot(data = sum_dt) +
  geom_map(aes(map_id = fips, fill = `COVID-19 Deaths`),
           map = uscounties_df, color = "gray80", size = 0.05)  +
  expand_limits(x = uscounties_df$long, y = uscounties_df$lat) +
  scale_fill_viridis(discrete = TRUE, direction = -1, alpha = 0.8, option = "magma") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_path(data = usstates_df, aes(long, lat, group = group),
            color = "gray30", size = .4, alpha = .8) +
  coord_equal() +
  theme_map() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "right") +
  labs(title = "Total COVID-19 Deaths in 2020",
       caption = paste0('Source: https://github.com/syzoekao/COVIDYPLL'))

ggsave(map_death, file = 'inst/death map.tiff', device = "tiff", width = 10, height = 6)


map_death_rate <- ggplot(data = sum_dt) +
  geom_map(aes(map_id = fips, fill = `Age-adjusted COVID-19 Death Rate\nPer 100,000 People`),
           map = uscounties_df, color = "gray80", size = 0.05)  +
  expand_limits(x = uscounties_df$long, y = uscounties_df$lat) +
  scale_fill_viridis(discrete = TRUE, direction = -1, alpha = 0.8, option = "magma") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_path(data = usstates_df, aes(long, lat, group = group),
            color = "gray30", size = .4, alpha = .8) +
  coord_equal() +
  theme_map() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "right") +
  labs(title = "Age-adjusted COVID-19 Death Rate in 2020",
       caption = paste0('Source: https://github.com/syzoekao/COVIDYPLL'))

ggsave(map_death_rate, file = 'inst/death rate map.tiff', device = "tiff", width = 10, height = 6)



map_rucc <- ggplot(data = sum_dt) +
  geom_map(aes(map_id = fips, fill = urban_rural_code),
           map = uscounties_df, color = "gray80", size = 0.05)  +
  expand_limits(x = uscounties_df$long, y = uscounties_df$lat) +
  scale_fill_viridis(discrete = TRUE, direction = -1, alpha = 0.8, option = "magma") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_path(data = usstates_df, aes(long, lat, group = group),
            color = "gray30", size = .4, alpha = .8) +
  coord_equal() +
  theme_map() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "right") +
  labs(title = "Urbanicity",
       caption = paste0('Source: https://github.com/syzoekao/COVIDYPLL'))

ggsave(map_rucc, file = 'inst/rucc map.tiff', device = "tiff", width = 10, height = 6)


