rm(list = ls())

library(COVIDYPLL)
library(data.table)
library(ggplot2)
library(viridis)
library(ggpubr)
library(dplyr)
library(openxlsx)

### Plots: by state, urban_rural_code, SVI category, and urban_rural_code plus SVI categorie
death_samp <- bind1000samples()
year_rle <- NA

usa_ypll_ls <- calculate_usa_ypll(dt = death_samp, year_rle = year_rle)

death_samp[, urban_rural_svi := ifelse(!is.na(svi_cate),
                                       paste0(urban_rural_code, ";\n", svi_cate, " SVI"), NA)]
death_samp[, svi_cate := ifelse(!is.na(svi_cate), paste0(svi_cate, " SVI"), NA)]

var_vec <- c("state", "urban_rural_code", "svi_cate", "urban_rural_svi")
axis_text_size <- c(8, 10, 12, 10)
axis_text_angle <- c(0, 0, 0, 90)
usa_text_angle <- c(90, 0, 0, 0)
width_vec <- c(14, 12, 12, 14)
height_vec <- c(14, 12, 12, 14)

for (i in c(1:length(var_vec))) {

  byvar <- var_vec[i]
  g_aa_ypll_rate <- plot_age_adjusted_ypll(dt = death_samp, byvar = byvar,
                                           usa_ypll_ls = usa_ypll_ls, panel_letter = "(A)",
                                           usa_text_angle = usa_text_angle[i],
                                           axis.text.x.size = axis_text_size[i],
                                           axis.text.x.angle = axis_text_angle[i])
  g_prop_by_age <- plot_prop_ypll_by_age(dt = death_samp, byvar = byvar,
                                         n_age_breaks = 4, year_rle = NA,
                                         usa_ypll_ls = usa_ypll_ls,
                                         panel_letter = "(B)",
                                         axis.text.y.size = axis_text_size[i])
  g_ratio <- plot_ratio_ypll_to_covid(dt = death_samp, byvar = byvar, year_rle = NA,
                                      usa_ypll_ls = NULL,
                                      panel_letter = "(C)",
                                      axis.text.y.size = axis_text_size[i])


  ggarrange(g_aa_ypll_rate$g_out,
            ggarrange(g_prop_by_age$g_out, g_ratio$g_out, widths = c(1, 0.75)),
            nrow = 2, heights = c(0.5, 1))
  ggsave(file = paste0('inst/age_group_and_ratio (', byvar, ').png'),
         device = "png", width = width_vec[i], height = height_vec[i])

  wb <- createWorkbook()
  addWorksheet(wb, "Age-Adjusted YPLL Rate")
  plot_data <- g_aa_ypll_rate$plot_data
  keep_cols <- grep("_mean", colnames(plot_data), value = T)
  keep_cols <- c(byvar, keep_cols)
  plot_data <- plot_data[, ..keep_cols]
  old_names <- c("covid_19_deaths_mean", "covid19_death_rate_mean", "covid19_death_rate_aa_mean",
                 "tot_ypll_mean", "ypll_rate_mean", "ypll_rate_aa_mean")
  setcolorder(plot_data, c(byvar, old_names))
  setnames(plot_data, old_names, c("COVID-19 Deaths", "COVID-19 Death Rate", "Age-Adjusted COVID-19 Death Rate",
                                   "Total YPLL", "YPLL Rate", "Age-Adjusted YPLL Rate"))
  writeData(wb, "Age-Adjusted YPLL Rate", plot_data, startRow = 3, startCol = 1)

  addWorksheet(wb, "Age Proportion")
  plot_data <- g_prop_by_age$plot_data
  keep_cols <- c(byvar, "age_group_plot", "ypll", "total_ypll", "prop_ypll_plot")
  plot_data <- plot_data[, ..keep_cols]
  setnames(plot_data, c("age_group_plot", "ypll", "total_ypll", "prop_ypll_plot"),
           c("Age Group", "Total YPLL", "Total YPLL by Category", "Proportion of Total YPLL Attributable to Age Group"))
  writeData(wb, "Age Proportion", plot_data, startRow = 3, startCol = 1)

  addWorksheet(wb, "Ratio YPLL to Death")
  plot_data <- g_ratio$plot_data
  keep_cols <- c(byvar, "covid_19_deaths", "prop_covid", "tot_ypll", "prop_ypll", "ratio")
  plot_data <- plot_data[, ..keep_cols]
  setnames(plot_data, c("covid_19_deaths", "prop_covid", "tot_ypll", "prop_ypll", "ratio"),
           c("COVID-19 Deaths", "% COVID-19 Deaths Attributable to Category",
             "Total YPLL", "% Total YPLL Attributable to Category", "Ratio of YPLL to COVID-19 Deaths"))
  writeData(wb, "Ratio YPLL to Death", plot_data, startRow = 3, startCol = 1)
  saveWorkbook(wb, file = paste0("inst/plot_data (", byvar, ").xlsx"), overwrite = T)
}



### Creating national table
usa_ypll_by_age <- calculate_ypll(death_samp, age_adjusted_output = F)
usa_ypll <- calculate_ypll(death_samp, age_adjusted_output = T, year_rle = year_rle)
usa_ypll[, `:=` (age_group = "Total")]

usa_ypll_by_age <- rbindlist(list(usa_ypll_by_age, usa_ypll), use.names = T, fill = T)

usa_ypll_by_age[, `:=` (covid_death = format(round(covid_19_deaths_mean), big.mark = ","),
                        covid_death_ci = paste0("(", format(round(covid_19_deaths_lb), big.mark = ","),
                                                "\U2012", format(round(covid_19_deaths_ub), big.mark = ","), ")"),
                        covid_death_rate = format(round(covid19_death_rate_mean, 2), big.mark = ","),
                        covid_death_rate_ci = paste0("(", format(round(covid19_death_rate_lb, 2), big.mark = ","),
                                                     "\U2012", format(round(covid19_death_rate_ub, 2), big.mark = ","), ")"),
                        covid_death_rate_aa = format(round(covid19_death_rate_aa_mean, 2), big.mark = ","),
                        covid_death_rate_aa_ci = paste0("(", format(round(covid19_death_rate_aa_lb, 2), big.mark = ","),
                                                        "\U2012", format(round(covid19_death_rate_aa_ub, 2), big.mark = ","), ")"),
                        tot_ypll = format(round(tot_ypll_mean), big.mark = ","),
                        tot_ypll_ci = paste0("(", format(round(tot_ypll_lb), big.mark = ","),
                                             "\U2012", format(round(tot_ypll_ub), big.mark = ","), ")"),
                        ypll_rate = format(round(ypll_rate_mean), big.mark = ","),
                        ypll_rate_ci = paste0("(", format(round(ypll_rate_lb), big.mark = ","),
                                              "\U2012", format(round(ypll_rate_ub), big.mark = ","), ")"),
                        ypll_rate_aa = format(round(ypll_rate_aa_mean, 2), big.mark = ","),
                        ypll_rate_aa_ci = paste0("(", format(round(ypll_rate_aa_lb, 2), big.mark = ","),
                                                 "\U2012", format(round(ypll_rate_aa_ub, 2), big.mark = ","), ")")
)]

death_var <- c("covid_death", "covid_death_rate", "covid_death_rate_aa",
               "tot_ypll", "ypll_rate", "ypll_rate_aa")
ci_vars <- paste0(death_var, "_ci")
out_var <- c("age_group", death_var, ci_vars)

out_table <- usa_ypll_by_age[, ..out_var]

out_table[, (ci_vars) := lapply(.SD, function(x) gsub("[[:space:]]", "", x)), .SDcols = ci_vars]


out_table <- melt(out_table, measure = lapply(death_var, function(x) c(x, paste0(x, "_ci"))),
                  value.name = death_var)

out_table[, variable := ifelse(variable == 1, "mean", "CI")]
out_table[, variable := factor(variable, levels = c("mean", "CI"))]
out_table <- out_table[order(age_group, variable)]

wb <- createWorkbook()
addWorksheet(wb, "Sheet1")
writeData(wb, "Sheet1", out_table, startRow = 3, startCol = 2)
saveWorkbook(wb, file = paste0("inst/death related outcome by age.xlsx"), overwrite = T)


### Plot maps

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


death_samp <- bind1000samples()
year_rle <- NA

system.time(county_ypll <- calculate_ypll(dt = death_samp, byvar = "fips",
                                          year_rle = year_rle,
                                          age_adjusted_output = TRUE,
                                          export_data_by_simno = F))

tmp_brk <- round(quantile(county_ypll$ypll_rate_aa_mean,
                          prob = c(0.2, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95), na.rm = T))
county_ypll[, `Age-adjusted YPLL Rate\nPer 100,000 People` :=
              cut(ypll_rate_aa_mean, breaks = c(-Inf, tmp_brk, Inf),
                  labels = paste0(paste0(c(20, 40, 50, 60, 70, 80, 90, 95, ">95"),
                                         "-th percentile"), ": ",
                                  c("0-1304", "1304-1942", "1942-2250", "2250-2582", "2582-3023",
                                    "3023-3660", "3660-4896", "4896-6335", ">=6335")))]

tmp_brk <- round(quantile(county_ypll$covid19_death_rate_aa_mean,
                          prob = c(0.2, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95)))
county_ypll[, `Age-adjusted COVID-19 Death Rate\nPer 100,000 People` :=
         cut(covid19_death_rate_aa_mean,
             breaks = c(-Inf, tmp_brk, Inf),
             labels = paste0(paste0(c(20, 40, 50, 60, 70, 80, 90, 95, ">95"),
                                    "-th percentile"), ": ",
                             c("0-89", "89-131", "131-147", "147-166", "166-192",
                               "192-225", "225-293", "293-366", ">=366")))]



# system.time(county_ypll_quarter <- calculate_ypll(dt = death_samp, byvar = c("fips", "quarter"),
#                                                   year_rle = year_rle,
#                                                   age_adjusted_output = TRUE, export_data_by_simno = TRUE))
# system.time(county_ypll_age <- calculate_ypll(dt = death_samp, byvar = c("fips", "quarter"),
#                                               year_rle = year_rle, age_adjusted_output = FALSE,
#                                               export_data_by_simno = TRUE))


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
map_ypll <- ggplot(data = county_ypll) +
  geom_map(aes(map_id = fips, fill = `Age-adjusted YPLL Rate\nPer 100,000 People`),
           map = uscounties_df, color = "gray80", size = 0.01)  +
  expand_limits(x = uscounties_df$long, y = uscounties_df$lat) +
  scale_fill_viridis(discrete = TRUE, direction = -1, alpha = 0.8, option = "magma") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_path(data = usstates_df, aes(long, lat, group = group),
            color = "gray30", size = .1, alpha = .8) +
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

ggsave(map_ypll, file = 'inst/ypll map.png', device = "png", width = 10, height = 6)



map_death_rate <- ggplot(data = county_ypll) +
  geom_map(aes(map_id = fips, fill = `Age-adjusted COVID-19 Death Rate\nPer 100,000 People`),
           map = uscounties_df, color = "gray80", size = 0.01)  +
  expand_limits(x = uscounties_df$long, y = uscounties_df$lat) +
  scale_fill_viridis(discrete = TRUE, direction = -1, alpha = 0.8, option = "magma") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_path(data = usstates_df, aes(long, lat, group = group),
            color = "gray30", size = .1, alpha = .8) +
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

ggsave(map_death_rate, file = 'inst/death rate map.png', device = "png", width = 10, height = 6)



