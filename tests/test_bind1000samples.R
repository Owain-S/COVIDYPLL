context("bind1000samples")

library(COVIDYPLL)
library(data.table)
library(ggplot2)
library(viridis)
library(ggpubr)
library(dplyr)
library(openxlsx)

# test the class
test_that("output from bind1000samples should be a data.table", {
  death_samp <- bind1000samples()
  expect_equal(is.data.table(death_samp), TRUE)
})

year_rle <- NA

system.time(county_ypll <- calculate_ypll(dt = death_samp, byvar = "fips", year_rle = year_rle,
                              age_adjusted_output = TRUE, export_data_by_simno = TRUE))
system.time(county_ypll_quarter <- calculate_ypll(dt = death_samp, byvar = c("fips", "quarter"),
                                                  year_rle = year_rle,
                                      age_adjusted_output = TRUE, export_data_by_simno = TRUE))
system.time(county_ypll_age <- calculate_ypll(dt = death_samp, byvar = c("fips", "quarter"),
                                              year_rle = year_rle, age_adjusted_output = FALSE,
                                              export_data_by_simno = TRUE))


### For plot

usa_ypll_ls <- calculate_usa_ypll(dt = death_samp, year_rle = year_rle)

var_vec <- c("state", "urban_rural_code", "svi_cate")
axis_text_size <- c(8, 10, 12)
axis_text_angle <- c(0, 0, 0)
usa_text_angle <- c(90, 0, 0)
width_vec <- c(14, 10, 10)
height_vec <- c(14, 12, 12)

for (i in c(1:length(var_vec))) {

  byvar <- var_vec[i]
  g_aa_ypll_rate <- plot_age_adjusted_ypll(dt = death_samp, byvar = byvar,
                                           usa_ypll_ls = usa_ypll_ls, panel_letter = "(A)",
                                           usa_text_angle = usa_text_angle[i],
                                           axis.text.x.size = axis_text_size[i],
                                           axis.text.x.angle = 0)
  g_prop_by_age <- plot_prop_ypll_by_age(dt = death_samp, byvar = byvar, year_rle = NA,
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
  writeData(wb, "Age-Adjusted YPLL Rate", g_aa_ypll_rate$plot_data, startRow = 3, startCol = 1)
  addWorksheet(wb, "Age Proportion")
  writeData(wb, "Age Proportion", g_prop_by_age$plot_data, startRow = 3, startCol = 1)
  addWorksheet(wb, "Ratio YPLL to Death")
  writeData(wb, "Ratio YPLL to Death", g_ratio$plot_data, startRow = 3, startCol = 1)
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
