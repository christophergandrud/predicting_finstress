# ---------------------------------------------------------------------------- #
# Merge FinStress, WDI, OECD, Run basic regressions on FinStress Variance
# ---------------------------------------------------------------------------- #

# Load required packages
library(repmis)
library(rio)
library(dplyr)
library(lubridate)
library(DataCombine)
library(countrycode)
library(WDI)
library(plm)
library(stargazer)

# Set working directory
possibles <- c('/git_repositories/predicting_finstress/analysis_data')
set_valid_wd(possibles)

# Load FinStress -------------------------------------------------
FinStress <- rio::import("http://bit.ly/1LFEnhM")

# Annual data --------
FinStress$year <- year(FinStress$date)

finstress <- FinStress %>% select(iso2c, date, year, C1_ma) %>%
    rename(finstress = C1_ma)

# Annual mean
finstress_yr_mean <- finstress %>% group_by(iso2c, year) %>%
    summarise(finstress_mean = mean(finstress, na.rm = T))

# Annual variance
finstress_yr <- finstress %>% group_by(iso2c, year) %>%
    summarise(finstress_var = var(finstress, na.rm = T))

finstress_yr <- merge(finstress_yr_mean, finstress_yr, by = c('iso2c', 'year'),
                      all = T)

# rescale to make coefficients more easily interpretable
finstress_yr$finstress_var <- finstress_yr$finstress_var * 1000

finstress_yr <- finstress_yr %>% arrange(iso2c, year)

FindDups(finstress_yr, Vars = c('iso2c', 'year'))

finstress_yr <- slide(finstress_yr, Var = 'finstress_var', GroupVar = 'iso2c',
                      NewVar = 'finstress_var_lead1yr', slideBy = 1)

# Download WDI gdp change ------------------------------------------------------
gdp <- WDI(indicator = 'NY.GDP.MKTP.KD.ZG', start = 2003, end = 2011,
           extra = T) %>%
    rename(gdp_growth = NY.GDP.MKTP.KD.ZG)

# Financial Fragility Indicators from Andrianova et al. (2015)
ff <- import('raw_data/Financial Fragility Database Stata.dta') %>%
    select(-countryname, -countryid) %>%
    dplyr::rename(iso2c = countrycode)

ff$log_imploans <- log(ff$ImpLoans)

# Merge ------------------------------------------------------------------------
comb <- merge(gdp, finstress_yr, by = c('iso2c', 'year'))
comb <- merge(comb, ff, by = c('iso2c', 'year'))

comb_high <- comb %>% filter(income == 'High income: OECD')

# Simple regression model ------------------------------------------------------
# Full sample
comb_pd <- pdata.frame(comb, index = c('iso2c', 'year'))
mfull_1 <- plm(finstress_var_lead1yr ~ gdp_growth, data = comb_pd)
mfull_2 <- plm(finstress_var_lead1yr ~ gdp_growth + finstress_var,
               data = comb_pd)
mfull_3 <- plm(finstress_var_lead1yr ~ gdp_growth + finstress_var +
                   finstress_mean, data = comb_pd)

mfull_4 <- plm(finstress_var_lead1yr ~ finstress_var + log_imploans, 
               data = comb_pd)

# High Income
comb_high_pd <- pdata.frame(comb_high, index = c('iso2c', 'year'))
moecd_1 <- plm(finstress_var_lead1yr ~ gdp_growth, data = comb_high_pd)
moecd_2 <- plm(finstress_var_lead1yr ~ gdp_growth + finstress_var, data = comb_high_pd)
moecd_3 <- plm(finstress_var_lead1yr ~ gdp_growth + finstress_var +
                   finstress_mean, data = comb_high_pd)

moecd_4 <- plm(finstress_var_lead1yr ~ finstress_var + log_imploans, 
               data = comb_high_pd)
moecd_5 <- plm(finstress_var_lead1yr ~ finstress_var + Liquid, 
               data = comb_high_pd)


stargazer(mfull_1, mfull_2, mfull_3, moecd_1, moecd_2, moecd_3, type = 'text',
          dep.var.labels = 'FinStress Variance (year+1)',
          covariate.labels = c('GDP Growth (%)', 'FinStress Variance (year+0)',
                               'FinStress Mean (year+0)') ,
          column.labels = c(rep('Full Sample', 3), rep('OECD', 3)),
          model.numbers = F)

#------------------------------------------------------------------------------#
# Quarterly data ----------------------
finstress <- FinStress %>% select(iso2c, date, C1_ma) %>%
    rename(finstress = C1_ma) %>% rename(quarter = date)

finstress$quarter <- quarter(finstress$quarter, with_year = T)

# Quarterly mean
finstress_qt_mean <- finstress %>% group_by(iso2c, quarter) %>%
    summarise(finstress_mean = mean(finstress, na.rm = T))

# Quarterly variance
finstress_qt <- finstress %>% group_by(iso2c, quarter) %>%
    summarise(finstress_var = var(finstress, na.rm = T))

finstress_qt <- merge(finstress_qt_mean, finstress_qt,
                      by = c('iso2c', 'quarter'), all = T)

# rescale to make coefficients more easily interpretable
finstress_qt$finstress_var <- finstress_qt$finstress_var * 1000

finstress_qt <- finstress_qt %>% arrange(iso2c, quarter)

FindDups(finstress_qt, Vars = c('iso2c', 'quarter'))

finstress_qt <- slide(finstress_qt, Var = 'finstress_var', GroupVar = 'iso2c',
                      NewVar = 'finstress_var_lead1qt', slideBy = 1)

# Load quarterly gdp growth (seasonally adjusted): Originally downloaded from:
# https://stats.oecd.org
oecd <- import('raw_data/QNA_06112015123914289.csv')
oecd <- oecd %>%
    filter(Measure == "Growth rate compared to the same quarter of previous year, seasonally adjusted" &
               Subject == 'Gross domestic product - expenditure approach')
oecd <- oecd[, c(2, 9, 17)]
names(oecd) <- c('country', 'quarter', 'gdp_growth_oecd')

oecd$quarter <- gsub('-Q', '\\.', oecd$quarter)
oecd$iso2c <- countrycode(oecd$country, origin = 'country.name',
                          destination = 'iso2c')
oecd <- oecd %>% select(-country)

FindDups(oecd, c('iso2c', 'quarter'))

# Merge together
comb_qt <- merge(oecd, finstress_qt, by = c('iso2c', 'quarter'))
comb_qt <- DropNA(comb_qt, 'iso2c') # NA is Euro area

# Quarterly regressions
comb_qt_pd <- pdata.frame(comb_qt, index = c('iso2c', 'quarter'))

mqt_1 <- plm(finstress_var_lead1qt ~ gdp_growth_oecd + finstress_var,
             data = comb_qt_pd)

mqt_2 <- plm(finstress_var_lead1qt ~ gdp_growth_oecd + finstress_var +
                 finstress_mean,
             data = comb_qt_pd)

stargazer(mqt_1, mqt_2, type = 'text',
          dep.var.labels = 'FinStress Variance (quarter+1)',
          covariate.labels = c('GDP Growth (%)',
                                'FinStress Variance (quarter+0)',
                                'FinStress Mean (quarter+0)')
          )
