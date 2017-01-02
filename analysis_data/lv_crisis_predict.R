# ---------------------------------------------------------------------------- #
# Predict LV crisis with FinStress
# ---------------------------------------------------------------------------- #

library(rio)
library(repmis)
library(dplyr)
library(survival)
library(simPH)
library(ggplot2)

# Set working directory
possibles <- c('/git_repositories/predicting_finstress/analysis_data')
set_valid_wd(possibles)

# Import data
comb <- import('combined_data.csv')

comb_high <- comb %>% filter(income == 'High income: OECD')

# Keep only crisis start years
comb_start <- comb %>% filter(!(lv_bank_crisis == 1 & lv_lead1yr == 1))

comb_start_high <- comb_high %>% 
    filter(!(lv_bank_crisis == 1 & lv_lead1yr == 1))


# Logistic approach ------------------------------------------------------------
l1 <- glm(lv_lead1yr ~ finstress_mean + as.factor(iso2c), 
                         data = comb_start, family = 'binomial')

l2 <- glm(lv_lead1yr ~ finstress_var + as.factor(iso2c), 
            data = comb_start, family = 'binomial')

l3 <- glm(lv_lead1yr ~ finstress_mean + as.factor(iso2c), 
            data = comb_start_high, family = 'binomial')

l4 <- glm(lv_lead1yr ~ gdp_growth + as.factor(iso2c), 
            data = comb_start, family = 'binomial')


ggplot(comb_start_high,aes(x = finstress_mean, colour = as.factor(lv_lead1yr), 
                           group = as.factor(lv_lead1yr))) + 
    geom_density() + 
    theme_bw()



# Cox PH Approach --------------------------------------------------------------
cox1 <- coxph(Surv(year, lv_lead1yr) ~ finstress_mean + cluster(iso2c), 
              data = comb_start_high)

sim1 <- coxsimLinear(cox1, 'finstress_mean', qi = 'Hazard Rate',
                     Xj = c(0.2, 0.6))

hr_finstress <- simGG(sim1, leg.name = 'FinStress') + 
                    xlab('') + ylab('Hazard Rate of Banking\nCrisis at t + 1\n')

ggsave(hr_finstress, filename = 'results_figures/hr_lv_crisis.pdf')


subbed <- comb_start_high %>% filter(finstress_mean >= 0.56 & lv_lead1yr == 0)

# Other ------------------------------------------------------------------------
ggplot(comb, aes(finstress_mean, stock_price_volatility)) +
    geom_point() +
    stat_smooth(se = F, method = 'lm') +
    theme_bw()

ggplot(comb, aes(finstress_mean, stock_returns)) +
    geom_point() +
    stat_smooth(se = F, method = 'lm') +
    theme_bw()
