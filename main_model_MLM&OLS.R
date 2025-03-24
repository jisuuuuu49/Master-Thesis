library(lme4)
library(tidyverse)
library(performance)
library(sjPlot)
library(insight)
library(splines)
library(clubSandwich)
library(sandwich)
library(lmtest)
library(htmltools)
library(broom)

# set your repository.
#setwd("/Users/aunal/Desktop/Uni/Study/ss 2024/master thesis/data")

#df <- read_csv("ESS_EUROSTAT_final_standardized.csv") %>%
#  janitor::clean_names() %>%
#  filter(year != 9999) %>%
#  filter(gndr != 9) %>%
#  mutate(
#    year = as.factor(year),
#    gndr = as.factor(gndr),
#    immig_background = as.factor(immig_background),
#    education_numeric = as.factor(education_numeric),
#    have_child = as.factor(have_child)
#  ) %>%
#  filter(!grepl("^(DE|UK)", country), !grepl("^(DE|UK)", nuts2))


#df <- df %>%
#  mutate(
#    economic_threat_index = rowMeans(as.matrix(.[, c("immigration_economy", "perceived_economic_competition", "jobs_taken")]), na.rm = TRUE),
#    economic_threat_index = (economic_threat_index - min(economic_threat_index, na.rm = TRUE)) /
#      (max(economic_threat_index, na.rm = TRUE) - min(economic_threat_index, na.rm = TRUE)),
#    economic_threat_index = 1 - economic_threat_index
#  )

df <- read_csv("your file.csv") %>% clean_names()
df <- df %>%
  group_by(nuts2) %>%
  mutate(
    social_contact_dm = social_contact - mean(social_contact, na.rm = TRUE),
    social_contact_m = mean(social_contact, na.rm = TRUE),
    economic_threat_index_dm = economic_threat_index - mean(economic_threat_index, na.rm = TRUE),
    economic_threat_index_m = mean(economic_threat_index, na.rm = TRUE),
    lrscale_dm = lrscale - mean(lrscale, na.rm = TRUE),
    lrscale_m = mean(lrscale, na.rm = TRUE),
    household_income_dm = household_income - mean(household_income, na.rm = TRUE),
    household_income_m = mean(household_income, na.rm = TRUE),
    minority_presence_dm = minority_presence - mean(minority_presence, na.rm = TRUE),
    minority_presence_m = mean(minority_presence, na.rm = TRUE),
    pop_density_dm = pop_density - mean(pop_density, na.rm = TRUE),
    pop_density_m = mean(pop_density, na.rm = TRUE),
    unemployment_dm = unemployment - mean(unemployment, na.rm = TRUE),
    unemployment_m = mean(unemployment, na.rm = TRUE),
    gdppc_dm = gdppc - mean(gdppc, na.rm = TRUE),
    gdppc_m = mean(gdppc, na.rm = TRUE),
    net_mig_dm = minority_presence - mean(net_mig, na.rm = TRUE),
    net_mig_m = mean(net_mig, na.rm = TRUE)
  ) %>%
  ungroup() %>%

  group_by(country) %>%
  mutate(
    unemployment_c_dm = unemployment - mean(unemployment, na.rm = TRUE),
    unemployment_c_m = mean(unemployment, na.rm = TRUE),
    gdppc_c_dm = gdppc - mean(gdppc, na.rm = TRUE),
    gdppc_c_m = mean(gdppc, na.rm = TRUE),
    net_mig_c_dm = minority_presence - mean(net_mig, na.rm = TRUE),
    net_mig_c_m = mean(net_mig, na.rm = TRUE)
  ) %>%
  ungroup()

m0 <- lmer(imwbcnt ~ 1 + (1|nuts2) + (1 | country/year), data = df)

var_ind <- insight::get_variance_residual(m0)
var_nuts2 <- insight::get_variance_intercept(m0)[1]
var_year <- insight::get_variance_intercept(m0)[2]
var_country <- insight::get_variance_intercept(m0)[3]

var_nuts2_share <- var_nuts2 / (var_ind + var_nuts2 + var_year + var_country)
var_year_share <- var_year / (var_ind + var_nuts2 + var_year + var_country)
var_country_share <- var_country / (var_ind + var_nuts2 + var_year + var_country)
var_ind_share <- var_ind / (var_ind + var_nuts2 + var_year + var_country)

cat("Variance within individuals:", round(var_ind, 4), "\n")
cat("Variance between NUTS2 regions:", round(var_nuts2, 4), "\n")
cat("Variance between years:", round(var_year, 4), "\n")
cat("Variance between countries:", round(var_country, 4), "\n")
cat("Share of variance between NUTS2 regions:", round(var_nuts2_share, 4), "\n")
cat("Share of variance between years:", round(var_year_share, 4), "\n")
cat("Share of variance between countries:", round(var_country_share, 4), "\n")
cat("Share of variance within individuals:", round(var_ind_share, 4), "\n")



# ---- Fit Macro-Level Model (Only Regional & National Factors) ----
m1_region <- lmer(imwbcnt ~ pop_density_dm + unemployment_dm + gdppc_dm + net_mig_dm +
                   pop_density_m + unemployment_m + gdppc_m + net_mig_m + year +
                   (1 | nuts2) + (1 | country/year),
                 data = df, control = lmerControl(optimizer = "bobyqa"))
m1_macro <- lmer(imwbcnt ~ pop_density_dm + unemployment_dm + gdppc_dm + net_mig_dm +
                   pop_density_m + unemployment_m + gdppc_m + net_mig_m + 
                   unemployment_c_dm + gdppc_c_dm +
                   unemployment_c_m + gdppc_c_m +
                   year +
                   (1 | nuts2) + (1 | country/year),
                 data = df, control = lmerControl(optimizer = "bobyqa"))

# ---- Fit Full Model (Macro + Micro-Level Factors) ----
m2_full <- lmer(imwbcnt ~
                  pop_density_dm + unemployment_dm + gdppc_dm + net_mig_dm +
                  pop_density_m + unemployment_m + gdppc_m + net_mig_m + 
                  unemployment_c_dm + gdppc_c_dm + 
                  unemployment_c_m + gdppc_c_m +
                  social_contact_dm + social_contact_m + minority_presence_dm + minority_presence_m +
                  economic_threat_index_dm + economic_threat_index_m + lrscale_dm +lrscale_m +
                  education_numeric + gndr + age + household_income_dm +household_income_m + year +
                  (1 | nuts2)  + (1 | country/year) , 
                data = df, control = lmerControl(optimizer = "bobyqa"))

# ---- Display Model Summaries ----
summary(m0)
summary(m1_macro)
summary(m2_full)

# ---- Check Convergence & Multicollinearity ----
# performance::check_convergence(m2_full)
# performance::icc(m2_full)
# vif(m2_full)

tab_model(
  m0, m1_region, m1_macro, m2_full,
  
  # 1) Label the columns (the DV labels) 
  dv.labels = c("M0", "M1", "M2", "M3"),
  
  # 2) Title of the table
  title = "Table 2: Multilevel Predictors of Outgroup Hostility",
  
  # 3) Collapse the standard errors in parentheses
  collapse.se = TRUE,
  
  # 4) We do not show Confidence Intervals, but do show AIC, ICC
  show.ci = FALSE,
  show.aic = TRUE,
  show.icc = TRUE,
  
  # 5) Significance thresholds
  p.threshold = c(0.05, 0.01, 0.001),
  # 6) Provide custom labels for each predictor
  pred.labels = c(
    "(Intercept)" = "Intercept",
    
    #  Micro-level variables
    "social_contact_dm"          = "Social Contact (WE)",
    "social_contact_m"           = "Social Contact (BE)",
    "minority_presence_dm"       = "Minority Presence (WE)",
    "minority_presence_m"        = "Minority Presence (BE)",
    "economic_threat_index_dm"   = "Perceived Economic Threat (WE)",
    "economic_threat_index_m"    = "Perceived Economic Threat (BE)",
    "lrscale_dm"                 = "Political Ideology (WE)",
    "lrscale_m"                  = "Political Ideology (BE)",
    "household_income_dm"        = "Household Income (WE)",
    "household_income_m"         = "Household Income (BE)",
    
    #  Categorical variables
    "education_numeric3" = "Education (ISCED3) [ref=ISCED2]",
    "education_numeric4" = "Education (ISCED4)",
    "education_numeric5" = "Education (ISCED5+)",
    "gndr2" = "Gender [ref = Male]", 
    "age" = "Age",
    "year2003" = "Year (2003) [ref = 2002]",
    "year2014" = "Year (2014)",
    "year2015" = "Year (2015)",
    
    
    #  Within (WE) and Between (BE) examples
    "pop_density_dm" = "Population Density (regional-level WE)",
    "pop_density_m"  = "Population Density (regional-level BE)",
    "unemployment_dm" = "Unemployment Rate (regional-level WE)",
    "unemployment_m"  = "Unemployment Rate (regional-level BE)",
    "gdppc_dm"        = "GDP per Capita (regional-level WE)",
    "gdppc_m"         = "GDP per Capita (regional-level BE)",
    "net_mig_dm"      = "Crude Net Migration Rate (regional-level WE)",
    "net_mig_m"       = "Crude Net Migration Rate (regional-level BE)",
    
    # If you have country-level decomposition (pop_density_c_dm, etc.), you can also rename:
    "unemployment_c_dm"= "Unemployment Rate (Country-level WE)",
    "unemployment_c_m" = "Unemployment Rate (Country-level BE)",
    "gdppc_c_dm"       = "GDP per Capita (Country-level WE)",
    "gdppc_c_m"        = "GDP per Capita (Country-level BE)"
    
  ),
  
  # 7) Additional labeling arguments
  string.pred = "Predictors",        # Column label for the predictor names
  string.est  = "Est. (SE)",         # Column label for the estimates
  string.p    = "p-value",
  
  # 8) Show reference levels automatically above the factor if you want
  show.reflvl = TRUE,
  file = "multilevel_predictors.html"
)




var_ind <- insight::get_variance_residual(m0)  # Individual-level variance
var_nuts2 <- insight::get_variance_intercept(m0)[1]  # Between-region variance
var_year <- insight::get_variance_intercept(m0)[2]  # Between-year variance
var_country <- insight::get_variance_intercept(m0)[3]  # Between-country variance

total_var <- var_ind + var_nuts2 + var_year + var_country

cat("\nVariance Decomposition Results:\n")
cat("Within-individual variance:", round(var_ind, 4), "\n")
cat("Between NUTS2 variance:", round(var_nuts2, 4), "\n")
cat("Between year variance:", round(var_year, 4), "\n")
cat("Between country variance:", round(var_country, 4), "\n")
cat("ICC (NUTS2):", round(var_nuts2 / total_var, 4), "\n")
cat("ICC (year):", round(var_year / total_var, 4), "\n")
cat("ICC (country):", round(var_country / total_var, 4), "\n")



# ---- Fixed Effects OLS Model ----
m0 <- lm(imwbcnt ~ factor(year)  + factor(country) + factor(nuts2),
         data = df, na.action = na.exclude)
summary(m0)


m1_region <- lm(imwbcnt ~ pop_density_dm + unemployment_dm + gdppc_dm + net_mig_dm +
                  factor(year) + factor(nuts2),
                data = df, na.action = na.exclude)
summary(m1_region)


m1_macro <- lm(imwbcnt ~ pop_density_dm + unemployment_dm + gdppc_dm + net_mig_dm +
                 unemployment_c_dm + gdppc_c_dm +
                 factor(year) + factor(country) + factor(nuts2), 
               data = df, na.action = na.exclude)
summary(m1_macro)


m2_full <- lm(imwbcnt ~ pop_density_dm + unemployment_dm + gdppc_dm + net_mig_dm +
                unemployment_c_dm + gdppc_c_dm + 
                social_contact_dm + social_contact_m +
                minority_presence_dm + minority_presence_m +
                economic_threat_index_dm + economic_threat_index_m +
                lrscale_dm + lrscale_m +
                education_numeric + household_income_dm + age + gndr +
                factor(year) + factor(nuts2) +factor(country),
              data = df, na.action = na.exclude)
summary(m2_full)


rename_coef_names <- function(model, renames) {
  coefs <- tidy(model)
  coefs <- coefs %>%
    mutate(term = ifelse(term %in% names(renames), renames[term], term))
  return(coefs)
}

year_names <- c("factor(year)2003" = "year(2003)",
                "factor(year)2014" = "Year(2014)",
                "factor(year)2015" = "Year(2015)")

m0_coefs <- rename_coef_names(m0_fe_clean, year_names)
m1_region_coefs <- rename_coef_names(m1_region_clean, year_names)
m1_macro_coefs <- rename_coef_names(m1_macro_clean, year_names)
m2_full_coefs <- rename_coef_names(m2_full_clean, year_names)

names(coef(m0_fe_clean))

bic_values <- c(
  BIC(m0),BIC(m1_region),BIC(m1_macro),BIC(m2_full))
print(bic_values)
tab_model(
  m0, m1_region, m1_macro, m2_full, #m0_fe_clean, m1_region_clean, m1_macro_clean, m2_full_clean,
  dv.labels = c("M0", "M1",
                "M2", "M3"),
  title = "Table 3: Linear Regression with Fixed Effects Models for Outgroup Hostility",
  collapse.se = TRUE,
  show.ci = FALSE,
  show.aic = TRUE,
  p.threshold = c(0.05, 0.01, 0.001),
  rm.terms = c("factor\\(\\s*nuts2\\s*\\).*"),
  
  pred.labels = c(
    "(Intercept)" = "Intercept",
    "social_contact_dm"          = "Social Contact (WE)",
    "social_contact_m"           = "Social Contact (BE)",
    "minority_presence_dm"       = "Minority Presence (WE)",
    "minority_presence_m"        = "Minority Presence (BE)",
    "economic_threat_index_dm"   = "Perceived Economic Threat (WE)",
    "economic_threat_index_m"    = "Perceived Economic Threat (BE)",
    "lrscale_dm"                 = "Political Ideology (WE)",
    "lrscale_m"                  = "Political Ideology (BE)",
    "household_income_dm"        = "Household Income (WE)",
    "household_income_m"         = "Household Income (BE)",
    
    # setNames(c("Year (2003)", "Year (2014)", "Year (2015)"), 
    #          c("year2003", "year2014", "year2015")),
    #  Categorical variables
    "education_numeric3" = "Education (ISCED3) [ref=ISCED2]",
    "education_numeric4" = "Education (ISCED4)",
    "education_numeric5" = "Education (ISCED5+)",
    "gndr2" = "Gender [ref = Male]", 
    "factor(year)2003" = "Year (2003) [ref = 2002]",
    "factor(year)2014" = "Year (2014)",
    "factor(year)2015" = "Year (2015)",
    "age"= "Age",
    
    "pop_density_dm" = "Population Density (regional-level WE)",
    "pop_density_m"  = "Population Density (regional-level BE)",
    "unemployment_dm" = "Unemployment Rate (regional-level WE)",
    "unemployment_m"  = "Unemployment Rate (regional-level BE)",
    "gdppc_dm"        = "GDP per Capita (regional-level WE)",
    "gdppc_m"         = "GDP per Capita (regional-level BE)",
    "net_mig_dm"      = "Crude Net Migration Rate (regional-level WE)",
    "net_mig_m"       = "Crude Net Migration Rate (regional-level BE)",
    
    "unemployment_c_dm"= "Unemployment Rate (Country-level WE)",
    "unemployment_c_m" = "Unemployment Rate (Country-level BE)",
    "gdppc_c_dm"       = "GDP per Capita (Country-level WE)",
    "gdppc_c_m"        = "GDP per Capita (Country-level BE)"
    
  ),
  
  string.pred = "Predictors",       
  string.est  = "Est. (SE)",        
  string.p    = "p-value",
  
  #rm.terms = c("factor\\(nuts2\\).*", "factor\\(country\\).*"),#,"^nuts2","^country"),#,".*nuts2.*",".*NUTS2.*",".*2*"),
  show.reflvl = FALSE
  #file = "ols_fixed_effects_models.html"
)

