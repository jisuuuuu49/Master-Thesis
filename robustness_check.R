# ---------------------------
# Load Required Packages
# ---------------------------
library(lme4)
library(tidyverse)
library(performance)
library(sjPlot)
library(insight)
library(splines)
library(clubSandwich)
library(sandwich)
library(lmtest)
library(insight)
library(htmltools)
library(mfp)
library(ordinal)

# ---------------------------
# Load & Prepare Data
# ---------------------------
# set your working directory and read your file.
df <- read_csv("your file.csv") %>% clean_names()
#setwd("/Users/aunal/Desktop/Uni/Study/ss 2024/master thesis/data")
#df <- read_csv("ESS_EUROSTAT_final_standardized.csv") %>%
#  janitor::clean_names() %>%
#  filter(year != 9999) %>%
#  filter(gndr != 9) %>%
#  mutate(
#    year = as.factor(year),
#    gndr = as.factor(gndr)
#  )%>%
#  # Exclude countries or NUTS2 codes starting with "DE" or "UK"
#  filter(!grepl("^(DE|UK)", country), !grepl("^(DE|UK)", nuts2))

# ---------------------------
# Create Economic Threat Index (Reverse-Coded)
# ---------------------------
df <- df %>%
  mutate(
    economic_threat_index = rowMeans(as.matrix(.[, c("immigration_economy", "perceived_economic_competition", "jobs_taken")]), na.rm = TRUE)
  ) %>%
  mutate(
    economic_threat_index = (economic_threat_index - min(economic_threat_index, na.rm = TRUE)) /
      (max(economic_threat_index, na.rm = TRUE) - min(economic_threat_index, na.rm = TRUE))
  ) %>%
  mutate(
    economic_threat_index = 1 - economic_threat_index  # reverse code: higher = higher threat
  )

# ---------------------------
# Decompose Predictors: Within- and Between-Group Centering
# ---------------------------
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
    unemployment_c_dm = unemployment_c - mean(unemployment_c, na.rm = TRUE),
    unemployment_c_m = mean(unemployment_c, na.rm = TRUE),
    gdppc_c_dm = gdppc_c - mean(gdppc_c, na.rm = TRUE),
    gdppc_c_m = mean(gdppc_c, na.rm = TRUE),
    net_mig_c_dm = net_mig_c - mean(net_mig_c, na.rm = TRUE),
    net_mig_c_m = mean(net_mig_c, na.rm = TRUE)
  ) %>%
  ungroup()


fp_model <- mfp(imwbcnt ~ fp(social_contact_dm, df = 2) + fp(economic_threat_dm, df = 3) +
                  lrscale_dm + age + gndr + factor(year) + factor(country) + factor(nuts2),
                family = gaussian, data = df_sample)

# Fit GLM with continuous and categorical predictors
fp_model_mfp <- mfp(imwbcnt ~ 
                      fp(economic_threat_index_dm) + 
                      fp(social_contact_dm) + 
                      fp(gdppc_c_dm) + 
                      fp(lrscale_dm) + 
                      education_numeric + 
                      fp(net_mig_dm) + 
                      fp(unemployment_c_dm) + 
                      fp(gdppc_dm) + 
                      fp(age) + 
                      fp(unemployment_dm) + 
                      gndr + 
                      fp(pop_density_dm) + 
                      fp(minority_presence_dm), 
                    family = gaussian, 
                    data = df)

summary(fp_model_mfp)
fp_model_glm <- glm(imwbcnt ~ 
                      I((economic_threat_index_dm + 0.6)^3) +  
                      I((social_contact_dm + 1)^2) + 
                      gdppc_c_dm +  
                      lrscale_dm + 
                      education_numeric + 
                      net_mig_dm +  
                      unemployment_c_dm + 
                      gdppc_dm + 
                      age +  household_income_dm +
                      unemployment_dm + 
                      gndr + 
                      pop_density_dm +  
                      minority_presence_dm + 
                      factor(year) + 
                      factor(nuts2) + 
                      factor(country), 
                    family = gaussian, 
                    data = df)

tab_model(fp_model_glm)
plot(fp_model)
#fp_test <- mfp(imwbcnt ~ fp(social_contact_dm, df = 2) + 
#                  fp(economic_threat_index_dm, df = 3) +
#                  pop_density_dm + unemployment_dm + gdppc_dm + net_mig_dm +
#                  unemployment_c_dm + gdppc_c_dm +
#                  minority_presence_dm + 
#                  lrscale_dm + age + gndr + education_numeric + factor(year) + factor(nuts2) + factor(country),
#                family = gaussian, data = df)



fp_summary <- summary(fp_model)

fp_results <- as.data.frame(fp_summary$coefficients)

colnames(fp_results) <- c("Estimate", "Std. Error", "t value", "p-value")


tab_model(fp_model_glm)



ologit_model <- clm(imwbcnt ~ pop_density_dm + unemployment_dm + gdppc_dm + net_mig_dm +
                 unemployment_c_dm + gdppc_c_dm + 
                 social_contact_dm + social_contact_m +
                 minority_presence_dm + minority_presence_m +
                 economic_threat_index_dm + economic_threat_index_m +
                 lrscale_dm + lrscale_m +
                 education_numeric + household_income_dm + age + gndr +
                 factor(year) + factor(nuts2) + factor(country),
               data = df, na.action = na.exclude)

clustered_se_model <- lm(imwbcnt ~ pop_density_dm + unemployment_dm + gdppc_dm + net_mig_dm +
                           unemployment_c_dm + gdppc_c_dm + 
                           social_contact_dm + social_contact_m +
                           minority_presence_dm + minority_presence_m +
                           economic_threat_index_dm + economic_threat_index_m +
                           lrscale_dm + lrscale_m +
                           education_numeric + household_income_dm + age + gndr +
                           factor(year) + factor(nuts2) + factor(country),
                         data = df,na.action = na.exclude)

clustered_se <- coeftest(clustered_se_model, vcov = vcovCL(clustered_se_model, cluster = ~nuts2))
clustered_se

df$immig_background
df_native <- df %>% filter(immig_background == "0")
df_immigrants <- df %>% filter(immig_background == "1")

native_model <- lm(imwbcnt ~ pop_density_dm + unemployment_dm + gdppc_dm + net_mig_dm +
                     unemployment_c_dm + gdppc_c_dm + 
                     social_contact_dm + social_contact_m +
                     minority_presence_dm + minority_presence_m +
                     economic_threat_index_dm + economic_threat_index_m +
                     lrscale_dm + lrscale_m +
                     education_numeric + household_income_dm + age + gndr +
                     factor(year) + factor(nuts2) + factor(country),
                   data = df_native, na.action = na.exclude)

immigrant_model <- lm(imwbcnt ~ pop_density_dm + unemployment_dm + gdppc_dm + net_mig_dm +
                        unemployment_c_dm + gdppc_c_dm + 
                        social_contact_dm + social_contact_m +
                        minority_presence_dm + minority_presence_m +
                        economic_threat_index_dm + economic_threat_index_m +
                        lrscale_dm + lrscale_m +
                        education_numeric + household_income_dm + age + gndr +
                        factor(year) + factor(nuts2) + factor(country),
                      data = df_immigrants, na.action = na.exclude)

df_excl <- df %>% filter(country != "FR")  
excl_model <- lm(imwbcnt ~ pop_density_dm + unemployment_dm + gdppc_dm + net_mig_dm +
                   unemployment_c_dm + gdppc_c_dm + 
                   social_contact_dm + social_contact_m +
                   minority_presence_dm + minority_presence_m +
                   economic_threat_index_dm + economic_threat_index_m +
                   lrscale_dm + lrscale_m +
                   education_numeric + household_income_dm + age + gndr +
                   factor(year) + factor(nuts2) + factor(country),
                 data = df_excl,na.action = na.exclude)
df_excl_more <- df %>% filter(country != "AT" & country !="FR") 
excl_more_model <- lm(imwbcnt ~ pop_density_dm + unemployment_dm + gdppc_dm + net_mig_dm +
                        unemployment_c_dm + gdppc_c_dm + 
                        social_contact_dm + social_contact_m +
                        minority_presence_dm + minority_presence_m +
                        economic_threat_index_dm + economic_threat_index_m +
                        lrscale_dm + lrscale_m +
                        education_numeric + household_income_dm + age + gndr +
                        factor(year) + factor(nuts2) + factor(country),
                 data = df_excl_more,na.action = na.exclude)

df_no2015 <- df %>% filter(year != "2015")
no2015_model <- lm(imwbcnt ~ pop_density_dm + unemployment_dm + gdppc_dm + net_mig_dm +
                     unemployment_c_dm + gdppc_c_dm + 
                     social_contact_dm + social_contact_m +
                     minority_presence_dm + minority_presence_m +
                     economic_threat_index_dm + economic_threat_index_m +
                     lrscale_dm + lrscale_m +
                     education_numeric + household_income_dm + age + gndr +
                     factor(year) + factor(nuts2) + factor(country),
                   data = df_no2015)

tab_model(fp_model_glm,  clustered_se_model, ologit_model,native_model, immigrant_model, excl_model,excl_more_model, no2015_model,
          dv.labels = c("Fractional Polynomial", "Clustered SE", "Ordered Logistic" ,"Native Only", "Immigrants Only", "Excluding Country","Exclude more countries", "Without 2015"),
          title = "Table 4: Robustness Checks",
          collapse.se = TRUE,
          show.ci = FALSE,
          show.aic = TRUE,
          p.threshold = c(0.05, 0.01, 0.001),
          show.se = TRUE,show.reflvl = FALSE,
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
            
            #  Within (WE) and Between (BE) examples
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
          
          # 7) Additional labeling arguments
          string.pred = "Predictors",        
          string.est  = "Est. (SE)",         
          string.p    = "p-value",  
          
          # Exclude certain terms
          #rm.terms = c("factor\\(\\s*nuts2\\s*\\).*","factor\\(\\s*country\\s*\\).*"),  
          
          # Save the table to an HTML file
          file = "robustness_checks.html"
)

#fp_model_glm_fixed <- glm(imwbcnt ~ 
#                            poly(social_contact_dm, 2) + 
#                            poly(economic_threat_index_dm, 3) + 
#                            lrscale_dm + age + gndr + factor(year) + factor(country) + factor(nuts2),
#                          data = df, family = gaussian)

fp_model_glm_fixed <- glm(imwbcnt ~ poly(social_contact_dm, 2) + 
                            poly(economic_threat_index_dm, 3) + 
                            pop_density_dm + unemployment_dm + gdppc_dm + net_mig_dm +
                            unemployment_c_dm + gdppc_c_dm + 
                             social_contact_m +
                            minority_presence_dm + minority_presence_m +
                            economic_threat_index_m +
                            lrscale_dm + lrscale_m +
                            education_numeric + household_income_dm + age + gndr +
                            factor(year) + factor(nuts2) + factor(country),
                          data = df, family = gaussian)

tab_model(fp_model_glm_fixed, clustered_se_model, ologit_model, 
          native_model, immigrant_model, excl_model, excl_more_model, 
          no2015_model,
          
          dv.labels = c("Fractional Polynomial", "Clustered Robust SEs(main model)", 
                        "Ordered Logistic", "Native Only", 
                        "Immigrants Only", "Excluding Country", 
                        "Exclude More Countries", "Without 2015"),
          
          title = "Table 4: Robustness Checks",
          show.ci = FALSE,      
          show.aic = TRUE, 
          collapse.se = TRUE,   
          p.style = "stars",    
          emph.p = TRUE,        
          show.se = TRUE,       
          show.reflvl = FALSE,  
          digits = 3,           
          show.p = FALSE,       
          
          pred.labels = c(
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
          string.pred = "",
          
          # Hide the Intercept row
          rm.terms = c("factor\\(\\s*nuts2\\s*\\).*", "(Intercept)","term","Predictors"),
          
          # Save to file
          file = "robustness_checks_02.html"
)
bic_values <- c(
  BIC(fp_model_glm_fixed),
  BIC(clustered_se_model), 
  BIC(ologit_model),
  BIC(native_model),
  BIC(immigrant_model),
  BIC(excl_model),
  BIC(excl_more_model),
  BIC(no2015_model)
)
cat("\n**BIC Values for Each Model:**\n")
print(bic_values)

# Compute AIC and BIC for each model
aic_values <- c(
  AIC(fp_model_glm_fixed),
  AIC(clustered_se_model),
  AIC(ologit_model),
  AIC(native_model),
  AIC(immigrant_model),
  AIC(excl_model),
  AIC(excl_more_model),
  AIC(no2015_model)
)

bic_values <- c(
  BIC(fp_model_glm_fixed),
  BIC(clustered_se_model),
  BIC(ologit_model),
  BIC(native_model),
  BIC(immigrant_model),
  BIC(excl_model),
  BIC(excl_more_model),
  BIC(no2015_model)
)

model_names <- c(
  "Fractional Polynomial",
  "Clustered Robust SEs (main model)",
  "Ordered Logistic",
  "Native Only",
  "Immigrants Only",
  "Excluding Country",
  "Exclude More Countries",
  "Without 2015"
)

aic_bic_df <- data.frame(Model = model_names, AIC = aic_values, BIC = bic_values)
print(aic_bic_df)




