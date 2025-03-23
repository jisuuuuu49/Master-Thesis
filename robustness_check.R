# ---- Install & Load Necessary Packages ----
install.packages(c("ordinal","mfp","htmltools","webshot","webshot2","insight","sandwich","lmtest","lme4", "tidyverse", "performance", "sjPlot", "insight", "splines", "clubSandwich"))
library(webshot2)
library(webshot)
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

setwd("/Users/aunal/Desktop/Uni/Study/ss 2024/master thesis/data")
df <- read_csv("ESS_EUROSTAT_final_standardized.csv") %>%
  janitor::clean_names() %>%
  filter(year != 9999) %>%
  filter(gndr != 9) %>%
  mutate(
    year = as.factor(year),
    gndr = as.factor(gndr),
    immig_background = as.factor(immig_background),
    #education_numeric = as.factor(education_numeric),
    #have_child = as.factor(have_child)
  )%>%
  # Exclude countries or NUTS2 codes starting with "DE" or "UK"
  filter(!grepl("^(DE|UK)", country), !grepl("^(DE|UK)", nuts2))

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
    pop_density_c_dm = pop_density - mean(pop_density, na.rm = TRUE),
    pop_density_c_m = mean(pop_density, na.rm = TRUE),
    unemployment_c_dm = unemployment - mean(unemployment, na.rm = TRUE),
    unemployment_c_m = mean(unemployment, na.rm = TRUE),
    gdppc_c_dm = gdppc - mean(gdppc, na.rm = TRUE),
    gdppc_c_m = mean(gdppc, na.rm = TRUE),
    net_mig_c_dm = minority_presence - mean(net_mig, na.rm = TRUE),
    net_mig_c_m = mean(net_mig, na.rm = TRUE)
  ) %>%
  ungroup()


df_sample <- df_sample %>%
  mutate(
    social_contact_fp = cut(social_contact_dm, breaks = quantile(social_contact_dm, probs = seq(0, 1, 0.1), na.rm = TRUE), labels = FALSE, include.lowest = TRUE),
    economic_threat_fp = cut(economic_threat_index_dm, breaks = quantile(economic_threat_index_dm, probs = seq(0, 1, 0.1), na.rm = TRUE), labels = FALSE, include.lowest = TRUE)
  )
df_sample <- df_sample %>%
  mutate(
    social_contact_fp = cut(social_contact_dm, 
                            breaks = unique(quantile(social_contact_dm, probs = seq(0, 1, 0.1), na.rm = TRUE)), 
                            labels = FALSE, include.lowest = TRUE),
    
    economic_threat_fp = cut(economic_threat_index_dm, 
                             breaks = unique(quantile(economic_threat_index_dm, probs = seq(0, 1, 0.1), na.rm = TRUE)), 
                             labels = FALSE, include.lowest = TRUE),
    
    pop_density_fp = cut(pop_density_dm, 
                         breaks = unique(quantile(pop_density_dm, probs = seq(0, 1, 0.1), na.rm = TRUE)), 
                         labels = FALSE, include.lowest = TRUE),
    
    unemployment_fp = cut(unemployment_dm, 
                          breaks = unique(quantile(unemployment_dm, probs = seq(0, 1, 0.1), na.rm = TRUE)), 
                          labels = FALSE, include.lowest = TRUE),
    
    gdppc_fp = cut(gdppc_dm, 
                   breaks = unique(quantile(gdppc_dm, probs = seq(0, 1, 0.1), na.rm = TRUE)), 
                   labels = FALSE, include.lowest = TRUE),
    
    net_mig_fp = cut(net_mig_dm, 
                     breaks = unique(quantile(net_mig_dm, probs = seq(0, 1, 0.1), na.rm = TRUE)), 
                     labels = FALSE, include.lowest = TRUE),
    
    unemployment_c_fp = cut(unemployment_c_dm, 
                            breaks = unique(quantile(unemployment_c_dm, probs = seq(0, 1, 0.1), na.rm = TRUE)), 
                            labels = FALSE, include.lowest = TRUE),
    
    gdppc_c_fp = cut(gdppc_c_dm, 
                     breaks = unique(quantile(gdppc_c_dm, probs = seq(0, 1, 0.1), na.rm = TRUE)), 
                     labels = FALSE, include.lowest = TRUE),
    
    minority_presence_fp = cut(minority_presence_dm, 
                               breaks = unique(quantile(minority_presence_dm, probs = seq(0, 1, 0.1), na.rm = TRUE)), 
                               labels = FALSE, include.lowest = TRUE),
    
    lrscale_fp = cut(lrscale_dm, 
                     breaks = unique(quantile(lrscale_dm, probs = seq(0, 1, 0.1), na.rm = TRUE)), 
                     labels = FALSE, include.lowest = TRUE),
    
    education_numeric_fp = cut(as.numeric(education_numeric), 
                               breaks = unique(quantile(as.numeric(education_numeric), probs = seq(0, 1, 0.1), na.rm = TRUE)), 
                               labels = FALSE, include.lowest = TRUE),
    
    household_income_fp = cut(household_income_dm, 
                              breaks = unique(quantile(household_income_dm, probs = seq(0, 1, 0.1), na.rm = TRUE)), 
                              labels = FALSE, include.lowest = TRUE),
    
    age_fp = cut(age, 
                 breaks = unique(quantile(age, probs = seq(0, 1, 0.1), na.rm = TRUE)), 
                 labels = FALSE, include.lowest = TRUE)
  )


fp_model <- mfp(imwbcnt ~ fp(social_contact_fp, df = 2) + fp(economic_threat_fp, df = 2) +
                  lrscale_dm + age + gndr + year_numeric,
                family = gaussian, data = df_sample)
fp_model <- mfp(imwbcnt ~ 
                  fp(social_contact_dm) + #fp(social_contact_dm) + 
                  economic_threat_index_dm +
                  pop_density_dm + unemployment_dm + gdppc_dm + net_mig_dm +
                  unemployment_c_dm + gdppc_c_dm +
                  social_contact_m +
                  minority_presence_dm + minority_presence_m +
                  economic_threat_index_m +
                  lrscale_dm + lrscale_m +
                  education_numeric + household_income_dm + age + gndr +
                  year_numeric, #+ factor(country),  # Replacing factor(year) with numeric form
                family = gaussian, data = df_sample)
summary(fp_model)
fp_model <- mfp(imwbcnt ~ fp(social_contact_fp, df = 2) + fp(economic_threat_fp, df = 2) +
                                    lrscale_dm + age + gndr + year_numeric,
                                family = gaussian, data = df_sample)
fp_model <- mfp(imwbcnt ~ fp(social_contact_dm, df = 2) + 
                  fp(economic_threat_index_dm, df = 2) +
                  pop_density_dm + unemployment_dm + gdppc_dm + net_mig_dm +
                  unemployment_c_dm + gdppc_c_dm +
                  minority_presence_dm + 
                  lrscale_dm + age + gndr + education_numeric + factor(year) + factor(nuts2),
                family = gaussian, data = df)


summary(fp_model)
tab_model(fp_model)
plot(fp_model)

fp_summary <- summary(fp_model)

fp_results <- as.data.frame(fp_summary$coefficients)

colnames(fp_results) <- c("Estimate", "Std. Error", "t value", "p-value")

fp_model_glm <- glm(imwbcnt ~ I((economic_threat_index_dm + 0.6)^1) +  
                      I((social_contact_dm + 1)^1) + gdppc_c_dm +  
                      lrscale_dm + education_numeric + net_mig_dm +  
                      unemployment_c_dm + gdppc_dm + age +  
                      unemployment_dm + gndr + pop_density_dm +  
                      minority_presence_dm, 
                    family = gaussian, data = df_sample)

tab_model(fp_model_glm)


df <- df %>%
  mutate(imwbcnt_ordinal = cut(imwbcnt, 
                               breaks = seq(min(imwbcnt, na.rm = TRUE), max(imwbcnt, na.rm = TRUE), length.out = 11), 
                               labels = FALSE, 
                               include.lowest = TRUE))

df$imwbcnt_ordinal <- factor(df$imwbcnt_ordinal, ordered = TRUE)


ologit_model <- clm(imwbcnt ~ pop_density_dm + unemployment_dm + gdppc_dm + net_mig_dm +
                 unemployment_c_dm + gdppc_c_dm + 
                 social_contact_dm + social_contact_m +
                 minority_presence_dm + minority_presence_m +
                 economic_threat_index_dm + economic_threat_index_m +
                 lrscale_dm + lrscale_m +
                 education_numeric + household_income_dm + age + gndr +
                 factor(year) + factor(nuts2),
               data = df, na.action = na.exclude)
clustered_se_model <- lm(imwbcnt ~ pop_density_dm + unemployment_dm + gdppc_dm + net_mig_dm +
                           unemployment_c_dm + gdppc_c_dm + 
                           social_contact_dm + social_contact_m +
                           minority_presence_dm + minority_presence_m +
                           economic_threat_index_dm + economic_threat_index_m +
                           lrscale_dm + lrscale_m +
                           education_numeric + household_income_dm + age + gndr +
                           factor(year) + factor(nuts2),
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
                     factor(year) + factor(nuts2),
                   data = df_native, na.action = na.exclude)

immigrant_model <- lm(imwbcnt ~ pop_density_dm + unemployment_dm + gdppc_dm + net_mig_dm +
                        unemployment_c_dm + gdppc_c_dm + 
                        social_contact_dm + social_contact_m +
                        minority_presence_dm + minority_presence_m +
                        economic_threat_index_dm + economic_threat_index_m +
                        lrscale_dm + lrscale_m +
                        education_numeric + household_income_dm + age + gndr +
                        factor(year) + factor(nuts2),
                      data = df_immigrants, na.action = na.exclude)

df_excl <- df %>% filter(country != "FR")  # Example exclusion (France & Italy)
excl_model <- lm(imwbcnt ~ pop_density_dm + unemployment_dm + gdppc_dm + net_mig_dm +
                   unemployment_c_dm + gdppc_c_dm + 
                   social_contact_dm + social_contact_m +
                   minority_presence_dm + minority_presence_m +
                   economic_threat_index_dm + economic_threat_index_m +
                   lrscale_dm + lrscale_m +
                   education_numeric + household_income_dm + age + gndr +
                   factor(year) + factor(nuts2),
                 data = df_excl,na.action = na.exclude)
df_excl_more <- df %>% filter(country != "AT" & country !="FR") 
excl_more_model <- lm(imwbcnt ~ pop_density_dm + unemployment_dm + gdppc_dm + net_mig_dm +
                        unemployment_c_dm + gdppc_c_dm + 
                        social_contact_dm + social_contact_m +
                        minority_presence_dm + minority_presence_m +
                        economic_threat_index_dm + economic_threat_index_m +
                        lrscale_dm + lrscale_m +
                        education_numeric + household_income_dm + age + gndr +
                        factor(year) + factor(nuts2),
                 data = df_excl_more,na.action = na.exclude)

df_no2015 <- df %>% filter(year != "2015")
no2015_model <- lm(imwbcnt ~ pop_density_dm + unemployment_dm + gdppc_dm + net_mig_dm +
                     unemployment_c_dm + gdppc_c_dm + 
                     social_contact_dm + social_contact_m +
                     minority_presence_dm + minority_presence_m +
                     economic_threat_index_dm + economic_threat_index_m +
                     lrscale_dm + lrscale_m +
                     education_numeric + household_income_dm + age + gndr +
                     factor(year) + factor(nuts2),
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
            
            # If you have country-level decomposition (pop_density_c_dm, etc.), you can also rename:
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
          rm.terms = c("factor\\(\\s*nuts2\\s*\\).*"),  
          
          # Save the table to an HTML file
          file = "robustness_checks.html"
)
fp_model_glm_fixed <- glm(imwbcnt ~ poly(social_contact_dm, 2) + 
                            poly(economic_threat_index_dm, 2) + 
                            lrscale_dm + age + gndr + factor(year),
                          data = df, family = gaussian)

fp_model_glm_fixed <- glm(imwbcnt ~ poly(social_contact_dm, 2) + 
                            poly(economic_threat_index_dm, 2) + 
                            pop_density_dm + unemployment_dm + gdppc_dm + net_mig_dm +
                            unemployment_c_dm + gdppc_c_dm + 
                             social_contact_m +
                            minority_presence_dm + minority_presence_m +
                            economic_threat_index_m +
                            lrscale_dm + lrscale_m +
                            education_numeric + household_income_dm + age + gndr +
                            factor(year) + factor(nuts2),
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




