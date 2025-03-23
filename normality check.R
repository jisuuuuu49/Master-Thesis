# ---------------------------
install.packages(c("gridExtra","ggeffects","dplyr","sandwich","lmtest","lme4", "tidyverse", 
                   "performance", "sjPlot", "insight", "splines", "clubSandwich", "segmented"))
library(dplyr)
library(tidyverse)
library(lme4)
library(sandwich)
library(lmtest)
library(sjPlot)
library(splines)
library(clubSandwich)
library(segmented)
library(performance)
library(ggeffects)
library(gridExtra)
# ---------------------------
# 2) Set Working Directory & Load Data
# ---------------------------
setwd("/Users/aunal/Desktop/Uni/Study/ss 2024/master thesis/data")
df <- read_csv("ESS_EUROSTAT_final_standardized.csv") %>%
  janitor::clean_names() %>%
  filter(year != 9999, gndr != 9) %>%
  mutate(
    year = as.factor(year),
    gndr = as.factor(gndr),
    immig_background = as.factor(immig_background),
    education_numeric = as.factor(education_numeric),
    have_child = as.factor(have_child)
  )


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


ggplot(df, aes(x = imwbcnt)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Outgroup Hostility (imwbcnt)",
       x = "imwbcnt", y = "Count") +
  theme_minimal()

ols_raw <- lm(imwbcnt ~ social_contact + economic_threat_index + lrscale + household_income +
                gdppc + pop_density + unemployment + age + gndr + factor(year) + factor(country),
              data = df, na.action = na.exclude)

ggplot(data.frame(resid = residuals(ols_raw)), aes(x = resid)) +
  geom_histogram(bins = 30, fill = "lightgreen", color = "black") +
  labs(title = "Histogram of Residuals from OLS Model", x = "Residuals", y = "Count") +
  theme_minimal()

qqnorm(residuals(ols_raw))
qqline(residuals(ols_raw), col = "red", lwd = 2)
