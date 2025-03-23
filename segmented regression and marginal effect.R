# ---- Install & Load Necessary Packages ----
install.packages(c("performance","segmented","sjPlot","dplyr","sandwich",
                   "lmtest","lme4", "tidyverse", "insight", "splines", "clubSandwich"))
library(lme4)
library(tidyverse)
library(performance)
library(sjPlot)
library(insight)
library(splines)
library(clubSandwich)
library(sandwich)
library(lmtest)
library(segmented)

# ---- Set Working Directory ----
setwd("/Users/aunal/Desktop/Uni/Study/ss 2024/master thesis/data")

# ---- Load & Clean Data ----
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

# ---- Create Economic Threat Index ----
# (Average three items, scale between 0 and 1, then reverse-code so that higher = higher threat)
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
    social_contact_m  = mean(social_contact, na.rm = TRUE),
    economic_threat_index_dm = economic_threat_index - mean(economic_threat_index, na.rm = TRUE),
    economic_threat_index_m  = mean(economic_threat_index, na.rm = TRUE),
    lrscale_dm = lrscale - mean(lrscale, na.rm = TRUE),
    lrscale_m  = mean(lrscale, na.rm = TRUE),
    household_income_dm = household_income - mean(household_income, na.rm = TRUE),
    household_income_m  = mean(household_income, na.rm = TRUE),
    minority_presence_dm = minority_presence - mean(minority_presence, na.rm = TRUE),
    minority_presence_m  = mean(minority_presence, na.rm = TRUE),
    pop_density_dm = pop_density - mean(pop_density, na.rm = TRUE),
    pop_density_m  = mean(pop_density, na.rm = TRUE),
    unemployment_dm = unemployment - mean(unemployment, na.rm = TRUE),
    unemployment_m  = mean(unemployment, na.rm = TRUE),
    gdppc_dm = gdppc - mean(gdppc, na.rm = TRUE),
    gdppc_m  = mean(gdppc, na.rm = TRUE),
    net_mig_dm = net_mig - mean(net_mig, na.rm = TRUE),
    net_mig_m  = mean(net_mig, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  group_by(country) %>%
  mutate(
    pop_density_c_dm = pop_density - mean(pop_density, na.rm = TRUE),
    pop_density_c_m  = mean(pop_density, na.rm = TRUE),
    unemployment_c_dm = unemployment - mean(unemployment, na.rm = TRUE),
    unemployment_c_m  = mean(unemployment, na.rm = TRUE),
    gdppc_c_dm = gdppc - mean(gdppc, na.rm = TRUE),
    gdppc_c_m  = mean(gdppc, na.rm = TRUE),
    net_mig_c_dm = minority_presence - mean(net_mig, na.rm = TRUE),
    net_mig_c_m  = mean(net_mig, na.rm = TRUE)
  ) %>%
  ungroup()

# (Optional: Re-compute within-group demeaning for social_contact and net_mig by nuts2)
df <- df %>%
  group_by(nuts2) %>%
  mutate(
    social_contact_dm = social_contact - mean(social_contact, na.rm = TRUE),
    net_mig_dm = net_mig - mean(net_mig, na.rm = TRUE)
  ) %>%
  ungroup()

# ---- Compute Double-Demeaned Interaction Term (interaction_dd) for social_contact_dm and net_mig_dm ----
df <- df %>%
  group_by(nuts2) %>%
  mutate(
    interaction_dd = social_contact_dm * net_mig_dm - mean(social_contact_dm * net_mig_dm, na.rm = TRUE)
  ) %>%
  ungroup()

# ---- Fit a Combined Linear Model (lm_combined) ----
# Note: To make segmentation more stable, we drop the interaction term here.
lm_combined <- lm(imwbcnt ~ social_contact_dm + economic_threat_index_dm +
                    lrscale_dm + lrscale_m + education_numeric + household_income_dm +
                    gdppc_dm + pop_density_dm + unemployment_dm +
                    gdppc_c_dm + pop_density_c_dm + unemployment_c_dm + net_mig_c_dm +
                    age + gndr + factor(year) + factor(nuts2) + factor(country),
                  data = df, na.action = na.exclude)
summary(lm_combined)


base_linear_no_interaction <- lm(
  imwbcnt ~ social_contact_dm + economic_threat_index_dm + lrscale_dm + age + gndr,
  data = df,
  na.action = na.exclude
)
seg_mod_combined <- segmented(
  base_linear_no_interaction,
  seg.Z = ~ social_contact_dm,
  psi = 0.3  
)
summary(seg_mod_combined)


typical_threat <- mean(df$economic_threat_index_dm, na.rm = TRUE)
typical_lr     <- mean(df$lrscale_dm, na.rm = TRUE)
typical_age    <- mean(df$age, na.rm = TRUE)
typical_gndr   <- "1"   

grid_data <- data.frame(
  social_contact_dm = seq(
    min(df$social_contact_dm, na.rm=TRUE),
    max(df$social_contact_dm, na.rm=TRUE),
    length.out = 200
  ),
  economic_threat_index_dm = typical_threat,
  lrscale_dm = typical_lr,
  age = typical_age,
  gndr = typical_gndr
)
pred_seg <- predict(seg_mod_combined, newdata=grid_data, se.fit=TRUE)

grid_data$fit <- pred_seg$fit
grid_data$upr <- pred_seg$fit + 1.96 * pred_seg$se.fit
grid_data$lwr <- pred_seg$fit - 1.96 * pred_seg$se.fit
library(ggplot2)

bp_value <- seg_mod_combined$psi[2]  

plot_segmented <-ggplot(grid_data, aes(x = social_contact_dm, y = fit)) +

  geom_line(color = "red", size = 1) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "red", alpha = 0.2) +
  geom_vline(xintercept = bp_value, linetype = "dashed", color = "blue", size = 1) +
  labs(
    title = "Figure 3: Segmented Regression for Social Contact",
    subtitle = paste("Breakpoint at social contact ≈", round(bp_value, 3)),
    x = "Social Contact",
    y = "Predicted outgroup hostility"
  ) +
  theme_minimal(base_size = 14)

marginal_model <- lm(imwbcnt ~ social_contact_dm + economic_threat_index_dm + 
                       lrscale_dm + age + gndr + factor(year) + factor(nuts2),
                     data = df, na.action = na.exclude)

pred_marginal <- ggpredict(marginal_model, terms = "social_contact_dm")

plot_marginal <- ggplot(pred_marginal, aes(x = x, y = predicted)) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "blue", alpha = 0.2) +
  labs(
    title = "Figure 4: Marginal Effect of Social Contact on Outgroup Hostility",
    x = "Social Contact",
    y = "Predicted Outgroup Hostility"
  ) +
  theme_minimal(base_size = 14)

grid.arrange(
  plot_segmented, plot_marginal,  # Use the assigned variable names
  ncol = 2

)
# ---- Plot Partial Residuals vs. social_contact_dm with the Segmented Fit ----
plot(df$social_contact_dm, residuals(base_linear_no_interaction),
     pch = 20, col = rgb(0, 0, 0, 0.4),
     xlab = "social_contact_dm", ylab = "Residuals of imwbcnt (base model)")
grid_vals <- data.frame(social_contact_dm = seq(min(df$social_contact_dm, na.rm = TRUE),
                                                max(df$social_contact_dm, na.rm = TRUE),
                                                length.out = 200))
grid_vals$yhat <- predict(seg_mod_combined, newdata = grid_vals)
lines(grid_vals$social_contact_dm, grid_vals$yhat, col = "red", lwd = 2)
abline(v = seg_mod_combined$psi[1], col = "blue", lty = 2, lwd = 2)

# ---- Check Data Distribution for social_contact_dm ----
ggplot(df, aes(x = social_contact_dm)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  geom_vline(xintercept = seg_mod_combined$psi[1], color = "red", linetype = "dashed", size = 1) +
  labs(title = "Histogram of social_contact_dm with Breakpoint",
       x = "social_contact_dm", y = "Count")

ggplot(df, aes(x = social_contact_dm)) +
  geom_density(fill = "skyblue", alpha = 0.4) +
  geom_vline(xintercept = seg_mod_combined$psi[1], color = "red", linetype = "dashed", size = 1) +
  labs(title = "Density of social_contact_dm with Breakpoint",
       x = "social_contact_dm", y = "Density")

ggplot(df, aes(x = social_contact_dm)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  geom_vline(xintercept = 0.291, color = "red", linetype = "dashed", size = 1) +
  labs(title = "Histogram of Social Contact (Demeaned)",
       x = "social_contact_dm",
       y = "Count")

ols_fe_interaction_spline <- lm(
  imwbcnt ~ ns(social_contact_dm, df = 3) * ns(economic_threat_index_dm, df = 3) +
    lrscale_dm + lrscale_m + education_numeric + household_income_dm +
    gdppc_dm + pop_density_dm + unemployment_dm + age + gndr + gdppc_c_dm + pop_density_c_dm + unemployment_c_dm + net_mig_c_dm +
    factor(year) + factor(nuts2), #+ factor(country),
  data = df
)


plot_model(ols_fe_interaction_spline,
           type = "pred",
           terms = c("social_contact_dm", "economic_threat_index_dm [0.2, 0.5, 0.8]"),
           title = "Predicted outgroup hostility by Social Contact (with Spline Interaction)",
           axis.title = c("Social Contact (demeaned)", "Predicted outgroup hostility"))
summary(ols_fe_interaction_spline)
clustered_se <- vcovCL(ols_fe_interaction_spline, cluster = ~nuts2)
coeftest(ols_fe_interaction_spline, clustered_se)





lm_combined <- lm(imwbcnt ~ social_contact_dm + economic_threat_index_dm +
                    lrscale_dm + age + gndr,
                  data = df, na.action = na.exclude)
summary(lm_combined)
seg_mod_combined <- segmented(lm_combined,
                              seg.Z = ~ social_contact_dm,
                              psi = quantile(df$social_contact_dm, 0.5, na.rm = TRUE))
summary(seg_mod_combined)
plot(seg_mod_combined, conf.level = 0.95)

model_linear <- lm(imwbcnt ~ social_contact_dm * economic_threat_index_dm +
                     lrscale_dm + lrscale_m + education_numeric + household_income_dm +
                     gdppc_dm + pop_density_dm + unemployment_dm + age + gndr +
                     factor(year) + factor(nuts2) + factor(country),
                   data = df)

model_spline <- lm(imwbcnt ~ ns(social_contact_dm, df = 3) * ns(economic_threat_index_dm, df = 3) +
                     lrscale_dm + lrscale_m + education_numeric + household_income_dm +
                     gdppc_dm + pop_density_dm + unemployment_dm + age + gndr +
                     factor(year) + factor(nuts2) + factor(country),
                   data = df)

# Compare models with AIC/BIC
AIC_vals <- AIC(model_linear, model_spline)
BIC_vals <- BIC(model_linear, model_spline)
print(AIC_vals)
print(BIC_vals)

# ---------------------------
# Plot Partial Effects from the Spline Model
# ---------------------------
plot_model(model_spline,
           type = "pred",
           terms = c("social_contact_dm", "economic_threat_index_dm [0.2, 0.5, 0.8]"),
           title = "Predicted outgroup hostility by Social Contact at Different Threat Levels",
           axis.title = c("Social Contact (demeaned)", "Predicted outgroup hostility")) +
  theme_minimal() +
  theme(legend.position = "right",
        axis.text = element_text(size = 11, color = "black"),
        axis.title = element_text(size = 12))


