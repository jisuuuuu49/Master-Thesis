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
library(segmented)

# ---------------------------
# Set Working Directory & Load Data
# ---------------------------
#setwd("/Users/aunal/Desktop/Uni/Study/ss 2024/master thesis/data")

#df <- read_csv("ESS_EUROSTAT_final_standardized.csv") %>%
#  janitor::clean_names() %>%
#  filter(year != 9999, gndr != 9) %>%
#  mutate(
#    year = as.factor(year),
#    gndr = as.factor(gndr),
#    immig_background = as.factor(immig_background),
#    education_numeric = as.factor(education_numeric)
#  )

# ---------------------------
# Create Economic Threat Index (Reversed)
# ---------------------------
#df <- df %>%
#  mutate(
#    economic_threat_index = rowMeans(across(c(immigration_economy, perceived_economic_competition, jobs_taken)), na.rm = TRUE),
#    economic_threat_index = (economic_threat_index - min(economic_threat_index, na.rm = TRUE)) /
#                            (max(economic_threat_index, na.rm = TRUE) - min(economic_threat_index, na.rm = TRUE)),
#    economic_threat_index = 1 - economic_threat_index
#  )

# ---------------------------
# Create Within- and Between-Group Variables
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
    net_mig_dm = net_mig - mean(net_mig, na.rm = TRUE),
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

# ---------------------------
# Double Demeaning & Interaction Variable
# ---------------------------
df <- df %>%
  group_by(nuts2) %>%
  mutate(
    social_contact_dm = social_contact - mean(social_contact, na.rm = TRUE),
    net_mig_dm = net_mig - mean(net_mig, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  group_by(nuts2) %>%
  mutate(
    interaction_dd = social_contact_dm * net_mig_dm - mean(social_contact_dm * net_mig_dm, na.rm = TRUE)
  ) %>%
  ungroup()

# ---------------------------
# Fit Linear & Segmented Models
# ---------------------------
lm_combined <- lm(imwbcnt ~ social_contact_dm + economic_threat_index_dm +
                    lrscale_dm + lrscale_m + education_numeric + household_income_dm +
                    gdppc_dm + pop_density_dm + unemployment_dm +
                    gdppc_c_dm + pop_density_c_dm + unemployment_c_dm + net_mig_c_dm +
                    age + gndr + factor(year) + factor(nuts2) + factor(country), data = df)

base_linear_no_interaction <- lm(imwbcnt ~ social_contact_dm + economic_threat_index_dm + lrscale_dm + age + gndr, data = df)
seg_mod_combined <- segmented(base_linear_no_interaction, seg.Z = ~ social_contact_dm, psi = 0.3)

# ---------------------------
# Predict from Segmented Model & Plot
# ---------------------------
typical_threat <- mean(df$economic_threat_index_dm, na.rm = TRUE)
typical_lr <- mean(df$lrscale_dm, na.rm = TRUE)
typical_age <- mean(df$age, na.rm = TRUE)
typical_gndr <- "1"

grid_data <- data.frame(
  social_contact_dm = seq(min(df$social_contact_dm, na.rm = TRUE), max(df$social_contact_dm, na.rm = TRUE), length.out = 200),
  economic_threat_index_dm = typical_threat,
  lrscale_dm = typical_lr,
  age = typical_age,
  gndr = typical_gndr
)

pred_seg <- predict(seg_mod_combined, newdata = grid_data, se.fit = TRUE)
grid_data$fit <- pred_seg$fit
grid_data$upr <- pred_seg$fit + 1.96 * pred_seg$se.fit
grid_data$lwr <- pred_seg$fit - 1.96 * pred_seg$se.fit
bp_value <- seg_mod_combined$psi[2]

plot_segmented <- ggplot(grid_data, aes(x = social_contact_dm, y = fit)) +
  geom_line(color = "red", size = 1) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "red", alpha = 0.2) +
  geom_vline(xintercept = bp_value, linetype = "dashed", color = "blue", size = 1) +
  labs(title = "Figure 3: Segmented Regression for Social Contact",
       subtitle = paste("Breakpoint at social contact ≈", round(bp_value, 3)),
       x = "Social Contact", y = "Predicted outgroup hostility") +
  theme_minimal(base_size = 14)

# ---------------------------
# Marginal Effect Plot
# ---------------------------
marginal_model <- lm(imwbcnt ~ social_contact_dm + economic_threat_index_dm + lrscale_dm + age + gndr + factor(year) + factor(nuts2), data = df)
pred_marginal <- ggpredict(marginal_model, terms = "social_contact_dm")

plot_marginal <- ggplot(pred_marginal, aes(x = x, y = predicted)) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "blue", alpha = 0.2) +
  labs(title = "Figure 4: Marginal Effect of Social Contact on Outgroup Hostility",
       x = "Social Contact", y = "Predicted Outgroup Hostility") +
  theme_minimal(base_size = 14)

grid.arrange(plot_segmented, plot_marginal, ncol = 2)

# ---------------------------
# Residual Plots & Histogram of Social Contact
# ---------------------------
plot(df$social_contact_dm, residuals(base_linear_no_interaction), pch = 20, col = rgb(0, 0, 0, 0.4))
grid_vals <- data.frame(social_contact_dm = seq(min(df$social_contact_dm, na.rm = TRUE), max(df$social_contact_dm, na.rm = TRUE), length.out = 200))
grid_vals$yhat <- predict(seg_mod_combined, newdata = grid_vals)
lines(grid_vals$social_contact_dm, grid_vals$yhat, col = "red", lwd = 2)
abline(v = seg_mod_combined$psi[1], col = "blue", lty = 2, lwd = 2)

ggplot(df, aes(x = social_contact_dm)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  geom_vline(xintercept = seg_mod_combined$psi[1], color = "red", linetype = "dashed", size = 1)

ggplot(df, aes(x = social_contact_dm)) +
  geom_density(fill = "skyblue", alpha = 0.4) +
  geom_vline(xintercept = seg_mod_combined$psi[1], color = "red", linetype = "dashed", size = 1)

# ---------------------------
# Compare Linear vs. Spline Interaction Models
# ---------------------------
model_linear <- lm(imwbcnt ~ social_contact_dm * economic_threat_index_dm + lrscale_dm + lrscale_m + education_numeric + household_income_dm +
                     gdppc_dm + pop_density_dm + unemployment_dm + age + gndr +
                     factor(year) + factor(nuts2) + factor(country), data = df)

model_spline <- lm(imwbcnt ~ ns(social_contact_dm, df = 3) * ns(economic_threat_index_dm, df = 3) + lrscale_dm + lrscale_m + education_numeric +
                     household_income_dm + gdppc_dm + pop_density_dm + unemployment_dm + age + gndr +
                     factor(year) + factor(nuts2) + factor(country), data = df)

AIC(model_linear, model_spline)
BIC(model_linear, model_spline)

plot_model(model_spline,
           type = "pred",
           terms = c("social_contact_dm", "economic_threat_index_dm [0.2, 0.5, 0.8]"),
           title = "Predicted outgroup hostility by Social Contact at Different Threat Levels",
           axis.title = c("Social Contact (demeaned)", "Predicted outgroup hostility")) +
  theme_minimal() +
  theme(legend.position = "right",
        axis.text = element_text(size = 11, color = "black"),
        axis.title = element_text(size = 12))
