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

setwd("/Users/aunal/Desktop/Uni/Study/ss 2024/master thesis/data") # Adjust path
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
colnames(df)
# ---------------------------
# 3) Create Economic Threat Index
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
# 4) Decompose Variables (Within-Group Demeaning)
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
  ungroup()

df <- df %>%
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
df <- df %>%
  mutate(
    nuts2 = droplevels(as.factor(nuts2)),
    country = droplevels(as.factor(country))
  )
model_spline <- lm(
  imwbcnt ~ ns(social_contact_dm, df = 3) * ns(economic_threat_index_dm, df = 3) +
    ns(net_mig_dm, df = 3) + ns(gdppc_dm, df = 3) + ns(pop_density_dm, df = 3) + ns(unemployment_dm, df = 3) +
    age + gndr + factor(year) + factor(nuts2),
  data = df
)

# ----------------------------------
# 5) Generate Corrected Marginal Effect Plots
# ----------------------------------

# 5.1: Marginal Effect of Social Contact on Outgroup Hostility (with splines)
pred_social_contact <- ggpredict(model_spline, terms = "social_contact_dm [all]")
plot_social_contact <- ggplot(pred_social_contact, aes(x = x, y = predicted)) +
  geom_line(color = "blue", size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "blue") +
  labs(title = "Marginal Effect of Social Contact on Outgroup Hostility",
       x = "Social Contact",
       y = "Predicted Outgroup Hostility") +
  theme_minimal()

# 5.2: Marginal Effect of Net Migration on Outgroup Hostility (with splines)
pred_net_migration <- ggpredict(model_spline, terms = "net_mig_dm [all]")
plot_net_migration <- ggplot(pred_net_migration, aes(x = x, y = predicted)) +
  geom_line(color = "red", size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "red") +
  labs(title = "Marginal Effect of Net Migration on Outgroup Hostility",
       x = "Net Migration (WE)",
       y = "Predicted Outgroup Hostility") +
  theme_minimal()

# ----------------------------------
# 6) Interaction Effects (Splines Applied)
# ----------------------------------

# 6.1: Interaction: Social Contact x Economic Threat
pred_interaction1 <- ggpredict(model_spline, terms = c("social_contact_dm", "economic_threat_index_dm [0.2, 0.5, 0.8]"))
plot_interaction1 <- ggplot(pred_interaction1, aes(x = x, y = predicted, color = group)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, color = NA) +
  labs(title = "Interaction: Social Contact x Economic Threat",
       x = "Social Contact (WE)",
       y = "Predicted Outgroup Hostility",
       color = "Economic Threat Level") +
  theme_minimal()

# 6.2: Interaction: Net Migration x Social Contact
pred_interaction2 <- ggpredict(model_spline, terms = c("net_mig_dm", "social_contact_dm [low, medium, high]"))
plot_interaction2 <- ggplot(pred_interaction2, aes(x = x, y = predicted, color = group)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, color = NA) +
  labs(title = "Interaction: Net Migration x Social Contact",
       x = "Net Migration (WE)",
       y = "Predicted Outgroup Hostility",
       color = "Social Contact Level") +
  theme_minimal()

# 6.3: Interaction: Net Migration x Economic Threat
pred_interaction3 <- ggpredict(model_spline, terms = c("net_mig_dm", "economic_threat_index_dm [low, medium, high]"))
plot_interaction3 <- ggplot(pred_interaction3, aes(x = x, y = predicted, color = group)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, color = NA) +
  labs(title = "Interaction: Net Migration x Economic Threat",
       x = "Net Migration (WE)",
       y = "Predicted Outgroup Hostility",
       color = "Economic Threat Level") +
  theme_minimal()

# ----------------------------------
# 7) Display Plots Separately
# ----------------------------------
print(plot_social_contact)
print(plot_net_migration)
print(plot_interaction1)
print(plot_interaction2)
print(plot_interaction3)

base_linear_no_interaction <- lm(
  imwbcnt ~ social_contact_dm + economic_threat_index_dm + lrscale_dm + age + gndr,
  data = df,
  na.action = na.exclude
)
seg_mod_combined <- segmented(
  base_linear_no_interaction,
  seg.Z = ~ social_contact_dm,
  psi = 0.3  # or quantile(df$social_contact_dm, 0.5)
)
summary(seg_mod_combined)

# 1) Choose typical values for other variables:
typical_threat <- mean(df$economic_threat_index_dm, na.rm = TRUE)
typical_lr     <- mean(df$lrscale_dm, na.rm = TRUE)
typical_age    <- mean(df$age, na.rm = TRUE)
typical_gndr   <- "1"   # Adjust factor level

# 2) Build a grid of social_contact_dm from min to max:
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

# 3) Predict from segmented model:
pred_seg <- predict(seg_mod_combined, newdata=grid_data, se.fit=TRUE)
grid_data$fit <- pred_seg$fit
grid_data$upr <- pred_seg$fit + 1.96 * pred_seg$se.fit
grid_data$lwr <- pred_seg$fit - 1.96 * pred_seg$se.fit

# Extract the estimated breakpoint from seg_mod_combined:
bp_value <- seg_mod_combined$psi[2]  # Extract breakpoint estimate

# Create Segmented Regression Plot
plot_segmented <- ggplot(grid_data, aes(x = social_contact_dm, y = fit)) +
  geom_line(color = "red", size = 1) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "red", alpha = 0.2) +
  geom_vline(xintercept = bp_value, linetype = "dashed", color = "blue", size = 1) +
  labs(
    title = "Segmented Regression for Social Contact",
    subtitle = paste("Breakpoint at Social Contact ≈", round(bp_value, 3)),
    x = "Social Contact",
    y = "Predicted Outgroup Hostility"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(size = 12),  # Adjust title size
        plot.subtitle = element_text(size = 10))  # Adjust subtitle size

# ----------------------------------
# 3) Marginal Effect of Social Contact (Splined Model)
# ----------------------------------
model_spline <- lm(
  imwbcnt ~ ns(social_contact_dm, df = 3) * ns(economic_threat_index_dm, df = 3) +
    ns(net_mig_dm, df = 3) + ns(gdppc_dm, df = 3) + ns(pop_density_dm, df = 3) + ns(unemployment_dm, df = 3) +
    age + gndr + factor(year) + factor(nuts2),
  data = df
)

# Generate Marginal Effect of Social Contact
pred_social_contact <- ggpredict(model_spline, terms = "social_contact_dm [all]")
plot_social_contact <- ggplot(pred_social_contact, aes(x = x, y = predicted)) +
  geom_line(color = "blue", size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "blue") +
  labs(title = "Marginal Effect of Social Contact on Outgroup Hostility",
       x = "Social Contact",
       y = "Predicted Outgroup Hostility") +
  theme_minimal() +
  theme(plot.title = element_text(size = 12))  # Adjust title size

# ----------------------------------
# 4) Combine Both Plots in a Grid Layout
# ----------------------------------
grid.arrange(plot_segmented, plot_social_contact, nrow = 1)


# ----------------------------------
# 1) Fit Models for Each Contextual Variable Separately
# ----------------------------------

# Model for Net Migration
model_net_mig <- lm(
  imwbcnt ~ ns(net_mig, df = 3) + age + gndr + factor(year) + factor(nuts2),
  data = df
)

# Model for GDP per Capita
model_gdppc <- lm(
  imwbcnt ~ ns(gdppc, df = 3) + age + gndr + factor(year) + factor(nuts2),
  data = df
)

# Model for Unemployment
model_unemployment <- lm(
  imwbcnt ~ ns(unemployment, df = 3) + age + gndr + factor(year) + factor(nuts2),
  data = df
)

# Model for Population Density
model_pop_density <- lm(
  imwbcnt ~ ns(pop_density, df = ) + age + gndr + factor(year) + factor(nuts2),
  data = df
)

# ----------------------------------
# 2) Generate Marginal Effects for Each Contextual Variable
# ----------------------------------

# Function to generate clean black & white plots with small titles
plot_marginal_effect <- function(model, term, title) {
  pred <- ggpredict(model, terms = paste0(term, " [all]"))
  
  ggplot(pred, aes(x = x, y = predicted)) +
    geom_line(color = "black", size = 1.2) +  # Black line
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "black") +  # Black confidence interval
    labs(title = title,
         x = term,
         y = "Predicted Outgroup Hostility") +
    theme_minimal() +
    theme(plot.title = element_text(size = 10))  # Reduce title size
}

plot_net_migration <- plot_marginal_effect(model_net_mig, "net_mig", "Marginal Effect of Net Migration on Outgroup Hostility")
plot_gdppc <- plot_marginal_effect(model_gdppc, "gdppc", "Marginal Effect of GDP per Capita on Outgroup Hostility")
plot_unemployment <- plot_marginal_effect(model_unemployment, "unemployment", "Marginal Effect of Unemployment on Outgroup Hostility")
plot_pop_density <- plot_marginal_effect(model_pop_density, "pop_density", "Marginal Effect of Population Density on Outgroup Hostility")

# ----------------------------------
# 3) Arrange All Plots in One Grid for Comparison
# ----------------------------------
grid.arrange(plot_net_migration, plot_gdppc, plot_unemployment, plot_pop_density, nrow = 2)

# 2.1: Social Contact × Economic Threat
model_social_econ <- lm(
  imwbcnt ~ ns(social_contact_dm, df = 3) * ns(economic_threat_index_dm, df = 3) +
    age + gndr + factor(year) + factor(nuts2),
  data = df
)
model_social_ideo <- lm(
  imwbcnt ~ ns(social_contact_dm, df = 3) * ns(lrscale_dm, df = 3) +
    age + gndr + factor(year) + factor(nuts2),
  data = df
)





plot_interaction_effect<- function(model, term1, term2, title) {
  pred <- ggpredict(model, terms = c(term1, paste0(term2, " [low, medium, high]")))
  
  ggplot(pred, aes(x = x, y = predicted, color = group, fill = group)) +
    geom_line(size = 1.2) +  
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15) +  
    scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A")) +  
    scale_fill_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A")) +  
    labs(title = title,
         x = term1,
         y = "Predicted Outgroup Hostility",
         color = "Group",
         fill = "Group") +
    theme_minimal() +
    theme(plot.title = element_text(size = 12),
          legend.position = "right")  }



x_limits <- c(-1, 0.5)  
y_limits <- c(min(pred_minority$conf.low, na.rm = TRUE) - 0.1, max(pred_minority$conf.high, na.rm = TRUE) + 0.1)

model_social_econ <- lm(
  imwbcnt ~ ns(social_contact_dm, df = 3) * ns(economic_threat_index_dm, df = 3) +
    age + gndr + factor(year) + factor(nuts2),
  data = df
)
model_social_ideo <- lm(
  imwbcnt ~ ns(social_contact_dm,df=3) * ns(lrscale_dm,df=3) +
    age + gndr + factor(year) + factor(nuts2),
  data = df
)
model_social_minority <- lm(
  imwbcnt ~ ns(social_contact_dm, df = 3) * ns(minority_presence_dm, df = 3) +
    age + gndr + factor(year) + factor(nuts2),
  data = df
)
model_social_pop_den <- lm(
  imwbcnt ~ ns(social_contact_dm, df = 3) * ns(pop_density_c_dm, df = 3) +
    age + gndr + factor(year) + factor(nuts2),
  data = df
)
# Function to plot interaction effects
plot_interaction_effect_01 <- function(model, term1, term2, title, xlabel) {
  pred <- ggpredict(model, terms = c(term1, paste0(term2, " [low, medium, high]")))

  ggplot(pred, aes(x = x, y = predicted, color = group, fill = group)) +
    geom_line(size = 1.2) +  # Assign different colors to each group
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1) +  # Light confidence interval
    scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A")) +  # Red, Blue, Green
    scale_fill_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A")) +  # Match fill colors
    labs(title = title,
         x = xlabel,  # Custom X-axis label to remove "_dm"
         y = "Predicted Outgroup Hostility",
         color = "Group",
         fill = "Group") +
    theme_minimal() +
    theme(plot.title = element_text(size = 12),
          legend.position = "right")  # Keep legend for clarity
}
# Function to plot interaction effects with fixed y-axis limits
plot_interaction_effect <- function(model, term1, term2, title, xlabel) {
  pred <- ggpredict(model, terms = c(term1, paste0(term2, " [low, medium, high]")))
  
  ggplot(pred, aes(x = x, y = predicted, color = group, fill = group)) +
    geom_line(size = 1.2) +  
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1) +  
    scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A")) +  
    scale_fill_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A")) +  
    labs(title = title,
         x = xlabel,  
         y = "Predicted Outgroup Hostility",
         color = "Group",
         fill = "Group") +
    theme_minimal() +
    theme(plot.title = element_text(size = 12),
          legend.position = "right") +  
    xlim(x_limits) +  # Set common x-axis limits
    ylim(y_limits) 
}
# Generate interaction plots with custom x-axis labels
plot_social_econ <- plot_interaction_effect_01(
  model_social_econ, "social_contact_dm", "economic_threat_index_dm",
  "Social Contact × Perceived Economic Threat", "Social Contact"
)

plot_social_ideo <- plot_interaction_effect(
  model_social_ideo, "social_contact_dm", "lrscale_dm",
  "Social Contact × Political Ideology", "Social Contact"
)
plot_social_minority <- plot_interaction_effect(
  model_social_minority, "social_contact_dm", "minority_presence_dm",
  "Social Contact × Minority Presence", "Social Contact"
)
plot_social_popden <- plot_interaction_effect(
  model_social_pop_den, "social_contact_dm", "pop_density_c_dm",
  "Social Contact × Regional Population Density", "Social Contact"
)
grid.arrange(plot_social_econ, plot_social_ideo, plot_social_minority,plot_social_popden, ncol = 2)

pred_minority <- ggpredict(model_social_minority, terms = c("social_contact_dm", "minority_presence_dm [low, medium, high]"))
print(pred_minority)

plot_interaction_effect <- function(model, term1, term2, title, xlabel) {
  pred <- ggpredict(model, terms = c(term1, paste0(term2, " [low, medium, high]")))
  
  ggplot(pred, aes(x = x, y = predicted, color = group, fill = group)) +
    geom_line(size = 1.2) +  
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15) +  
    scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A")) +  
    scale_fill_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A")) +  
    labs(title = title,
         x = xlabel,  
         y = "Predicted Outgroup Hostility",
         color = "Migration Level",
         fill = "Migration Level") +
    theme_minimal() +
    theme(plot.title = element_text(size = 12),
          legend.position = "right") # +  
    # xlim(x_limits) +  # Set consistent x-axis limits
    # ylim(y_limits)    # Set consistent y-axis limits
}
# Fit models for interaction effects
model_social_reg_mig <- lm(
  imwbcnt ~ ns(social_contact_dm, df = 3) * ns(net_mig_dm, df = 3) +
    age + gndr + factor(year), #+ factor(nuts2),
  data = df
)

model_social_nat_mig <- lm(
  imwbcnt ~ ns(social_contact_dm, df = 3) * ns(net_mig_c_dm, df = 3) +
    age + gndr + factor(year),# + factor(nuts2),
  data = df
)

plot_social_reg_mig <- plot_interaction_effect(
  model_social_reg_mig, "social_contact_dm", "net_mig_dm",
  "Social Contact × Regional Crude Net Migration Rate", "Social Contact"
)

plot_social_nat_mig <- plot_interaction_effect(
  model_social_nat_mig, "social_contact_dm", "net_mig_c_dm",
  "Social Contact × National Crude Net Migration Rate", "Social Contact"
)

grid.arrange(plot_social_reg_mig, plot_social_nat_mig, ncol = 2)


model_social_pop_den <- lm(
  imwbcnt ~ ns(social_contact_dm, df = 3) * ns(pop_density_dm, df = 3) +
    age + gndr + factor(year),# + factor(nuts2),
  data = df
)
model_social_pop_den_c <- lm(
  imwbcnt ~ ns(social_contact_dm, df = 3) * ns(pop_density_c_dm, df = 3) +
    age + gndr + factor(year),# + factor(nuts2),
  data = df
)
plot_social_pop_den <- plot_interaction_effect(
  model_social_pop_den, "social_contact_dm", "pop_density_dm",
  "Social Contact × Regional Population density", "Social Contact"
)
plot_social_pop_den_c <- plot_interaction_effect(
  model_social_pop_den_c, "social_contact_dm", "pop_density_c_dm",
  "Social Contact × National population density", "Social Contact"
)

grid.arrange(plot_social_pop_den, plot_social_pop_den_c, ncol = 2)




model_df2 <- lm(imwbcnt ~ ns(social_contact_dm, df = 2) * ns(economic_threat_index_dm, df = 2) +
                  age + gndr + factor(year) + factor(nuts2), data = df)

model_df3 <- lm(imwbcnt ~ ns(social_contact_dm, df = 3) * ns(economic_threat_index_dm, df = 3) +
                  age + gndr + factor(year) + factor(nuts2), data = df)

model_df4 <- lm(imwbcnt ~ ns(social_contact_dm, df = 4) * ns(economic_threat_index_dm, df = 4) +
                  age + gndr + factor(year) + factor(nuts2), data = df)

# Compare AIC and BIC values
AIC(model_df2, model_df3, model_df4)
BIC(model_df2, model_df3, model_df4)


model_df02 <- lm(imwbcnt ~ ns(social_contact_dm, df = 2) * ns(lrscale_dm, df = 2) +
                  age + gndr + factor(year) + factor(nuts2), data = df)

model_df03 <- lm(imwbcnt ~ ns(social_contact_dm, df = 3) * ns(lrscale_dm, df = 3) +
                  age + gndr + factor(year) + factor(nuts2), data = df)

model_df04 <- lm(imwbcnt ~ ns(social_contact_dm, df = 4) * ns(lrscale_dm, df = 4) +
                  age + gndr + factor(year) + factor(nuts2), data = df)

# Compare AIC and BIC values
AIC(model_df02, model_df03, model_df04)
BIC(model_df02, model_df03, model_df04)

model_df002 <- lm(imwbcnt ~ ns(social_contact_dm, df = 2) * ns(minority_presence_dm, df = 2) +
                   age + gndr + factor(year) + factor(nuts2), data = df)

model_df003 <- lm(imwbcnt ~ ns(social_contact_dm, df = 3) * ns(minority_presence_dm, df = 3) +
                   age + gndr + factor(year) + factor(nuts2), data = df)

model_df004 <- lm(imwbcnt ~ ns(social_contact_dm, df = 4) * ns(minority_presence_dm, df = 4) +
                   age + gndr + factor(year) + factor(nuts2), data = df)

# Compare AIC and BIC values
AIC(model_df002, model_df003, model_df004)
BIC(model_df002, model_df003, model_df004)


# Fit the fully linear interaction model (no splines)
model_linear_ideo <- lm(imwbcnt ~ social_contact_dm * lrscale_dm + 
                          age + gndr + factor(year) + factor(nuts2), data = df)

model_linear_minority <- lm(imwbcnt ~ social_contact_dm * minority_presence_dm + 
                              age + gndr + factor(year) + factor(nuts2), data = df)

# Compare models for Political Ideology
AIC(model_linear_ideo, model_df002)  # Linear vs. Spline (df = 2)
BIC(model_linear_ideo, model_df002)

# Compare models for Minority Presence
AIC(model_linear_minority, model_df002)  # Linear vs. Spline (df = 2)
BIC(model_linear_minority, model_df002)




colnames(df)

df_values <- c(2, 3, 4)
plot_list <- list()

for (df_value in df_values) {
  model <- lm(imwbcnt ~ ns(social_contact_dm, df = df_value) * ns(minority_presence_dm, df = df_value) +
                age + gndr + factor(year) + factor(nuts2), data = df)
  
  pred <- ggpredict(model, terms = "social_contact_dm")
  
  p <- ggplot(pred, aes(x = x, y = predicted)) +
    geom_line(color = "blue", size = 1.2) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, fill = "blue") +
    labs(title = paste("df =", df_value), x = "Social Contact", y = "Predicted Outgroup Hostility") +
    theme_minimal()
  
  plot_list[[df_value]] <- p
}

grid.arrange(plot_list[[2]], plot_list[[3]], plot_list[[4]], ncol = 3)






df_values <- c(2, 3, 4)
plot_list <- list()

for (df_value in df_values) {
  model <- lm(imwbcnt ~ ns(social_contact_dm, df = df_value) * ns(lrscale_dm, df = df_value) +
                age + gndr + factor(year) + factor(nuts2), data = df)
  
  pred <- ggpredict(model, terms = "social_contact_dm")
  
  p <- ggplot(pred, aes(x = x, y = predicted)) +
    geom_line(color = "blue", size = 1.2) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, fill = "blue") +
    labs(title = paste("df =", df_value), x = "Social Contact", y = "Predicted Outgroup Hostility") +
    theme_minimal()
  
  plot_list[[df_value]] <- p
}

grid.arrange(plot_list[[2]], plot_list[[3]], plot_list[[4]], ncol = 3)


library(boot)


library(ggplot2)

df_values <- c(2, 3, 4, 5)
plot_list <- list()

for (df_value in df_values) {
  model <- lm(imwbcnt ~ ns(social_contact_dm, df = df_value), data = df)
  pred <- ggpredict(model, terms = "social_contact_dm")
  
  p <- ggplot(pred, aes(x = x, y = predicted)) +
    geom_line(color = "blue", size = 1.2) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, fill = "blue") +
    labs(title = paste("df =", df_value), x = "Social Contact", y = "Predicted Outgroup Hostility") +
    theme_minimal()
  
  plot_list[[df_value]] <- p
}

library(gridExtra)
grid.arrange(plot_list[[2]], plot_list[[3]], plot_list[[4]], plot_list[[5]], ncol = 2)






# model_spline <- lm(
#   imwbcnt ~ ns(social_contact_dm, df = 3) * ns(economic_threat_index_dm, df = 3) +
#     ns(net_mig_dm, df = 3) + age + gndr + factor(year) + factor(nuts2),
#   data = df
# )
# # 5.1: Marginal Effect of Social Contact on Outgroup Hostility (with splines)
# pred_social_contact <- ggpredict(model_spline, terms = "social_contact_dm [all]")
# plot_social_contact <- ggplot(pred_social_contact, aes(x = x, y = predicted)) +
#   geom_line(color = "blue", size = 1.2) +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "blue") +
#   labs(title = "Marginal Effect of Social Contact on Outgroup Hostility",
#        x = "Social Contact (demeaned)",
#        y = "Predicted Outgroup Hostility") +
#   theme_minimal()
# 
# # 5.2: Marginal Effect of Net Migration on Outgroup Hostility (with splines)
# pred_net_migration <- ggpredict(model_spline, terms = "net_mig_dm [all]")
# plot_net_migration <- ggplot(pred_net_migration, aes(x = x, y = predicted)) +
#   geom_line(color = "red", size = 1.2) +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "red") +
#   labs(title = "Marginal Effect of Net Migration on Outgroup Hostility",
#        x = "Net Migration (demeaned)",
#        y = "Predicted Outgroup Hostility") +
#   theme_minimal()
# 
# # ----------------------------------
# # 6) Interaction Effects (Splines Applied)
# # ----------------------------------
# 
# # 6.1: Interaction: Social Contact x Economic Threat
# pred_interaction1 <- ggpredict(model_spline, terms = c("social_contact_dm", "economic_threat_index_dm [0.2, 0.5, 0.8]"))
# plot_interaction1 <- ggplot(pred_interaction1, aes(x = x, y = predicted, color = group)) +
#   geom_line(size = 1.2) +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, color = NA) +
#   labs(title = "Interaction: Social Contact x Economic Threat",
#        x = "Social Contact (demeaned)",
#        y = "Predicted Outgroup Hostility",
#        color = "Economic Threat Level") +
#   theme_minimal()
# 
# # 6.2: Interaction: Net Migration x Social Contact
# pred_interaction2 <- ggpredict(model_spline, terms = c("net_mig_dm", "social_contact_dm [low, medium, high]"))
# plot_interaction2 <- ggplot(pred_interaction2, aes(x = x, y = predicted, color = group)) +
#   geom_line(size = 1.2) +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, color = NA) +
#   labs(title = "Interaction: Net Migration x Social Contact",
#        x = "Net Migration (demeaned)",
#        y = "Predicted Outgroup Hostility",
#        color = "Social Contact Level") +
#   theme_minimal()
# 
# # ----------------------------------
# # 7) Display Plots Separately
# # ----------------------------------
# print(plot_social_contact)
# print(plot_net_migration)
# print(plot_interaction1)
# print(plot_interaction2)
# 







# # ---------------------------
# # 5) Fit Linear Model for Marginal Effects
# # ---------------------------
# model_fe <- lm(
#   imwbcnt ~ ns(social_contact_dm, df = 3) * ns(economic_threat_index_dm, df = 3) + 
#     lrscale_dm + minority_presence_dm + gdppc_dm + pop_density_dm + 
#     unemployment_dm + net_mig_dm + age + gndr + factor(year) + factor(nuts2),
#   data = df
# )
# 
# # ----------------------------------
# # 6) Generate Marginal Effect Predictions
# # ----------------------------------
# pred_social_contact <- ggpredict(model_fe, terms = c("social_contact_dm"))
# pred_net_migration <- ggpredict(model_fe, terms = c("net_mig_dm"))
# pred_interaction <- ggpredict(model_fe, terms = c("social_contact_dm", "economic_threat_index_dm [0.2, 0.5, 0.8]"))
# 
# # ---------------------------
# # 7) Plot Marginal Effects
# # ---------------------------
# plot_social_contact <- ggplot(pred_social_contact, aes(x = x, y = predicted)) +
#   geom_line(color = "blue", size = 1.2) +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "blue") +
#   labs(title = "Marginal Effect of Social Contact on Outgroup Hostility",
#        x = "Social Contact (demeaned)",
#        y = "Predicted Outgroup Hostility") +
#   theme_minimal()
# 
# plot_net_migration <- ggplot(pred_net_migration, aes(x = x, y = predicted)) +
#   geom_line(color = "red", size = 1.2) +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "red") +
#   labs(title = "Marginal Effect of Net Migration on Outgroup Hostility",
#        x = "Net Migration (demeaned)",
#        y = "Predicted Outgroup Hostility") +
#   theme_minimal()
# 
# plot_interaction <- ggplot(pred_interaction, aes(x = x, y = predicted, color = group)) +
#   geom_line(size = 1.2) +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, color = NA) +
#   labs(title = "Interaction: Social Contact x Economic Threat",
#        x = "Social Contact (demeaned)",
#        y = "Predicted Outgroup Hostility",
#        color = "Economic Threat Level") +
#   theme_minimal()
# 
# # ---------------------------
# # 8) Arrange Plots Together
# # ---------------------------
# grid.arrange(plot_social_contact, plot_net_migration, plot_interaction, nrow = 2)
