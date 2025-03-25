# ---------------------------
# Load Required Packages
# ---------------------------
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
library(mfp)

# ---------------------------
# Load & Prepare Data
# ---------------------------
# set your own working directory and load the data
df <- read_csv("your file.csv") %>% clean_names()
#setwd("/Users/aunal/Desktop/Uni/Study/ss 2024/master thesis/data")
#df <- read_csv("ESS_EUROSTAT_final_standardized.csv") %>%
#  janitor::clean_names() %>%
#  filter(year != 9999, gndr != 9) %>%
#  mutate(
#    year = as.factor(year),
#    gndr = as.factor(gndr),
#    immig_background = as.factor(immig_background),
#    education_numeric = as.factor(education_numeric),
#    have_child = as.factor(have_child)
#  )

# ---------------------------
# Create Economic Threat Index
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
    net_mig_dm = net_mig - mean(net_mig, na.rm = TRUE),
    net_mig_m = mean(net_mig, na.rm = TRUE)
  ) %>%
  ungroup()

df <- df %>%
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

model_spline <- lm(
  imwbcnt ~ ns(social_contact_dm, df = 2) * ns(economic_threat_index_dm, df = 3) +
    ns(net_mig_dm, df = 3) + gdppc_dm + pop_density_dm + unemployment_dm + minority_presence_dm +
    lrscale_dm + education_numeric + household_income_dm + unemployment_c_dm + gdppc_c_dm +
    age + gndr + factor(year) + factor(nuts2) + factor(country),
  data = df, na.action = na.exclude
)

# ----------------------------------
# Generate Marginal Effect Plots
# ----------------------------------

# Marginal Effect of Social Contact on Outgroup Hostility (with splines)
pred_social_contact <- ggpredict(model_spline, terms = "social_contact_dm [all]")
plot_social_contact <- ggplot(pred_social_contact, aes(x = x, y = predicted)) +
  geom_line(color = "blue", size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "blue") +
  labs(title = "Marginal Effect of Social Contact on Outgroup Hostility",
       x = "Social Contact",
       y = "Predicted Outgroup Hostility") +
  theme_minimal()

# Marginal Effect of Net Migration on Outgroup Hostility (with splines)
#pred_net_migration <- ggpredict(model_spline, terms = "net_mig_dm [all]")
#plot_net_migration <- ggplot(pred_net_migration, aes(x = x, y = predicted)) +
#  geom_line(color = "red", size = 1.2) +
#  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "red") +
#  labs(title = "Marginal Effect of Net Migration on Outgroup Hostility",
#       x = "Net Migration (WE)",
#       y = "Predicted Outgroup Hostility") +
#  theme_minimal()

# ----------------------------------
# Interaction Effects (Splines Applied)
# ----------------------------------
# Interaction: Social Contact x Economic Threat
#pred_interaction1 <- ggpredict(model_spline, terms = c("social_contact_dm", "economic_threat_index_dm [0.2, 0.5, 0.8]"))
#plot_interaction1 <- ggplot(pred_interaction1, aes(x = x, y = predicted, color = group)) +
#  geom_line(size = 1.2) +
#  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, color = NA) +
#  labs(title = "Interaction: Social Contact x Economic Threat",
#       x = "Social Contact",
#       y = "Predicted Outgroup Hostility",
#       color = "Economic Threat Level") +
#  theme_minimal()

# Interaction: Social Contact x Political Ideology
#pred_interaction2 <- ggpredict(model_spline, terms = c("social_contact_dm", "lrscale_dm [0.2, 0.5, 0.8]"))
#plot_interaction2 <- ggplot(pred_interaction2, aes(x = x, y = predicted, color = group)) +
#  geom_line(size = 1.2) +
#  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, color = NA) +
#  labs(title = "Interaction: Net Migration x Social Contact",
#       x = "Social Contact",
#       y = "Predicted Outgroup Hostility",
#       color = "Political Ideology Level") +
#  theme_minimal()

# Interaction: Social Contact x Minority Presence
#pred_interaction3 <- ggpredict(model_spline, terms = c("social_contact_dm", "minority_presence_dm [0.2, 0.5, 0.8]"))
#plot_interaction3 <- ggplot(pred_interaction3, aes(x = x, y = predicted, color = group)) +
#  geom_line(size = 1.2) +
#  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, color = NA) +
#  labs(title = "Interaction: Net Migration x Economic Threat",
#       x = "Social Contact",
#       y = "Predicted Outgroup Hostility",
#       color = "Minority Presence Level") +
#  theme_minimal()

#print(plot_social_contact)
#print(plot_net_migration)
#print(plot_interaction1)
#print(plot_interaction2)
#print(plot_interaction3)


seg_mod_combined <- segmented(
  model_spline,
  seg.Z = ~ social_contact_dm,
  psi = 0.3  # or quantile(df$social_contact_dm, 0.5)
)
summary(seg_mod_combined)


typical_threat <- mean(df$economic_threat_index_dm, na.rm = TRUE)
typical_lr     <- mean(df$lrscale_dm, na.rm = TRUE)
typical_age    <- mean(df$age, na.rm = TRUE)
typical_gndr   <- levels(df$gndr)[1]   

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

bp_value <- seg_mod_combined$psi[2]  

# Segmented Regression Plot
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
  theme(plot.title = element_text(size = 12), 
        plot.subtitle = element_text(size = 10)) 

# ----------------------------------
# Marginal Effect of Social Contact (Splined Model)
# ----------------------------------
pred_social_contact <- ggpredict(model_spline, terms = "social_contact_dm [all]")
plot_social_contact <- ggplot(pred_social_contact, aes(x = x, y = predicted)) +
  geom_line(color = "blue", size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "blue") +
  labs(title = "Marginal Effect of Social Contact on Outgroup Hostility",
       x = "Social Contact",
       y = "Predicted Outgroup Hostility") +
  theme_minimal() +
  theme(plot.title = element_text(size = 12))  

grid.arrange(plot_segmented, plot_social_contact, nrow = 1)


# Social Contact × Economic Threat
model_social_econ <- lm(
  imwbcnt ~ ns(social_contact_dm, df = 2) * ns(economic_threat_index_dm, df = 3) +
    age + gndr +
    minority_presence_dm + minority_presence_m +
    economic_threat_index_m + social_contact_m +
    lrscale_dm + lrscale_m +
    household_income_dm + household_income_m +
    unemployment_dm + unemployment_m +
    unemployment_c_dm + unemployment_c_m +
    gdppc_dm + gdppc_m + education_numeric + 
    gdppc_c_dm + gdppc_c_m + factor(year) + factor(nuts2) + factor(country),
  data = df
)

# Social Contact x Political Ideology
model_social_ideo <- lm(
  imwbcnt ~ ns(social_contact_dm, df = 2) * ns(lrscale_dm, df = 2) +
    age + gndr +
    minority_presence_dm + minority_presence_m +
    economic_threat_index_m + social_contact_m +
    economic_threat_index_dm + lrscale_m +
    household_income_dm + household_income_m +
    unemployment_dm + unemployment_m +
    unemployment_c_dm + unemployment_c_m +
    gdppc_dm + gdppc_m + education_numeric +
    gdppc_c_dm + gdppc_c_m +
    factor(year) + factor(nuts2) + factor(country),
  data = df
)

model_social_minority <- lm(
  imwbcnt ~ ns(social_contact_dm, df = 2) * ns(minority_presence_dm, df = 2) +
    age + gndr +
    lrscale_dm + minority_presence_m +
    economic_threat_index_m + social_contact_m +
    economic_threat_index_dm + lrscale_m +
    household_income_dm + household_income_m +
    unemployment_dm + unemployment_m +
    unemployment_c_dm + unemployment_c_m +
    gdppc_dm + gdppc_m + education_numeric + 
    gdppc_c_dm + gdppc_c_m +
    factor(year) + factor(nuts2) + factor(country),
  data = df
)



#plot_interaction_effect<- function(model, term1, term2, title) {
#  pred <- ggpredict(model, terms = c(term1, paste0(term2, " [low, medium, high]")))  
#  ggplot(pred, aes(x = x, y = predicted, color = group, fill = group)) +
#    geom_line(size = 1.2) +  
#    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15) +  
#    scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A")) +  
#    scale_fill_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A")) +  
#    labs(title = title,
#         x = term1,
#         y = "Predicted Outgroup Hostility",
#         color = "Group",
#         fill = "Group") +
#    theme_minimal() +
#    theme(plot.title = element_text(size = 12),
#          legend.position = "right")  }



x_limits <- c(-1, 0.5)
plot_interaction_effect <- function(model, term1, term2, title = "", xlabel = "X", ylabel = "Y") {
  pred <- ggpredict(model, terms = c(term1, paste0(term2, " [1, 2, 3]")))
  ggplot(pred, aes(x = x, y = predicted, color = group, fill = group)) +
    geom_line(size = 1.2) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15) +
    scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A")) +  
    scale_fill_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A")) +  
    labs(
      title = title,
      x = xlabel,
      y = "Predicted Outgroup Hostility",
      color = "Group",
      fill = "Group"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(size = 12),
          legend.position = "right") +  
    xlim(x_limits)
}

#plot_interaction_effect <- function(model, term1, term2, title, xlabel) {
#  pred <- ggpredict(model, terms = c(term1, paste0(term2, " [low, medium, high]")))  
#  ggplot(pred, aes(x = x, y = predicted, color = group, fill = group)) +
#    geom_line(size = 1.2) +  
#    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1) +  
#    scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A")) +  
#    scale_fill_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A")) +  
#    labs(title = title,
#         x = xlabel,  
#         y = "Predicted Outgroup Hostility",
#         color = "Group",
#         fill = "Group") +
#    theme_minimal() +
#    theme(plot.title = element_text(size = 12),
#          legend.position = "right") +  
#    xlim(x_limits) 
#}

plot_social_econ <- plot_interaction_effect(
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

grid.arrange(plot_social_econ, plot_social_ideo, plot_social_minority, ncol = 3)

#pred_minority <- ggpredict(model_social_minority, terms = c("social_contact_dm", "minority_presence_dm [low, medium, high]"))
#print(pred_minority)

#plot_interaction_effect <- function(model, term1, term2, title, xlabel) {
#  pred <- ggpredict(model, terms = c(term1, paste0(term2, " [1, 2, 3]")))  
#  ggplot(pred, aes(x = x, y = predicted, color = group, fill = group)) +
#    geom_line(size = 1.2) +  
#    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15) +  
#    scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A")) +  
#    scale_fill_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A")) +  
#    labs(title = title,
#         x = xlabel,  
#         y = "Predicted Outgroup Hostility",
#         color = "Migration Level",
#         fill = "Migration Level") +
#    theme_minimal() +
#    theme(plot.title = element_text(size = 12),
#          legend.position = "right") 
#}

# Fit models for interaction effects
#model_social_reg_mig <- lm(
#  imwbcnt ~ ns(social_contact_dm, df = 2) * ns(net_mig_dm, df = 3) +
#  age + gndr +
#    minority_presence_dm + minority_presence_m +
#    economic_threat_index_m + social_contact_m +
#    economic_threat_index_dm + lrscale_m +
#    household_income_dm + household_income_m +
#    unemployment_dm + unemployment_m +
#    unemployment_c_dm + unemployment_c_m +
#    gdppc_dm + gdppc_m + education_numeric + 
#    gdppc_c_dm + gdppc_c_m +
#  factor(year), #+ factor(nuts2) + factor(country),
#  data = df
#)

#model_social_nat_mig <- lm(
#  imwbcnt ~ ns(social_contact_dm, df = 2) * ns(net_mig_c_dm, df = 3) +
#  age + gndr +
#    minority_presence_dm + minority_presence_m +
#    economic_threat_index_m + social_contact_m +
#    economic_threat_index_dm + lrscale_m +
#    household_income_dm + household_income_m +
#    unemployment_dm + unemployment_m +
#    unemployment_c_dm + unemployment_c_m +
#    gdppc_dm + gdppc_m + education_numeric + 
#    gdppc_c_dm + gdppc_c_m +
#  factor(year),# + factor(nuts2) + factor(country),
#  data = df
#)

#plot_social_reg_mig <- plot_interaction_effect(
#  model_social_reg_mig, "social_contact_dm", "net_mig_dm",
#  "Social Contact × Regional Crude Net Migration Rate", "Social Contact"
#)

#plot_social_nat_mig <- plot_interaction_effect(
#  model_social_nat_mig, "social_contact_dm", "net_mig_c_dm",
#  "Social Contact × National Crude Net Migration Rate", "Social Contact"
#)
#grid.arrange(plot_social_reg_mig, plot_social_nat_mig, ncol = 1)

fit_dd_spline_model <- function(df, response, spline1, spline2, df1 = 2, df2 = 3) {
  spline_terms <- function(df, var, df_spline) {
    spline_mat <- ns(df[[var]], df = df_spline)
    colnames(spline_mat) <- paste0(var, "_s", seq_len(ncol(spline_mat)))
    bind_cols(df, as.data.frame(spline_mat))
  }

  double_demeaned_interactions <- function(df, var1_prefix, var2_prefix) {
    var1_terms <- grep(paste0("^", var1_prefix, "_s"), names(df), value = TRUE)
    var2_terms <- grep(paste0("^", var2_prefix, "_s"), names(df), value = TRUE)

    for (v1 in var1_terms) {
      for (v2 in var2_terms) {
        interaction_name <- paste0("int_", v1, "_", v2)
        interaction_raw <- df[[v1]] * df[[v2]]
        df[[interaction_name]] <- interaction_raw - mean(interaction_raw, na.rm = TRUE)
      }
    }
    return(df)
  }

  df <- spline_terms(df, spline1, df1)
  df <- spline_terms(df, spline2, df2)
  df <- double_demeaned_interactions(df, spline1, spline2)
  spline1_terms <- grep(paste0("^", spline1, "_s"), names(df), value = TRUE)
  spline2_terms <- grep(paste0("^", spline2, "_s"), names(df), value = TRUE)
  interaction_terms <- grep("^int_", names(df), value = TRUE)

  control_vars <- c(
    "age", "gndr", "education_numeric",
    "minority_presence_dm", "minority_presence_m",
    "economic_threat_index_m", "social_contact_m",
    "lrscale_dm", "lrscale_m", "education_numeric",
    "household_income_dm", "household_income_m",
    "unemployment_dm", "unemployment_m",
    "unemployment_c_dm", "unemployment_c_m",
    "gdppc_dm", "gdppc_m",
    "gdppc_c_dm", "gdppc_c_m",
    "factor(year)", "factor(nuts2)", "factor(country)"
  )

  formula_str <- paste(
    response, "~",
    paste(c(spline1_terms, spline2_terms, interaction_terms, control_vars), collapse = " + ")
  )

  model <- lm(as.formula(formula_str), data = df)
  return(list(model = model, df = df))
}

plot_dd_interaction <- function(model, term1, term2, title = "", xlabel = "X", ylabel = "Y") {
  pred <- ggpredict(model, terms = c(term1, paste0(term2, " [1, 2, 3]")))
  ggplot(pred, aes(x = x, y = predicted, color = group, fill = group)) +
    geom_line(size = 1.2) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15) +
    scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A")) +
    scale_fill_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A")) +
    labs(
      title = title,
      x = xlabel,
      y = ylabel,
      color = "Group",
      fill = "Group"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12),
      axis.text = element_text(size = 11),
      axis.title = element_text(size = 12),
      legend.position = "right"
    )
}

reg_mig <- fit_dd_spline_model(df, "imwbcnt", "social_contact_dm", "net_mig_dm", df1 = 2, df2 = 3)

plot_reg_mig <- plot_dd_interaction(
  reg_mig$model,
  "social_contact_dm_s1", "net_mig_dm_s1",
  title = "Figure 8: Social Contact × Regional Net Migration",
  xlabel = "Social Contact",
  ylabel = "Predicted Outgroup Hostility"
)
nat_mig <- fit_dd_spline_model( df, response = "imwbcnt", "social_contact_dm", "net_mig_c_dm", df1 = 2, df2 = 3)

plot_nat_mig <- plot_dd_interaction(
  model = nat_mig$model,
  term1 = "social_contact_dm_s1",
  term2 = "net_mig_c_dm_s1",
  title = "Figure 9: Social Contact × National Net Migration",
  xlabel = "Social Contact",
  ylabel = "Predicted Outgroup Hostility"
)
grid.arrange(plot_reg_mig, plot_nat_mig, ncol = 1)


model_df1 <- lm(imwbcnt ~ ns(social_contact_dm, df = 2) * ns(economic_threat_index_dm, df = 1) +
                  age + gndr + education_numeric +
                  minority_presence_dm + minority_presence_m +
                  economic_threat_index_m + social_contact_m +
                  lrscale_dm + lrscale_m +
                  household_income_dm + household_income_m +
                  unemployment_dm + unemployment_m +
                  unemployment_c_dm + unemployment_c_m +
                  gdppc_dm + gdppc_m +
                  gdppc_c_dm + gdppc_c_m + 
                  factor(year) + factor(nuts2) + factor(country), data = df)

model_df2 <- lm(imwbcnt ~ ns(social_contact_dm, df = 2) * ns(economic_threat_index_dm, df = 2) +
                age + gndr + education_numeric +
                minority_presence_dm + minority_presence_m +
                economic_threat_index_m + social_contact_m +
                lrscale_dm + lrscale_m +
                household_income_dm + household_income_m +
                unemployment_dm + unemployment_m +
                unemployment_c_dm + unemployment_c_m +
                gdppc_dm + gdppc_m +
                gdppc_c_dm + gdppc_c_m + # ---------------------------
# Load Required Packages
# ---------------------------
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
library(mfp)

# ---------------------------
# Load & Prepare Data
# ---------------------------
# set your own working directory and load the data
df <- read_csv("your file".csv) %>% clean_names()
#setwd("/Users/aunal/Desktop/Uni/Study/ss 2024/master thesis/data")
#df <- read_csv("ESS_EUROSTAT_final_standardized.csv") %>%
#  janitor::clean_names() %>%
#  filter(year != 9999, gndr != 9) %>%
#  mutate(
#    year = as.factor(year),
#    gndr = as.factor(gndr),
#    immig_background = as.factor(immig_background),
#    education_numeric = as.factor(education_numeric),
#    have_child = as.factor(have_child)
#  )

# ---------------------------
# Create Economic Threat Index
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
    net_mig_dm = net_mig - mean(net_mig, na.rm = TRUE),
    net_mig_m = mean(net_mig, na.rm = TRUE)
  ) %>%
  ungroup()

df <- df %>%
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

model_spline <- lm(
  imwbcnt ~ ns(social_contact_dm, df = 2) * ns(economic_threat_index_dm, df = 3) +
    ns(net_mig_dm, df = 3) + gdppc_dm + pop_density_dm + unemployment_dm + minority_presence_dm +
    lrscale_dm + education_numeric + household_income_dm + unemployment_c_dm + gdppc_c_dm +
    age + gndr + factor(year) + factor(nuts2) + factor(country),
  data = df, na.action = na.exclude
)

# ----------------------------------
# Generate Marginal Effect Plots
# ----------------------------------

# Marginal Effect of Social Contact on Outgroup Hostility (with splines)
pred_social_contact <- ggpredict(model_spline, terms = "social_contact_dm [all]")
plot_social_contact <- ggplot(pred_social_contact, aes(x = x, y = predicted)) +
  geom_line(color = "blue", size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "blue") +
  labs(title = "Marginal Effect of Social Contact on Outgroup Hostility",
       x = "Social Contact",
       y = "Predicted Outgroup Hostility") +
  theme_minimal()

# Marginal Effect of Net Migration on Outgroup Hostility (with splines)
#pred_net_migration <- ggpredict(model_spline, terms = "net_mig_dm [all]")
#plot_net_migration <- ggplot(pred_net_migration, aes(x = x, y = predicted)) +
#  geom_line(color = "red", size = 1.2) +
#  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "red") +
#  labs(title = "Marginal Effect of Net Migration on Outgroup Hostility",
#       x = "Net Migration (WE)",
#       y = "Predicted Outgroup Hostility") +
#  theme_minimal()

# ----------------------------------
# Interaction Effects (Splines Applied)
# ----------------------------------
# Interaction: Social Contact x Economic Threat
#pred_interaction1 <- ggpredict(model_spline, terms = c("social_contact_dm", "economic_threat_index_dm [0.2, 0.5, 0.8]"))
#plot_interaction1 <- ggplot(pred_interaction1, aes(x = x, y = predicted, color = group)) +
#  geom_line(size = 1.2) +
#  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, color = NA) +
#  labs(title = "Interaction: Social Contact x Economic Threat",
#       x = "Social Contact",
#       y = "Predicted Outgroup Hostility",
#       color = "Economic Threat Level") +
#  theme_minimal()

# Interaction: Social Contact x Political Ideology
#pred_interaction2 <- ggpredict(model_spline, terms = c("social_contact_dm", "lrscale_dm [0.2, 0.5, 0.8]"))
#plot_interaction2 <- ggplot(pred_interaction2, aes(x = x, y = predicted, color = group)) +
#  geom_line(size = 1.2) +
#  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, color = NA) +
#  labs(title = "Interaction: Net Migration x Social Contact",
#       x = "Social Contact",
#       y = "Predicted Outgroup Hostility",
#       color = "Political Ideology Level") +
#  theme_minimal()

# Interaction: Social Contact x Minority Presence
#pred_interaction3 <- ggpredict(model_spline, terms = c("social_contact_dm", "minority_presence_dm [0.2, 0.5, 0.8]"))
#plot_interaction3 <- ggplot(pred_interaction3, aes(x = x, y = predicted, color = group)) +
#  geom_line(size = 1.2) +
#  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, color = NA) +
#  labs(title = "Interaction: Net Migration x Economic Threat",
#       x = "Social Contact",
#       y = "Predicted Outgroup Hostility",
#       color = "Minority Presence Level") +
#  theme_minimal()

#print(plot_social_contact)
#print(plot_net_migration)
#print(plot_interaction1)
#print(plot_interaction2)
#print(plot_interaction3)


seg_mod_combined <- segmented(
  model_spline,
  seg.Z = ~ social_contact_dm,
  psi = 0.3  # or quantile(df$social_contact_dm, 0.5)
)
summary(seg_mod_combined)


typical_threat <- mean(df$economic_threat_index_dm, na.rm = TRUE)
typical_lr     <- mean(df$lrscale_dm, na.rm = TRUE)
typical_age    <- mean(df$age, na.rm = TRUE)
typical_gndr   <- levels(df$gndr)[1]   

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

bp_value <- seg_mod_combined$psi[2]  

# Segmented Regression Plot
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
  theme(plot.title = element_text(size = 12), 
        plot.subtitle = element_text(size = 10)) 

# ----------------------------------
# Marginal Effect of Social Contact (Splined Model)
# ----------------------------------
pred_social_contact <- ggpredict(model_spline, terms = "social_contact_dm [all]")
plot_social_contact <- ggplot(pred_social_contact, aes(x = x, y = predicted)) +
  geom_line(color = "blue", size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "blue") +
  labs(title = "Marginal Effect of Social Contact on Outgroup Hostility",
       x = "Social Contact",
       y = "Predicted Outgroup Hostility") +
  theme_minimal() +
  theme(plot.title = element_text(size = 12))  

grid.arrange(plot_segmented, plot_social_contact, nrow = 1)


# Social Contact × Economic Threat
model_social_econ <- lm(
  imwbcnt ~ ns(social_contact_dm, df = 2) * ns(economic_threat_index_dm, df = 3) +
    age + gndr +
    minority_presence_dm + minority_presence_m +
    economic_threat_index_m + social_contact_m +
    lrscale_dm + lrscale_m +
    household_income_dm + household_income_m +
    unemployment_dm + unemployment_m +
    unemployment_c_dm + unemployment_c_m +
    gdppc_dm + gdppc_m + education_numeric + 
    gdppc_c_dm + gdppc_c_m + factor(year) + factor(nuts2) + factor(country),
  data = df
)

# Social Contact x Political Ideology
model_social_ideo <- lm(
  imwbcnt ~ ns(social_contact_dm, df = 2) * ns(lrscale_dm, df = 2) +
    age + gndr +
    minority_presence_dm + minority_presence_m +
    economic_threat_index_m + social_contact_m +
    economic_threat_index_dm + lrscale_m +
    household_income_dm + household_income_m +
    unemployment_dm + unemployment_m +
    unemployment_c_dm + unemployment_c_m +
    gdppc_dm + gdppc_m + education_numeric +
    gdppc_c_dm + gdppc_c_m +
    factor(year) + factor(nuts2) + factor(country),
  data = df
)

model_social_minority <- lm(
  imwbcnt ~ ns(social_contact_dm, df = 2) * ns(minority_presence_dm, df = 2) +
    age + gndr +
    lrscale_dm + minority_presence_m +
    economic_threat_index_m + social_contact_m +
    economic_threat_index_dm + lrscale_m +
    household_income_dm + household_income_m +
    unemployment_dm + unemployment_m +
    unemployment_c_dm + unemployment_c_m +
    gdppc_dm + gdppc_m + education_numeric + 
    gdppc_c_dm + gdppc_c_m +
    factor(year) + factor(nuts2) + factor(country),
  data = df
)



#plot_interaction_effect<- function(model, term1, term2, title) {
#  pred <- ggpredict(model, terms = c(term1, paste0(term2, " [low, medium, high]")))  
#  ggplot(pred, aes(x = x, y = predicted, color = group, fill = group)) +
#    geom_line(size = 1.2) +  
#    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15) +  
#    scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A")) +  
#    scale_fill_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A")) +  
#    labs(title = title,
#         x = term1,
#         y = "Predicted Outgroup Hostility",
#         color = "Group",
#         fill = "Group") +
#    theme_minimal() +
#    theme(plot.title = element_text(size = 12),
#          legend.position = "right")  }



x_limits <- c(-1, 0.5)
plot_interaction_effect <- function(model, term1, term2, title = "", xlabel = "X", ylabel = "Y") {
  pred <- ggpredict(model, terms = c(term1, paste0(term2, " [1, 2, 3]")))
  ggplot(pred, aes(x = x, y = predicted, color = group, fill = group)) +
    geom_line(size = 1.2) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15) +
    scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A")) +  
    scale_fill_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A")) +  
    labs(
      title = title,
      x = xlabel,
      y = "Predicted Outgroup Hostility",
      color = "Group",
      fill = "Group"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(size = 12),
          legend.position = "right") +  
    xlim(x_limits)
}

#plot_interaction_effect <- function(model, term1, term2, title, xlabel) {
#  pred <- ggpredict(model, terms = c(term1, paste0(term2, " [low, medium, high]")))  
#  ggplot(pred, aes(x = x, y = predicted, color = group, fill = group)) +
#    geom_line(size = 1.2) +  
#    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1) +  
#    scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A")) +  
#    scale_fill_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A")) +  
#    labs(title = title,
#         x = xlabel,  
#         y = "Predicted Outgroup Hostility",
#         color = "Group",
#         fill = "Group") +
#    theme_minimal() +
#    theme(plot.title = element_text(size = 12),
#          legend.position = "right") +  
#    xlim(x_limits) 
#}

plot_social_econ <- plot_interaction_effect(
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

grid.arrange(plot_social_econ, plot_social_ideo, plot_social_minority, ncol = 3)

#pred_minority <- ggpredict(model_social_minority, terms = c("social_contact_dm", "minority_presence_dm [low, medium, high]"))
#print(pred_minority)

#plot_interaction_effect <- function(model, term1, term2, title, xlabel) {
#  pred <- ggpredict(model, terms = c(term1, paste0(term2, " [1, 2, 3]")))  
#  ggplot(pred, aes(x = x, y = predicted, color = group, fill = group)) +
#    geom_line(size = 1.2) +  
#    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15) +  
#    scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A")) +  
#    scale_fill_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A")) +  
#    labs(title = title,
#         x = xlabel,  
#         y = "Predicted Outgroup Hostility",
#         color = "Migration Level",
#         fill = "Migration Level") +
#    theme_minimal() +
#    theme(plot.title = element_text(size = 12),
#          legend.position = "right") 
#}

# Fit models for interaction effects
#model_social_reg_mig <- lm(
#  imwbcnt ~ ns(social_contact_dm, df = 2) * ns(net_mig_dm, df = 3) +
#  age + gndr +
#    minority_presence_dm + minority_presence_m +
#    economic_threat_index_m + social_contact_m +
#    economic_threat_index_dm + lrscale_m +
#    household_income_dm + household_income_m +
#    unemployment_dm + unemployment_m +
#    unemployment_c_dm + unemployment_c_m +
#    gdppc_dm + gdppc_m + education_numeric + 
#    gdppc_c_dm + gdppc_c_m +
#  factor(year), #+ factor(nuts2) + factor(country),
#  data = df
#)

#model_social_nat_mig <- lm(
#  imwbcnt ~ ns(social_contact_dm, df = 2) * ns(net_mig_c_dm, df = 3) +
#  age + gndr +
#    minority_presence_dm + minority_presence_m +
#    economic_threat_index_m + social_contact_m +
#    economic_threat_index_dm + lrscale_m +
#    household_income_dm + household_income_m +
#    unemployment_dm + unemployment_m +
#    unemployment_c_dm + unemployment_c_m +
#    gdppc_dm + gdppc_m + education_numeric + 
#    gdppc_c_dm + gdppc_c_m +
#  factor(year),# + factor(nuts2) + factor(country),
#  data = df
#)

#plot_social_reg_mig <- plot_interaction_effect(
#  model_social_reg_mig, "social_contact_dm", "net_mig_dm",
#  "Social Contact × Regional Crude Net Migration Rate", "Social Contact"
#)

#plot_social_nat_mig <- plot_interaction_effect(
#  model_social_nat_mig, "social_contact_dm", "net_mig_c_dm",
#  "Social Contact × National Crude Net Migration Rate", "Social Contact"
#)
#grid.arrange(plot_social_reg_mig, plot_social_nat_mig, ncol = 1)

fit_dd_spline_model <- function(df, response, spline1, spline2, df1 = 2, df2 = 3) {
  spline_terms <- function(df, var, df_spline) {
    spline_mat <- ns(df[[var]], df = df_spline)
    colnames(spline_mat) <- paste0(var, "_s", seq_len(ncol(spline_mat)))
    bind_cols(df, as.data.frame(spline_mat))
  }

  double_demeaned_interactions <- function(df, var1_prefix, var2_prefix) {
    var1_terms <- grep(paste0("^", var1_prefix, "_s"), names(df), value = TRUE)
    var2_terms <- grep(paste0("^", var2_prefix, "_s"), names(df), value = TRUE)

    for (v1 in var1_terms) {
      for (v2 in var2_terms) {
        interaction_name <- paste0("int_", v1, "_", v2)
        interaction_raw <- df[[v1]] * df[[v2]]
        df[[interaction_name]] <- interaction_raw - mean(interaction_raw, na.rm = TRUE)
      }
    }
    return(df)
  }

  df <- spline_terms(df, spline1, df1)
  df <- spline_terms(df, spline2, df2)
  df <- double_demeaned_interactions(df, spline1, spline2)
  spline1_terms <- grep(paste0("^", spline1, "_s"), names(df), value = TRUE)
  spline2_terms <- grep(paste0("^", spline2, "_s"), names(df), value = TRUE)
  interaction_terms <- grep("^int_", names(df), value = TRUE)

  control_vars <- c(
    "age", "gndr", "education_numeric",
    "minority_presence_dm", "minority_presence_m",
    "economic_threat_index_m", "social_contact_m",
    "lrscale_dm", "lrscale_m", "education_numeric",
    "household_income_dm", "household_income_m",
    "unemployment_dm", "unemployment_m",
    "unemployment_c_dm", "unemployment_c_m",
    "gdppc_dm", "gdppc_m",
    "gdppc_c_dm", "gdppc_c_m",
    "factor(year)", "factor(nuts2)", "factor(country)"
  )

  formula_str <- paste(
    response, "~",
    paste(c(spline1_terms, spline2_terms, interaction_terms, control_vars), collapse = " + ")
  )

  model <- lm(as.formula(formula_str), data = df)
  return(list(model = model, df = df))
}

plot_dd_interaction <- function(model, term1, term2, title = "", xlabel = "X", ylabel = "Y") {
  pred <- ggpredict(model, terms = c(term1, paste0(term2, " [1, 2, 3]")))
  ggplot(pred, aes(x = x, y = predicted, color = group, fill = group)) +
    geom_line(size = 1.2) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15) +
    scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A")) +
    scale_fill_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A")) +
    labs(
      title = title,
      x = xlabel,
      y = ylabel,
      color = "Group",
      fill = "Group"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12),
      axis.text = element_text(size = 11),
      axis.title = element_text(size = 12),
      legend.position = "right"
    )
}

reg_mig <- fit_dd_spline_model(df, "imwbcnt", "social_contact_dm", "net_mig_dm", df1 = 2, df2 = 3)

plot_reg_mig <- plot_dd_interaction(
  reg_mig$model,
  "social_contact_dm_s1", "net_mig_dm_s1",
  title = "Figure 8: Social Contact × Regional Net Migration",
  xlabel = "Social Contact",
  ylabel = "Predicted Outgroup Hostility"
)
nat_mig <- fit_dd_spline_model( df, response = "imwbcnt", "social_contact_dm", "net_mig_c_dm", df1 = 2, df2 = 3)

plot_nat_mig <- plot_dd_interaction(
  model = nat_mig$model,
  term1 = "social_contact_dm_s1",
  term2 = "net_mig_c_dm_s1",
  title = "Figure 9: Social Contact × National Net Migration",
  xlabel = "Social Contact",
  ylabel = "Predicted Outgroup Hostility"
)
grid.arrange(plot_reg_mig, plot_nat_mig, ncol = 1)


model_df1 <- lm(imwbcnt ~ ns(social_contact_dm, df = 2) * ns(economic_threat_index_dm, df = 1) +
                  age + gndr + education_numeric +
                  minority_presence_dm + minority_presence_m +
                  economic_threat_index_m + social_contact_m +
                  lrscale_dm + lrscale_m +
                  household_income_dm + household_income_m +
                  unemployment_dm + unemployment_m +
                  unemployment_c_dm + unemployment_c_m +
                  gdppc_dm + gdppc_m +
                  gdppc_c_dm + gdppc_c_m + 
                  factor(year) + factor(nuts2) + factor(country), data = df)

model_df2 <- lm(imwbcnt ~ ns(social_contact_dm, df = 2) * ns(economic_threat_index_dm, df = 2) +
                age + gndr + education_numeric +
                minority_presence_dm + minority_presence_m +
                economic_threat_index_m + social_contact_m +
                lrscale_dm + lrscale_m +
                household_income_dm + household_income_m +
                unemployment_dm + unemployment_m +
                unemployment_c_dm + unemployment_c_m +
                gdppc_dm + gdppc_m +
                gdppc_c_dm + gdppc_c_m + 
                factor(year) + factor(nuts2) + factor(country), data = df)

model_df3 <- lm(imwbcnt ~ ns(social_contact_dm, df = 2) * ns(economic_threat_index_dm, df = 3) +
                age + gndr + education_numeric +
                minority_presence_dm + minority_presence_m +
                economic_threat_index_m + social_contact_m +
                lrscale_dm + lrscale_m +
                household_income_dm + household_income_m +
                unemployment_dm + unemployment_m +
                unemployment_c_dm + unemployment_c_m +
                gdppc_dm + gdppc_m +
                gdppc_c_dm + gdppc_c_m + 
                factor(year) + factor(nuts2) + factor(country), data = df)

model_df4 <- lm(imwbcnt ~ ns(social_contact_dm, df = 2) * ns(economic_threat_index_dm, df = 4) +
                age + gndr + education_numeric +
                minority_presence_dm + minority_presence_m +
                economic_threat_index_m + social_contact_m +
                lrscale_dm + lrscale_m +
                household_income_dm + household_income_m +
                unemployment_dm + unemployment_m +
                unemployment_c_dm + unemployment_c_m +
                gdppc_dm + gdppc_m +
                gdppc_c_dm + gdppc_c_m + 
                factor(year) + factor(nuts2) + factor(country), data = df)

# Compare AIC and BIC values
AIC(model_df1, model_df2, model_df3, model_df4)
BIC(model_df1, model_df2, model_df3, model_df4)

model_df01 <- lm(imwbcnt ~ ns(social_contact_dm, df = 2) * ns(lrscale_dm, df = 1) +
                 age + gndr + education_numeric +
                 minority_presence_dm + minority_presence_m +
                 economic_threat_index_m + social_contact_m +
                 economic_threat_index_dm + lrscale_m +
                 household_income_dm + household_income_m +
                 unemployment_dm + unemployment_m +
                 unemployment_c_dm + unemployment_c_m +
                 gdppc_dm + gdppc_m +
                 gdppc_c_dm + gdppc_c_m + 
                 factor(year) + factor(nuts2) + factor(country), data = df)

model_df02 <- lm(imwbcnt ~ ns(social_contact_dm, df = 2) * ns(lrscale_dm, df = 2) +
                 age + gndr + education_numeric +
                 minority_presence_dm + minority_presence_m +
                 economic_threat_index_m + social_contact_m +
                 economic_threat_index_dm + lrscale_m +
                 household_income_dm + household_income_m +
                 unemployment_dm + unemployment_m +
                 unemployment_c_dm + unemployment_c_m +
                 gdppc_dm + gdppc_m +
                 gdppc_c_dm + gdppc_c_m + 
                 factor(year) + factor(nuts2) + factor(country), data = df)

model_df03 <- lm(imwbcnt ~ ns(social_contact_dm, df = 3) * ns(lrscale_dm, df = 3) +
                 age + gndr + education_numeric +
                 minority_presence_dm + minority_presence_m +
                 economic_threat_index_m + social_contact_m +
                 economic_threat_index_dm + lrscale_m +
                 household_income_dm + household_income_m +
                 unemployment_dm + unemployment_m +
                 unemployment_c_dm + unemployment_c_m +
                 gdppc_dm + gdppc_m +
                 gdppc_c_dm + gdppc_c_m + 
                 factor(year) + factor(nuts2) + factor(country), data = df)

model_df04 <- lm(imwbcnt ~ ns(social_contact_dm, df = 4) * ns(lrscale_dm, df = 4) +
                 age + gndr + education_numeric +
                 minority_presence_dm + minority_presence_m +
                 economic_threat_index_m + social_contact_m +
                 economic_threat_index_dm + lrscale_m +
                 household_income_dm + household_income_m +
                 unemployment_dm + unemployment_m +
                 unemployment_c_dm + unemployment_c_m +
                 gdppc_dm + gdppc_m +
                 gdppc_c_dm + gdppc_c_m + 
                 factor(year) + factor(nuts2) + factor(country), data = df)

# Compare AIC and BIC values
AIC(model_df01, model_df02, model_df03, model_df04)
BIC(model_df01, model_df02, model_df03, model_df04)

model_df001 <- lm(imwbcnt ~ ns(social_contact_dm, df = 2) * ns(minority_presence_dm, df = 1) +
                  age + gndr + education_numeric +
                  lrscale_dm + minority_presence_m +
                  economic_threat_index_m + social_contact_m +
                  economic_threat_index_dm + lrscale_m +
                  household_income_dm + household_income_m +
                  unemployment_dm + unemployment_m +
                  unemployment_c_dm + unemployment_c_m +
                  gdppc_dm + gdppc_m +
                  gdppc_c_dm + gdppc_c_m + 
                  factor(year) + factor(nuts2) + factor(country), data = df)

model_df002 <- lm(imwbcnt ~ ns(social_contact_dm, df = 2) * ns(minority_presence_dm, df = 2) +
                  age + gndr + education_numeric +
                  lrscale_dm + minority_presence_m +
                  economic_threat_index_m + social_contact_m +
                  economic_threat_index_dm + lrscale_m +
                  household_income_dm + household_income_m +
                  unemployment_dm + unemployment_m +
                  unemployment_c_dm + unemployment_c_m +
                  gdppc_dm + gdppc_m +
                  gdppc_c_dm + gdppc_c_m + 
                  factor(year) + factor(nuts2) + factor(country), data = df)

model_df003 <- lm(imwbcnt ~ ns(social_contact_dm, df = 3) * ns(minority_presence_dm, df = 3) +
                  age + gndr + education_numeric +
                  lrscale_dm + minority_presence_m +
                  economic_threat_index_m + social_contact_m +
                  economic_threat_index_dm + lrscale_m +
                  household_income_dm + household_income_m +
                  unemployment_dm + unemployment_m +
                  unemployment_c_dm + unemployment_c_m +
                  gdppc_dm + gdppc_m +
                  gdppc_c_dm + gdppc_c_m + 
                  factor(year) + factor(nuts2) + factor(country), data = df)

model_df004 <- lm(imwbcnt ~ ns(social_contact_dm, df = 4) * ns(minority_presence_dm, df = 4) +
                  age + gndr + education_numeric +
                  lrscale_dm + minority_presence_m +
                  economic_threat_index_m + social_contact_m +
                  economic_threat_index_dm + lrscale_m +
                  household_income_dm + household_income_m +
                  unemployment_dm + unemployment_m +
                  unemployment_c_dm + unemployment_c_m +
                  gdppc_dm + gdppc_m +
                  gdppc_c_dm + gdppc_c_m + 
                  factor(year) + factor(nuts2) + factor(country), data = df)

# Compare AIC and BIC values
AIC(model_df001,model_df002, model_df003, model_df004)
BIC(model_df001,model_df002, model_df003, model_df004)


model_linear_ideo <- lm(imwbcnt ~ social_contact_dm * lrscale_dm + 
                        age + gndr + education_numeric +
                        minority_presence_dm + minority_presence_m +
                        economic_threat_index_m + social_contact_m +
                        economic_threat_index_dm + lrscale_m +
                        household_income_dm + household_income_m +
                        unemployment_dm + unemployment_m +
                        unemployment_c_dm + unemployment_c_m +
                        gdppc_dm + gdppc_m +
                        gdppc_c_dm + gdppc_c_m + 
                        factor(year) + factor(nuts2) + factor(country), data = df)

model_linear_minority <- lm(imwbcnt ~ social_contact_dm * minority_presence_dm + 
                            age + gndr + education_numeric +
                            lrscale_dm + minority_presence_m +
                            economic_threat_index_m + social_contact_m +
                            economic_threat_index_dm + lrscale_m +
                            household_income_dm + household_income_m +
                            unemployment_dm + unemployment_m +
                            unemployment_c_dm + unemployment_c_m +
                            gdppc_dm + gdppc_m +
                            gdppc_c_dm + gdppc_c_m + 
                            factor(year) + factor(nuts2) + factor(country), data = df)
                             
AIC(model_linear_ideo, model_df002)  
BIC(model_linear_ideo, model_df002)

AIC(model_linear_minority, model_df002)  
BIC(model_linear_minority, model_df002)


df_values <- c(1, 2, 3, 4)
plot_list <- list()

for (df_value in df_values) {
  model <- lm(imwbcnt ~ ns(social_contact_dm, df = df_value) * ns(minority_presence_dm, df = df_value) +
              age + gndr + education_numeric +
              lrscale_dm + minority_presence_m +
              economic_threat_index_m + social_contact_m +
              economic_threat_index_dm + lrscale_m +
              household_income_dm + household_income_m +
              unemployment_dm + unemployment_m +
              unemployment_c_dm + unemployment_c_m +
              gdppc_dm + gdppc_m +
              gdppc_c_dm + gdppc_c_m + 
              factor(year) + factor(nuts2) + factor(country), data = df)
  pred <- ggpredict(model, terms = "social_contact_dm")
  
  p <- ggplot(pred, aes(x = x, y = predicted)) +
    geom_line(color = "blue", size = 1.2) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, fill = "blue") +
    labs(title = paste("df =", df_value), x = "Social Contact", y = "Predicted Outgroup Hostility") +
    theme_minimal()
  
  plot_list[[df_value]] <- p
}

grid.arrange(plot_list[[1]], plot_list[[2]], plot_list[[3]], plot_list[[4]], ncol = 2)



df_values <- c(1, 2, 3, 4)
plot_list <- list()

for (df_value in df_values) {
  model <- lm(imwbcnt ~ ns(social_contact_dm, df = df_value) * ns(lrscale_dm, df = df_value) +
              age + gndr + education_numeric +
              minority_presence_dm + minority_presence_m +
              economic_threat_index_m + social_contact_m +
              economic_threat_index_dm + lrscale_m +
              household_income_dm + household_income_m +
              unemployment_dm + unemployment_m +
              unemployment_c_dm + unemployment_c_m +
              gdppc_dm + gdppc_m +
              gdppc_c_dm + gdppc_c_m + 
              factor(year) + factor(nuts2) + factor(country), data = df)
  pred <- ggpredict(model, terms = "social_contact_dm")
  
  p <- ggplot(pred, aes(x = x, y = predicted)) +
    geom_line(color = "blue", size = 1.2) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, fill = "blue") +
    labs(title = paste("df =", df_value), x = "Social Contact", y = "Predicted Outgroup Hostility") +
    theme_minimal()
  
  plot_list[[df_value]] <- p
}

grid.arrange(plot_list[[1]], plot_list[[2]], plot_list[[3]], plot_list[[4]], ncol = 3)


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

grid.arrange(plot_list[[2]], plot_list[[3]], plot_list[[4]], plot_list[[5]], ncol = 2)

model_df1 <- lm(imwbcnt ~ ns(social_contact_dm, df = 2) * ns(economic_threat_index_dm, df = 1) +
                age + gndr + education_numeric +
                minority_presence_dm + minority_presence_m +
                economic_threat_index_m + social_contact_m +
                lrscale_dm + lrscale_m +
                household_income_dm + household_income_m +
                unemployment_dm + unemployment_m +
                unemployment_c_dm + unemployment_c_m +
                gdppc_dm + gdppc_m +
                gdppc_c_dm + gdppc_c_m + 
                factor(year) + factor(nuts2) + factor(country), data = df)

model_df2 <- lm(imwbcnt ~ ns(social_contact_dm, df = 2) * ns(economic_threat_index_dm, df = 2) +
                age + gndr + education_numeric +
                minority_presence_dm + minority_presence_m +
                economic_threat_index_m + social_contact_m +
                lrscale_dm + lrscale_m +
                household_income_dm + household_income_m +
                unemployment_dm + unemployment_m +
                unemployment_c_dm + unemployment_c_m +
                gdppc_dm + gdppc_m +
                gdppc_c_dm + gdppc_c_m + 
                factor(year) + factor(nuts2) + factor(country), data = df)
                
model_df3 <- lm(imwbcnt ~ ns(social_contact_dm, df = 2) * ns(economic_threat_index_dm, df = 3) +
                age + gndr + education_numeric +
                minority_presence_dm + minority_presence_m +
                economic_threat_index_m + social_contact_m +
                lrscale_dm + lrscale_m +
                household_income_dm + household_income_m +
                unemployment_dm + unemployment_m +
                unemployment_c_dm + unemployment_c_m +
                gdppc_dm + gdppc_m +
                gdppc_c_dm + gdppc_c_m + 
                factor(year) + factor(nuts2) + factor(country), data = df)

model_df4 <- lm(imwbcnt ~ ns(social_contact_dm, df = 2) * ns(economic_threat_index_dm, df = 4) +
                age + gndr + education_numeric +
                minority_presence_dm + minority_presence_m +
                economic_threat_index_m + social_contact_m +
                lrscale_dm + lrscale_m +
                household_income_dm + household_income_m +
                unemployment_dm + unemployment_m +
                unemployment_c_dm + unemployment_c_m +
                gdppc_dm + gdppc_m +
                gdppc_c_dm + gdppc_c_m + 
                factor(year) + factor(nuts2) + factor(country), data = df)

# Compare AIC and BIC values
AIC(model_df1, model_df2, model_df3, model_df4)
BIC(model_df1, model_df2, model_df3, model_df4)

model_df01 <- lm(imwbcnt ~ ns(social_contact_dm, df = 2) * ns(lrscale_dm, df = 1) +
                 age + gndr + education_numeric +
                 minority_presence_dm + minority_presence_m +
                 economic_threat_index_m + social_contact_m +
                 economic_threat_index_dm + lrscale_m +
                 household_income_dm + household_income_m +
                 unemployment_dm + unemployment_m +
                 unemployment_c_dm + unemployment_c_m +
                 gdppc_dm + gdppc_m +
                 gdppc_c_dm + gdppc_c_m + 
                 factor(year) + factor(nuts2) + factor(country), data = df)

model_df02 <- lm(imwbcnt ~ ns(social_contact_dm, df = 2) * ns(lrscale_dm, df = 2) +
                 age + gndr + education_numeric +
                 minority_presence_dm + minority_presence_m +
                 economic_threat_index_m + social_contact_m +
                 economic_threat_index_dm + lrscale_m +
                 household_income_dm + household_income_m +
                 unemployment_dm + unemployment_m +
                 unemployment_c_dm + unemployment_c_m +
                 gdppc_dm + gdppc_m +
                 gdppc_c_dm + gdppc_c_m + 
                 factor(year) + factor(nuts2) + factor(country), data = df)

model_df03 <- lm(imwbcnt ~ ns(social_contact_dm, df = 3) * ns(lrscale_dm, df = 3) +
                 age + gndr + education_numeric +
                 minority_presence_dm + minority_presence_m +
                 economic_threat_index_m + social_contact_m +
                 economic_threat_index_dm + lrscale_m +
                 household_income_dm + household_income_m +
                 unemployment_dm + unemployment_m +
                 unemployment_c_dm + unemployment_c_m +
                 gdppc_dm + gdppc_m +
                 gdppc_c_dm + gdppc_c_m + 
                 factor(year) + factor(nuts2) + factor(country), data = df)

model_df04 <- lm(imwbcnt ~ ns(social_contact_dm, df = 4) * ns(lrscale_dm, df = 4) +
                 age + gndr + education_numeric +
                 minority_presence_dm + minority_presence_m +
                 economic_threat_index_m + social_contact_m +
                 economic_threat_index_dm + lrscale_m +
                 household_income_dm + household_income_m +
                 unemployment_dm + unemployment_m +
                 unemployment_c_dm + unemployment_c_m +
                 gdppc_dm + gdppc_m +
                 gdppc_c_dm + gdppc_c_m + 
                 factor(year) + factor(nuts2) + factor(country), data = df)

# Compare AIC and BIC values
AIC(model_df01, model_df02, model_df03, model_df04)
BIC(model_df01, model_df02, model_df03, model_df04)

model_df001 <- lm(imwbcnt ~ ns(social_contact_dm, df = 2) * ns(minority_presence_dm, df = 1) +
                  age + gndr + education_numeric +
                  lrscale_dm + minority_presence_m +
                  economic_threat_index_m + social_contact_m +
                  economic_threat_index_dm + lrscale_m +
                  household_income_dm + household_income_m +
                  unemployment_dm + unemployment_m +
                  unemployment_c_dm + unemployment_c_m +
                  gdppc_dm + gdppc_m +
                  gdppc_c_dm + gdppc_c_m + 
                  factor(year) + factor(nuts2) + factor(country), data = df)

model_df002 <- lm(imwbcnt ~ ns(social_contact_dm, df = 2) * ns(minority_presence_dm, df = 2) +
                  age + gndr + education_numeric +
                  lrscale_dm + minority_presence_m +
                  economic_threat_index_m + social_contact_m +
                  economic_threat_index_dm + lrscale_m +
                  household_income_dm + household_income_m +
                  unemployment_dm + unemployment_m +
                  unemployment_c_dm + unemployment_c_m +
                  gdppc_dm + gdppc_m +
                  gdppc_c_dm + gdppc_c_m + 
                  factor(year) + factor(nuts2) + factor(country), data = df)

model_df003 <- lm(imwbcnt ~ ns(social_contact_dm, df = 3) * ns(minority_presence_dm, df = 3) +
                  age + gndr + education_numeric +
                  lrscale_dm + minority_presence_m +
                  economic_threat_index_m + social_contact_m +
                  economic_threat_index_dm + lrscale_m +
                  household_income_dm + household_income_m +
                  unemployment_dm + unemployment_m +
                  unemployment_c_dm + unemployment_c_m +
                  gdppc_dm + gdppc_m +
                  gdppc_c_dm + gdppc_c_m + 
                  factor(year) + factor(nuts2) + factor(country), data = df)

model_df004 <- lm(imwbcnt ~ ns(social_contact_dm, df = 4) * ns(minority_presence_dm, df = 4) +
                  age + gndr + education_numeric +
                  lrscale_dm + minority_presence_m +
                  economic_threat_index_m + social_contact_m +
                  economic_threat_index_dm + lrscale_m +
                  household_income_dm + household_income_m +
                  unemployment_dm + unemployment_m +
                  unemployment_c_dm + unemployment_c_m +
                  gdppc_dm + gdppc_m +
                  gdppc_c_dm + gdppc_c_m + 
                  factor(year) + factor(nuts2) + factor(country), data = df)

# Compare AIC and BIC values
AIC(model_df001,model_df002, model_df003, model_df004)
BIC(model_df001,model_df002, model_df003, model_df004)


model_linear_ideo <- lm(imwbcnt ~ social_contact_dm * lrscale_dm + 
                        age + gndr + education_numeric +
                        minority_presence_dm + minority_presence_m +
                        economic_threat_index_m + social_contact_m +
                        economic_threat_index_dm + lrscale_m +
                        household_income_dm + household_income_m +
                        unemployment_dm + unemployment_m +
                        unemployment_c_dm + unemployment_c_m +
                        gdppc_dm + gdppc_m +
                        gdppc_c_dm + gdppc_c_m + 
                        factor(year) + factor(nuts2) + factor(country), data = df)

model_linear_minority <- lm(imwbcnt ~ social_contact_dm * minority_presence_dm + 
                            age + gndr + education_numeric +
                            lrscale_dm + minority_presence_m +
                            economic_threat_index_m + social_contact_m +
                            economic_threat_index_dm + lrscale_m +
                            household_income_dm + household_income_m +
                            unemployment_dm + unemployment_m +
                            unemployment_c_dm + unemployment_c_m +
                            gdppc_dm + gdppc_m +
                            gdppc_c_dm + gdppc_c_m + 
                            factor(year) + factor(nuts2) + factor(country), data = df)
                             
AIC(model_linear_ideo, model_df002)  
BIC(model_linear_ideo, model_df002)

AIC(model_linear_minority, model_df002)  
BIC(model_linear_minority, model_df002)


df_values <- c(1, 2, 3, 4)
plot_list <- list()

for (df_value in df_values) {
  model <- lm(imwbcnt ~ ns(social_contact_dm, df = df_value) * ns(minority_presence_dm, df = df_value) +
              age + gndr + education_numeric +
              lrscale_dm + minority_presence_m +
              economic_threat_index_m + social_contact_m +
              economic_threat_index_dm + lrscale_m +
              household_income_dm + household_income_m +
              unemployment_dm + unemployment_m +
              unemployment_c_dm + unemployment_c_m +
              gdppc_dm + gdppc_m +
              gdppc_c_dm + gdppc_c_m + 
              factor(year) + factor(nuts2) + factor(country), data = df)
  pred <- ggpredict(model, terms = "social_contact_dm")
  
  p <- ggplot(pred, aes(x = x, y = predicted)) +
    geom_line(color = "blue", size = 1.2) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, fill = "blue") +
    labs(title = paste("df =", df_value), x = "Social Contact", y = "Predicted Outgroup Hostility") +
    theme_minimal()
  
  plot_list[[df_value]] <- p
}

grid.arrange(plot_list[[1]], plot_list[[2]], plot_list[[3]], plot_list[[4]], ncol = 2)



df_values <- c(1, 2, 3, 4)
plot_list <- list()

for (df_value in df_values) {
  model <- lm(imwbcnt ~ ns(social_contact_dm, df = df_value) * ns(lrscale_dm, df = df_value) +
              age + gndr + education_numeric +
              minority_presence_dm + minority_presence_m +
              economic_threat_index_m + social_contact_m +
              economic_threat_index_dm + lrscale_m +
              household_income_dm + household_income_m +
              unemployment_dm + unemployment_m +
              unemployment_c_dm + unemployment_c_m +
              gdppc_dm + gdppc_m +
              gdppc_c_dm + gdppc_c_m + 
              factor(year) + factor(nuts2) + factor(country), data = df)
  pred <- ggpredict(model, terms = "social_contact_dm")
  
  p <- ggplot(pred, aes(x = x, y = predicted)) +
    geom_line(color = "blue", size = 1.2) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, fill = "blue") +
    labs(title = paste("df =", df_value), x = "Social Contact", y = "Predicted Outgroup Hostility") +
    theme_minimal()
  
  plot_list[[df_value]] <- p
}

grid.arrange(plot_list[[1]], plot_list[[2]], plot_list[[3]], plot_list[[4]], ncol = 3)


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

grid.arrange(plot_list[[2]], plot_list[[3]], plot_list[[4]], plot_list[[5]], ncol = 2)



