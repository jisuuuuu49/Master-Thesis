# ---------------------------
# Load Required Packages
# ---------------------------
install.packages(c("RColorBrewer","ggplot2", "dplyr", "tidyverse", "sjPlot", "gridExtra","grid"))
library(ggplot2)
library(dplyr)
library(tidyverse)
library(sjPlot)
library(gridExtra)
library(RColorBrewer)
library(grid)

# ---------------------------
# Set Working Directory & Load Data
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
  ) %>%
  # Exclude Germany (DE) and United Kingdom (UK)
  filter(!(country %in% c("DE", "UK")))

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
  mutate(year_group = case_when(
    year %in% c("2002", "2003") ~ "ESS1",
    year %in% c("2014", "2015") ~ "ESS7",
    TRUE ~ "Other Years"  # Keeps other years, if available
  ))

# ---------------------------
# Define Individual-Level Predictors for Trend Analysis
# ---------------------------
indiv_vars <- c("social_contact", "lrscale", "minority_presence", "economic_threat_index")
indiv_labels <- c("Social Contact", "LR Scale", "Minority Presence", "Economic Threat")

missing_vars <- setdiff(indiv_vars, colnames(df))
df <- df %>%
  filter(!is.na(social_contact) & !is.na(lrscale) & !is.na(minority_presence) & !is.na(economic_threat_index))


plot_trend <- function(var, label) {
  num_countries <- length(unique(df$country))

  ggplot(df, aes(x = year, y = !!sym(var), group = country, color = country)) +
    stat_summary(fun = mean, geom = "line", size = 0.7) +  # Line plot with mean values
    stat_summary(fun = mean, geom = "point", size = 1.5) +  # Points at mean values
    scale_color_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(num_countries)) + # Fixed max colors
    facet_wrap(~year_group, scales = "free_x") +  # Separate 2002-2003 and 2014-2015
    labs(
      title = paste("Trend of", label, "Over Time"),
      x = "Year",
      y = paste("Average", label)
    ) +
    theme_minimal() +
    theme(legend.position = "right",
          axis.text = element_text(size = 11, color = "black"),
          axis.title = element_text(size = 12))
}

# ---------------------------
# Generate Trend Plots for Each Predictor
# ---------------------------
trend_plots <- list()

for (i in seq_along(indiv_vars)) {
  if (indiv_vars[i] %in% colnames(df)) {  # Ensure variable exists before plotting
    trend_plots[[indiv_labels[i]]] <- plot_trend(indiv_vars[i], indiv_labels[i])
  }
}

# ---------------------------
# Arrange and Display Plots
# ---------------------------
title_grob <- textGrob("Figure 2: Trends Over Time",
                       x = 0, hjust = 0,  # Left-aligned
                       gp = gpar(fontsize = 16))
grid.arrange(
  arrangeGrob(grobs = trend_plots, nrow = 2, top = title_grob)  # Add title grob inside arrangeGrob()
)


