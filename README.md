# Master’s Thesis Repository
## *Social Contact and Outgroup Hostility*

This repository accompanies the master's thesis entitled **“Social Contact and Outgroup Hostility”**, submitted by **Jisu Kim (1246559)** in partial fulfillment of the requirements for the Master of Science in Social and Economic Data Science at the **University of Konstanz**.

This project investigates the role of interpersonal contact with immigrants in shaping public hostility toward outgroups across European regions. The study further examines how this relationship is moderated by perceived economic threat, political ideology, and migration context. A combination of multilevel and fixed-effects modeling is employed to analyze harmonized data from the **European Social Survey (ESS)** and **Eurostat**, covering 22 countries and 365 NUTS2 regions.

---

## 1. Thesis Overview

- **Author**: Jisu Kim  
- **Supervisors**: Dr. Peter Selb (First), Dr. Christian Breunig (Second)  
- **Institution**: University of Konstanz  
- **Degree Program**: MSc Social and Economic Data Science  
- **Completion Date**: March 25, 2025  

---

## 2. Research Objectives

This thesis addresses the following research questions:

1. Is there a statistically significant relationship between social contact and outgroup hostility? If so, is this relationship linear or non-linear?
2. Are there thresholds (tipping points) where the effect of social contact changes direction?
3. How do regional and national migration levels moderate the relationship between social contact and hostility toward outgroups?

## 2.1 Theoretical Frameworks
This study is theoretically grounded in Intergroup Contact Theory (Allport, 1954) and Group Threat Theory (Blalock, 1967). While contact theory posits that interpersonal interaction with outgroups reduces prejudice, threat theory suggests that such interaction—particularly under perceived competition—may intensify hostility. These theories motivate the interaction terms and non-linear modeling strategies employed throughout this project.


---

## 3. Data Sources

### 3.1 Individual-Level Data
- **European Social Survey (ESS)** – Rounds 1 (2002/03) and 7 (2014/15)
- Key variables: intergroup contact, economic perceptions, ideology, education, income, age, gender, attitudes toward immigrants

### 3.2 Regional and Country-Level Contextual Data
- **Eurostat Regional Indicators**
  - Crude net migration rate (with adjustment)
  - Regional GDP per capita (PPS)
  - Unemployment rate
  - Population density
- **Eurostat National Indicators**
  -  Unemployment rate
  -  National GDP per capita (PPS)
> *Note: Germany and the United Kingdom are excluded due to incompatibility in regional (NUTS) coding.*

---

## 4. Methodology Summary

### 4.1 Core Analytical Framework
- **Three-Level Multilevel Linear Model (MLM)**: Individuals (Level 1) nested within regions (NUTS2, Level 2), nested within countries (Level 3)
- **Fixed-effects linear regression**: Controls for unobserved time-invariant heterogeneity
- **Non-linear modeling**: Implemented using spline transformations and segmented regression

### 4.2 Key Interactions Analyzed
- Social Contact × Economic Threat  
- Social Contact × Political Ideology  
- Social Contact × Minority Presence  
- Social Contact × Net Migration (regional and national, double-demeaned)

### 4.3 Technical Features
- **Group-mean centering**: Decomposes predictors into within-region and between-region components.
- **Double-demeaned interactions**: Used for cross-level interaction terms (e.g. Social Contact × Net Migration) to isolate within-region moderation effects.
- **Spline and segmented regression**: Employed to model non-linear relationships and identify potential tipping points in the effects of social contact.

### 4.4 Robustness Checks
- Fractional polynomial regression  
- Clustered standard errors  
- Ordered logistic models  
- Subgroup analysis (natives vs immigrants)  
- Country and year exclusions

---

## 5. Code Structure

| File                | Description                                                    |
|---------------------|----------------------------------------------------------------|
| `ess_selection.sql` | Extracts and reverse-codes individual-level variables from ESS |
| `merged_eurostat.sql` | Selects and processes regional and national contextual variables from Eurostat |
| `descriptive_table.ipynb` | Generates descriptive statistics (education subgroups) using Python |
| `descriptive_graph01.R` | Plots the number of observations per NUTS2-year unit |
| `descriptive_graph02.R` | Visualizes trends in key independent variables |
| `main_model_MLM&OLS.R` | Specifies and estimates multilevel and fixed-effects regression models |
| `marginal effect and interaction terms.R` | Constructs spline-based interaction plots and segmented models |
| `segmented regression and marginal effect.R` | Detects breakpoints and estimates segmented regression models |
| `normality check.R` | Assesses normality of residuals via histogram and Q-Q plots |
| `robustness_check.R` | Implements alternative model specifications for robustness |

---

## 6. Replication Guide

### Step 1: Data Acquisition
- Download and prepare harmonized datasets from ESS Rounds 1 and 7
- Extract contextual variables from Eurostat at NUTS2 and country level

### Step 2: Data Processing
- Use `ess_selection.sql` and `merged_eurostat.sql` (via Google BigQuery) to process and merge datasets
- Export as a single CSV file

### Step 3: Run Analyses
1. **Descriptive Statistics & Graphs**:  
   Run `descriptive_table.ipynb`, `descriptive_graph01.R`, and `descriptive_graph02.R`  
2. **Main Models**:  
   Execute `main_model_MLM&OLS.R` for baseline and full multilevel models and splines with linear regression with fixed effects
3. **Marginal & Interaction Effects**:  
   Use `marginal effect and interaction terms.R` and `segmented regression and marginal effect.R`  
4. **Diagnostics and Checks**:  
   - `normality check.R` for residual diagnostics  
   - `robustness_check.R` for alternative specifications and subgroup analyses  

---

## 7. Software & Package Requirements
- SQL queries (`ess_selection.sql` and `merged_eurostat.sql`) are designed to be executed in Google BigQuery.
- All R scripts require R version ≥ 4.2.
- The descriptive statistics notebook is developed in Google Colab using Python.

### R (≥ 4.2)
tidyverse, janitor, lme4, sjPlot, performance, insight,
splines, segmented, sandwich, lmtest, clubSandwich, ggeffects, mfp, gridExtra

### Python (used Google Collab)
pandas, scikit-learn, matplotlib

### SQL (Google Big Query)

## 8. Access and Ethics
- This project uses data from the [European Social Survey (ESS)](https://www.europeansocialsurvey.org) and [Eurostat](https://ec.europa.eu/eurostat). All data are used in accordance with the respective licensing terms and citation guidelines.

