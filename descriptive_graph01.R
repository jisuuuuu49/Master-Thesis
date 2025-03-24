# ---------------------------
# Load Required Packages
# ---------------------------
library(tidyverse)     
library(janitor)       
library(pastecs)       
library(writexl)       
library(fastDummies)  
library(ggplot2)       

# ---------------------------
# Load and Clean Data
# ---------------------------
#setwd("/Users/aunal/Desktop/Uni/Study/ss 2024/master thesis/data")
df <- read_csv("your file.csv") %>% clean_names()
#df <- read_csv("ESS_EUROSTAT_final_standardized-2.csv") %>%
#  clean_names()

# ---------------------------
# Function: Aggregate from NUTS3 to NUTS2 Level: in NUTS2 there were some NUTS3 are included so i added up to NUTS2 and drop the NUTS3 level 
# ---------------------------
#aggregate_to_nuts2 <- function(df) {
#  df %>%
#    mutate(nuts2_parent = ifelse(nchar(nuts2) == 5, substr(nuts2, 1, 4), nuts2)) %>%
#    group_by(nuts2_parent, year) %>%
#    summarise(
#      N = sum(!is.na(idno)),
#      across(where(is.numeric) & !any_of("idno"), \(x) sum(x, na.rm = TRUE)),
#      .groups = "drop"
#    ) %>%
#    rename(nuts2 = nuts2_parent)
#}

# ---------------------------
# Aggregation & Normalization
# ---------------------------
#df_aggregated <- aggregate_to_nuts2(df)

#exclude_cols <- c("country", "nuts2", "idno", "year", "gndr", "nuts1", "eisced",
#                  "have_child", "net_mig", "gdppc", "immig_background", "pop_density",
#                  "unemployment", "education_numeric")

#df_aggregated <- df_aggregated %>%
#  group_by(year) %>%
#  mutate(across(
#    .cols = setdiff(names(df_aggregated), exclude_cols),
#    .fns = \(x) ifelse(is.na(x), NA, (x - min(x, na.rm = TRUE)) / 
#                        (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))),
#    .names = "scaled_{.col}"
#  )) %>%
#  ungroup()

# ---------------------------
# Count Observations per Region per Year
# ---------------------------
nuts2_years <- df_aggregated %>%
  group_by(nuts2, year) %>%
  summarise(n = sum(N), .groups = "drop")

nuts2_years_filtered <- nuts2_years %>%
  filter(
    year != "9999",
    nuts2 != "9999",
    nuts2 != "IL",
    !is.na(nuts2),
    !str_detect(nuts2, "^DE|^UK"),
    nchar(nuts2) > 3
  )

# ---------------------------
# Plot: Observations per Region Across Years
# ---------------------------
ggplot(nuts2_years_filtered, aes(x = factor(year), y = n, fill = nuts2)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d() +
  labs(
    title = "Figure 1: Number of Observations per Region Across Years",
    x = "Year",
    y = "Number of Observations"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, size = 10),
    axis.text.y = element_text(size = 10),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.3, "cm"),
    legend.position = "right"
  )
