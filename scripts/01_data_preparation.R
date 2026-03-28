# =============================================================================
# Determinants of Homeownership in Portugal — Script 01: Data Preparation
# Authors: Majda El Oumami, Américo Serra, Ciro Mainella
# Description: Loads and cleans the municipality-level dataset, creates
#              log transformations, and saves the processed data.
# =============================================================================

# --- 1. Package Management ---------------------------------------------------

required_packages <- c("readxl", "dplyr", "ggplot2", "moments")

install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
  library(pkg, character.only = TRUE)
}

invisible(lapply(required_packages, install_if_missing))

# --- 2. Load Data ------------------------------------------------------------

data_path <- "C:/Users/Lenovo/Desktop/homeownership-portugal/data/raw/base data.xlsx"

df <- read_excel(data_path, range = "A1:AE309")

cat(sprintf("Data loaded: %d observations, %d variables\n", nrow(df), ncol(df)))
cat("Variables:\n")
print(names(df))

# --- 3. Key Variables --------------------------------------------------------

key_vars <- c(
  "homeownership_rate", "income", "houseprice_growth",
  "unemp_total", "high_educ_share", "share_25_34",
  "share_male", "foreign_share", "tourism_intensity", "pop_total"
)

# --- 4. Descriptive Statistics -----------------------------------------------

detailed_summary <- function(x) {
  data.frame(
    Mean      = round(mean(x, na.rm = TRUE), 4),
    Median    = round(median(x, na.rm = TRUE), 4),
    SD        = round(sd(x, na.rm = TRUE), 4),
    Min       = round(min(x, na.rm = TRUE), 4),
    Max       = round(max(x, na.rm = TRUE), 4),
    Skewness  = round(moments::skewness(x, na.rm = TRUE), 4),
    Kurtosis  = round(moments::kurtosis(x, na.rm = TRUE), 4),
    N_missing = sum(is.na(x))
  )
}

cat("\n=== Descriptive Statistics — Raw Variables ===\n")
desc_raw <- do.call(rbind, lapply(df[, key_vars], detailed_summary))
desc_raw$Variable <- key_vars
desc_raw <- desc_raw[, c("Variable", setdiff(names(desc_raw), "Variable"))]
print(desc_raw, digits = 4)

write.csv(desc_raw, "outputs/descriptive_statistics_raw.csv", row.names = FALSE)
cat("✓ Raw descriptive statistics saved\n")

cat("\n=== Missing Values ===\n")
print(colSums(is.na(df[, key_vars])))

# --- 5. Log Transformations --------------------------------------------------

df <- df %>%
  mutate(
    log_income            = log(income),
    log_unemp_total       = log(unemp_total + 0.01),
    log_high_educ_share   = log(high_educ_share + 0.01),
    log_foreign_share     = log(foreign_share + 0.001),
    log_tourism_intensity = log(tourism_intensity + 1),
    log_pop_total         = log(pop_total)
  )

transformed_vars <- c(
  "log_income", "log_unemp_total", "log_high_educ_share",
  "log_foreign_share", "log_tourism_intensity", "log_pop_total"
)

cat("\n=== Descriptive Statistics — Transformed Variables ===\n")
desc_transformed <- do.call(rbind, lapply(df[, transformed_vars], detailed_summary))
desc_transformed$Variable <- transformed_vars
desc_transformed <- desc_transformed[, c("Variable", setdiff(names(desc_transformed), "Variable"))]
print(desc_transformed, digits = 4)

write.csv(desc_transformed, "outputs/descriptive_statistics_transformed.csv", row.names = FALSE)
cat("✓ Transformed descriptive statistics saved\n")

# --- 6. Save Processed Data --------------------------------------------------

saveRDS(df, file = "data/processed/homeownership_processed.rds")
write.csv(df, "outputs/data_processed.csv", row.names = FALSE)

cat("\n✓ Processed data saved\n")
cat(sprintf("Final dataset: %d municipalities, %d variables\n", nrow(df), ncol(df)))