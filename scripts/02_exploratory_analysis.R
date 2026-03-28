# =============================================================================
# Determinants of Homeownership in Portugal — Script 02: Exploratory Analysis
# Authors: Majda El Oumami, Américo Serra, Ciro Mainella
# Description: Visualizes distributions of key variables (raw and transformed),
#              scatter plots vs homeownership, and a correlation heatmap.
# =============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)

# --- 1. Load Data ------------------------------------------------------------

df <- readRDS("data/processed/homeownership_processed.rds")

# --- 2. Plot 1: Distribution of Homeownership Rate ---------------------------

p1 <- ggplot(df, aes(x = homeownership_rate)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30,
                 fill = "#2e86c1", alpha = 0.7, color = "white") +
  geom_density(color = "#c0392b", linewidth = 1) +
  stat_function(
    fun  = dnorm,
    args = list(mean = mean(df$homeownership_rate, na.rm = TRUE),
                sd   = sd(df$homeownership_rate, na.rm = TRUE)),
    color = "#27ae60", linetype = "dashed", linewidth = 1
  ) +
  labs(
    title    = "Distribution of Homeownership Rate — 308 Portuguese Municipalities",
    subtitle = "Red = kernel density | Green dashed = normal distribution",
    x        = "Homeownership Rate",
    y        = "Density"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title    = element_text(face = "bold"),
        plot.subtitle = element_text(color = "gray40"))

ggsave("figures/01_homeownership_distribution.png", p1, width = 9, height = 5, dpi = 150)
cat("✓ Figure 1 saved\n")

# --- 3. Plot 2: Log(Income) Distribution -------------------------------------

p2 <- ggplot(df, aes(x = log_income)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30,
                 fill = "#2e86c1", alpha = 0.7, color = "white") +
  geom_density(color = "#c0392b", linewidth = 1) +
  labs(
    title = "Distribution of Log(Income) Across Municipalities",
    x     = "Log(Income)",
    y     = "Density"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

ggsave("figures/02_log_income_distribution.png", p2, width = 8, height = 5, dpi = 150)
cat("✓ Figure 2 saved\n")

# --- 4. Plot 3: Tourism Intensity (Raw vs Log) --------------------------------

p3a <- ggplot(df, aes(x = tourism_intensity)) +
  geom_histogram(bins = 30, fill = "#884ea0", alpha = 0.7, color = "white") +
  labs(title = "Tourism Intensity — Raw (Highly Skewed)",
       x = "Tourism Intensity", y = "Count") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

p3b <- ggplot(df, aes(x = log_tourism_intensity)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30,
                 fill = "#884ea0", alpha = 0.7, color = "white") +
  geom_density(color = "#c0392b", linewidth = 1) +
  labs(title = "Log(Tourism Intensity) — After Transformation",
       x = "Log(Tourism Intensity + 1)", y = "Density") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

ggsave("figures/03a_tourism_raw.png", p3a, width = 7, height = 4.5, dpi = 150)
ggsave("figures/03b_tourism_log.png", p3b, width = 7, height = 4.5, dpi = 150)
cat("✓ Figures 3a & 3b saved\n")

# --- 5. Plot 4: Scatter Plots — Homeownership vs Key Predictors --------------

scatter_plot <- function(xvar, xlabel, color) {
  ggplot(df, aes(x = .data[[xvar]], y = homeownership_rate)) +
    geom_point(alpha = 0.5, color = color, size = 1.8) +
    geom_smooth(method = "lm", se = TRUE, color = "#c0392b", linewidth = 0.8) +
    labs(
      title = paste("Homeownership Rate vs", xlabel),
      x     = xlabel,
      y     = "Homeownership Rate"
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"))
}

p4a <- scatter_plot("log_income",            "Log(Income)",            "#1a5276")
p4b <- scatter_plot("share_25_34",           "Share Aged 25–34",       "#117a65")
p4c <- scatter_plot("log_unemp_total",       "Log(Unemployment Rate)", "#884ea0")
p4d <- scatter_plot("log_tourism_intensity", "Log(Tourism Intensity)", "#b7950b")

ggsave("figures/04a_scatter_income.png",  p4a, width = 7, height = 5, dpi = 150)
ggsave("figures/04b_scatter_youth.png",   p4b, width = 7, height = 5, dpi = 150)
ggsave("figures/04c_scatter_unemp.png",   p4c, width = 7, height = 5, dpi = 150)
ggsave("figures/04d_scatter_tourism.png", p4d, width = 7, height = 5, dpi = 150)
cat("✓ Scatter plots saved\n")

# --- 6. Plot 5: Correlation Heatmap ------------------------------------------

model_vars <- c(
  "homeownership_rate", "log_income", "houseprice_growth",
  "log_unemp_total", "log_high_educ_share", "share_25_34",
  "share_male", "log_foreign_share", "log_tourism_intensity", "log_pop_total"
)

var_labels <- c(
  "Homeownership", "Log Income", "House Price Growth",
  "Log Unemployment", "Log High Educ.", "Share 25–34",
  "Share Male", "Log Foreign", "Log Tourism", "Log Population"
)

corr_matrix <- cor(df[, model_vars], use = "complete.obs")
corr_df <- as.data.frame(corr_matrix) %>%
  mutate(Var1 = var_labels) %>%
  pivot_longer(-Var1, names_to = "Var2_raw", values_to = "Correlation") %>%
  mutate(Var2 = var_labels[match(Var2_raw, model_vars)])

p5 <- ggplot(corr_df, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Correlation, 2)), size = 2.8) +
  scale_fill_gradient2(low = "#c0392b", mid = "white", high = "#1a5276",
                       midpoint = 0, limits = c(-1, 1)) +
  labs(
    title = "Correlation Matrix — Key Model Variables",
    x = NULL, y = NULL, fill = "r"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title  = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

ggsave("figures/05_correlation_heatmap.png", p5, width = 9, height = 8, dpi = 150)
cat("✓ Figure 5: Correlation heatmap saved\n")

cat("\n✓ All EDA figures saved to figures/\n")