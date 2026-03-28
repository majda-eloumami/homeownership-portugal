# =============================================================================
# Determinants of Homeownership in Portugal — Script 05: Model Comparison
# Authors: Majda El Oumami, Américo Serra, Ciro Mainella
# Description: Produces the final side-by-side model comparison table using
#              stargazer and a model fit summary.
# =============================================================================

library(stargazer)
library(sandwich)
library(lmtest)
library(dplyr)
library(ggplot2)

# --- 1. Load Models ----------------------------------------------------------

df        <- readRDS("data/processed/homeownership_processed.rds")
ols_model <- readRDS("data/processed/ols_model.rds")
frac_mods <- readRDS("data/processed/fractional_models.rds")

frac_logit  <- frac_mods$frac_logit
frac_probit <- frac_mods$frac_probit

# --- 2. Stargazer Comparison Table -------------------------------------------

cat("=== Model Comparison: OLS, Fractional Logit, Fractional Probit ===\n\n")

stargazer(
  ols_model, frac_logit, frac_probit,
  type          = "text",
  title         = "Determinants of Homeownership Rates — Portuguese Municipalities",
  column.labels = c("OLS", "Frac. Logit", "Frac. Probit"),
  covariate.labels = c(
    "Log(Income)", "House Price Growth", "Log(Unemployment)",
    "Log(High Educ. Share)", "Share 25-34", "Share Male",
    "Log(Foreign Share)", "Log(Tourism Intensity)", "Log(Population)"
  ),
  se = list(
    sqrt(diag(vcovHC(ols_model,   type = "HC1"))),
    sqrt(diag(vcovHC(frac_logit,  type = "HC1"))),
    sqrt(diag(vcovHC(frac_probit, type = "HC1")))
  ),
  omit.stat    = c("f", "ser"),
  digits       = 4,
  star.cutoffs = c(0.05, 0.01, 0.001)
)

# --- 3. Model Fit Summary ----------------------------------------------------

model_fit <- data.frame(
  Model    = c("OLS", "Frac. Logit", "Frac. Probit"),
  AIC      = round(c(AIC(ols_model), AIC(frac_logit), AIC(frac_probit)), 2),
  BIC      = round(c(BIC(ols_model), BIC(frac_logit), BIC(frac_probit)), 2),
  Deviance = round(c(deviance(ols_model), deviance(frac_logit), deviance(frac_probit)), 4),
  R2       = c(round(summary(ols_model)$r.squared, 4), NA, NA)
)

cat("\n=== Model Fit Statistics ===\n")
print(model_fit)
write.csv(model_fit, "outputs/model_fit_comparison.csv", row.names = FALSE)
cat("✓ Model fit comparison saved\n")

# --- 4. Predicted vs Actual Plot ---------------------------------------------

# Use model's own data to avoid missing value row mismatch
pred_df <- data.frame(
  Actual    = ols_model$model$homeownership_rate,
  Predicted = fitted(ols_model)
)

p_pred <- ggplot(pred_df, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.5, color = "#1a5276", size = 1.8) +
  geom_abline(slope = 1, intercept = 0, color = "#c0392b",
              linetype = "dashed", linewidth = 1) +
  labs(
    title    = "OLS: Predicted vs Actual Homeownership Rate",
    subtitle = "Points on the red line = perfect prediction",
    x        = "Actual Homeownership Rate",
    y        = "Predicted Homeownership Rate"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title    = element_text(face = "bold"),
        plot.subtitle = element_text(color = "gray40"))

ggsave("figures/09_predicted_vs_actual.png", p_pred, width = 7, height = 6, dpi = 150)
cat("✓ Figure 9: Predicted vs Actual saved\n")

# --- 5. Key Findings ---------------------------------------------------------

cat("\n=== KEY FINDINGS ===\n")
cat("• Log(Income):     Negative and significant → higher-income municipalities\n")
cat("                   have LOWER homeownership (compositional effect)\n")
cat("• Share 25-34:     Strongly negative → youth concentration reduces ownership\n")
cat("• Log(Unemploy.):  Negative → labor market insecurity reduces ownership\n")
cat("• Log(High Educ.): Positive in fractional models → education promotes wealth\n")
cat("• Tourism/Foreign: Not significant at municipality level\n")
cat("• House Prices:    Not significant → may be endogenous\n")
cat("\n• Fractional models preferred: homeownership rate is bounded [0,1]\n")
cat("• OLS and fractional models agree on signs and significance\n")

cat("\n✓ Script 05 complete — analysis finished!\n")