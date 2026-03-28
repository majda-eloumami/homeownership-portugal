# =============================================================================
# Determinants of Homeownership in Portugal — Script 03: OLS Regression
# Authors: Majda El Oumami, Américo Serra, Ciro Mainella
# Description: Estimates OLS with robust standard errors, runs diagnostic
#              tests (VIF, Breusch-Pagan, White, RESET), and saves results.
# =============================================================================

library(car)
library(lmtest)
library(sandwich)
library(ggplot2)
library(dplyr)

# --- 1. Load Data ------------------------------------------------------------

df <- readRDS("data/processed/homeownership_processed.rds")

# --- 2. OLS Model ------------------------------------------------------------

cat("=== OLS Regression — Homeownership Rate ===\n")

ols_model <- lm(
  homeownership_rate ~ log_income + houseprice_growth +
    log_unemp_total + log_high_educ_share + share_25_34 +
    share_male + log_foreign_share + log_tourism_intensity + log_pop_total,
  data = df
)

cat("\n--- Standard OLS Results ---\n")
print(summary(ols_model))

ols_robust    <- coeftest(ols_model, vcov = vcovHC(ols_model, type = "HC1"))
ols_robust_ci <- coefci(ols_model,   vcov = vcovHC(ols_model, type = "HC1"))

cat("\n--- OLS with Robust Standard Errors (HC1) ---\n")
print(ols_robust)

cat("\n--- 95% Confidence Intervals (Robust) ---\n")
print(round(ols_robust_ci, 4))

print(colnames(as.data.frame(ols_robust)))

# Extract coefficients manually
ols_coef_df <- data.frame(
  Variable  = rownames(summary(ols_model)$coefficients),
  Estimate  = summary(ols_model)$coefficients[, 1],
  Std_Error = sqrt(diag(vcovHC(ols_model, type = "HC1"))),
  row.names = NULL
)

# Compute t-values and p-values with robust SE
ols_coef_df$t_value <- ols_coef_df$Estimate / ols_coef_df$Std_Error
ols_coef_df$p_value <- 2 * pt(-abs(ols_coef_df$t_value), df = nobs(ols_model) - length(coef(ols_model)))

print(ols_coef_df)
write.csv(ols_coef_df, "outputs/ols_robust_coefficients.csv", row.names = FALSE)
cat("✓ OLS coefficients saved\n")

cat("✓ OLS coefficients saved\n")

# --- 3. Diagnostic Tests -----------------------------------------------------

cat("\n=== DIAGNOSTIC TESTS ===\n")

# VIF
cat("\n--- VIF Test ---\n")
vif_values <- vif(ols_model)
print(round(vif_values, 3))
cat(sprintf("Mean VIF: %.3f\n", mean(vif_values)))
cat(ifelse(max(vif_values) > 10,
           "⚠ VIF > 10 detected — potential multicollinearity\n",
           "✓ No severe multicollinearity (all VIF < 10)\n"))

write.csv(data.frame(Variable = names(vif_values), VIF = round(vif_values, 4)),
          "outputs/vif_results.csv", row.names = FALSE)

# Breusch-Pagan
cat("\n--- Breusch-Pagan Test ---\n")
bp_test <- bptest(ols_model)
print(bp_test)
cat(ifelse(bp_test$p.value < 0.05,
           "⚠ Heteroskedasticity detected → robust SEs appropriate\n",
           "✓ No significant heteroskedasticity\n"))

# White Test
cat("\n--- White Test ---\n")
ols_aux    <- lm(residuals(ols_model)^2 ~ fitted(ols_model) + I(fitted(ols_model)^2))
n          <- nobs(ols_model)
white_stat <- n * summary(ols_aux)$r.squared
white_pval <- 1 - pchisq(white_stat, df = 2)
cat(sprintf("White test statistic: %.4f\n", white_stat))
cat(sprintf("P-value:              %.4f\n", white_pval))

# RESET Test
cat("\n--- Ramsey RESET Test ---\n")
reset_test <- resettest(ols_model, power = 2:3, type = "fitted")
print(reset_test)

diag_summary <- data.frame(
  Test      = c("Breusch-Pagan", "White Test", "RESET Test"),
  Statistic = round(c(bp_test$statistic, white_stat, reset_test$statistic), 4),
  p_value   = round(c(bp_test$p.value,   white_pval, reset_test$p.value), 4),
  Conclusion = c(
    ifelse(bp_test$p.value    < 0.05, "Heteroskedasticity present", "Homoskedastic"),
    ifelse(white_pval         < 0.05, "Heteroskedasticity present", "Homoskedastic"),
    ifelse(reset_test$p.value < 0.05, "Misspecification possible",  "Adequate form")
  )
)
write.csv(diag_summary, "outputs/diagnostic_tests.csv", row.names = FALSE)
cat("✓ Diagnostic tests saved\n")

# --- 4. Residual Plot --------------------------------------------------------

resid_df <- data.frame(
  Fitted    = fitted(ols_model),
  Residuals = residuals(ols_model)
)

p_resid <- ggplot(resid_df, aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.5, color = "#1a5276", size = 1.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#c0392b") +
  geom_smooth(method = "loess", se = FALSE, color = "#27ae60", linewidth = 0.8) +
  labs(
    title    = "OLS Residuals vs Fitted Values",
    subtitle = "Fanning pattern would indicate heteroskedasticity",
    x        = "Fitted Values",
    y        = "Residuals"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title    = element_text(face = "bold"),
        plot.subtitle = element_text(color = "gray40"))

ggsave("figures/06_ols_residuals_vs_fitted.png", p_resid, width = 8, height = 5, dpi = 150)
cat("✓ Figure 6: Residual plot saved\n")

# --- 5. Coefficient Plot -----------------------------------------------------

coef_plot_df <- data.frame(
  Variable    = rownames(ols_robust)[-1],
  Estimate    = ols_robust[-1, "Estimate"],
  SE          = ols_robust[-1, "Std. Error"],
  p_value     = ols_robust[-1, "Pr(>|t|)"]
) %>%
  mutate(
    CI_low      = Estimate - 1.96 * SE,
    CI_high     = Estimate + 1.96 * SE,
    Significant = ifelse(p_value < 0.05, "p < 0.05", "p ≥ 0.05")
  )

p_coef <- ggplot(coef_plot_df,
                 aes(x = reorder(Variable, Estimate),
                     y = Estimate, color = Significant)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), width = 0.3, linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = c("p < 0.05" = "#1a5276", "p ≥ 0.05" = "#aab7b8")) +
  coord_flip() +
  labs(
    title = "OLS Coefficient Plot with 95% Confidence Intervals (Robust SE)",
    x     = NULL,
    y     = "Coefficient Estimate",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom")

ggsave("figures/07_ols_coefficient_plot.png", p_coef, width = 9, height = 6, dpi = 150)
cat("✓ Figure 7: Coefficient plot saved\n")

saveRDS(ols_model, "data/processed/ols_model.rds")
cat("\n✓ Script 03 complete.\n")