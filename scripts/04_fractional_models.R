# =============================================================================
# Determinants of Homeownership in Portugal — Script 04: Fractional Models
# Authors: Majda El Oumami, Américo Serra, Ciro Mainella
# Description: Estimates fractional logit and probit models, computes
#              average marginal effects, and compares models.
# =============================================================================

library(lmtest)
library(sandwich)
library(marginaleffects)
library(ggplot2)
library(dplyr)

# --- 1. Load Data ------------------------------------------------------------

df        <- readRDS("data/processed/homeownership_processed.rds")
ols_model <- readRDS("data/processed/ols_model.rds")

formula_rhs <- homeownership_rate ~ log_income + houseprice_growth +
  log_unemp_total + log_high_educ_share + share_25_34 +
  share_male + log_foreign_share + log_tourism_intensity + log_pop_total

# --- 2. Fractional Logit -----------------------------------------------------

cat("=== Fractional Logit Model ===\n")

frac_logit <- glm(
  formula_rhs,
  data   = df,
  family = binomial(link = "logit")
)

logit_robust <- coeftest(frac_logit, vcov = vcovHC(frac_logit, type = "HC1"))
cat("\n--- Fractional Logit (Robust SE) ---\n")
print(logit_robust)

cat(sprintf("\nAIC: %.2f | BIC: %.2f | Deviance: %.4f\n",
            AIC(frac_logit), BIC(frac_logit), deviance(frac_logit)))

cat("\n--- Average Marginal Effects (Logit) ---\n")
me_logit <- avg_slopes(frac_logit, vcov = "HC1")
print(me_logit)

# --- 3. Fractional Probit ----------------------------------------------------

cat("\n=== Fractional Probit Model ===\n")

frac_probit <- glm(
  formula_rhs,
  data   = df,
  family = binomial(link = "probit")
)

probit_robust <- coeftest(frac_probit, vcov = vcovHC(frac_probit, type = "HC1"))
cat("\n--- Fractional Probit (Robust SE) ---\n")
print(probit_robust)

cat(sprintf("\nAIC: %.2f | BIC: %.2f | Deviance: %.4f\n",
            AIC(frac_probit), BIC(frac_probit), deviance(frac_probit)))

cat("\n--- Average Marginal Effects (Probit) ---\n")
me_probit <- avg_slopes(frac_probit, vcov = "HC1")
print(me_probit)

# --- 4. Save Results ---------------------------------------------------------

# Save logit coefficients manually
logit_coef_df <- data.frame(
  Variable  = rownames(summary(frac_logit)$coefficients),
  Estimate  = summary(frac_logit)$coefficients[, 1],
  Std_Error = sqrt(diag(vcovHC(frac_logit, type = "HC1"))),
  row.names = NULL
)
logit_coef_df$t_value <- logit_coef_df$Estimate / logit_coef_df$Std_Error
logit_coef_df$p_value <- 2 * pt(-abs(logit_coef_df$t_value),
                                df = nobs(frac_logit) - length(coef(frac_logit)))

# Save probit coefficients manually
probit_coef_df <- data.frame(
  Variable  = rownames(summary(frac_probit)$coefficients),
  Estimate  = summary(frac_probit)$coefficients[, 1],
  Std_Error = sqrt(diag(vcovHC(frac_probit, type = "HC1"))),
  row.names = NULL
)
probit_coef_df$t_value <- probit_coef_df$Estimate / probit_coef_df$Std_Error
probit_coef_df$p_value <- 2 * pt(-abs(probit_coef_df$t_value),
                                 df = nobs(frac_probit) - length(coef(frac_probit)))

write.csv(logit_coef_df,            "outputs/fractional_logit_coefficients.csv",  row.names = FALSE)
write.csv(probit_coef_df,           "outputs/fractional_probit_coefficients.csv", row.names = FALSE)
write.csv(as.data.frame(me_logit),  "outputs/marginal_effects_logit.csv",         row.names = FALSE)
write.csv(as.data.frame(me_probit), "outputs/marginal_effects_probit.csv",        row.names = FALSE)
cat("✓ Model results saved\n")

# --- 5. Marginal Effects Comparison Plot -------------------------------------

ols_me <- data.frame(
  term     = names(coef(ols_model))[-1],
  estimate = coef(ols_model)[-1],
  Model    = "OLS"
)

logit_me_df <- as.data.frame(me_logit) %>%
  select(term, estimate) %>%
  mutate(Model = "Frac. Logit")

probit_me_df <- as.data.frame(me_probit) %>%
  select(term, estimate) %>%
  mutate(Model = "Frac. Probit")

all_me <- bind_rows(ols_me, logit_me_df, probit_me_df)

all_me <- all_me %>%
  mutate(term_label = case_match(term,
                                 "log_income"            ~ "Log(Income)",
                                 "houseprice_growth"     ~ "House Price Growth",
                                 "log_unemp_total"       ~ "Log(Unemployment)",
                                 "log_high_educ_share"   ~ "Log(High Education)",
                                 "share_25_34"           ~ "Share 25-34",
                                 "share_male"            ~ "Share Male",
                                 "log_foreign_share"     ~ "Log(Foreign Share)",
                                 "log_tourism_intensity" ~ "Log(Tourism)",
                                 "log_pop_total"         ~ "Log(Population)",
                                 .default = term
  ))

p_me <- ggplot(all_me, aes(x = reorder(term_label, estimate),
                           y = estimate, fill = Model)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  scale_fill_manual(values = c(
    "OLS"          = "#1a5276",
    "Frac. Logit"  = "#117a65",
    "Frac. Probit" = "#884ea0"
  )) +
  coord_flip() +
  labs(
    title    = "Average Marginal Effects — OLS vs Fractional Logit vs Probit",
    subtitle = "All three models show consistent signs across predictors",
    x        = NULL,
    y        = "Marginal Effect on Homeownership Rate",
    fill     = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold"),
    plot.subtitle   = element_text(color = "gray40"),
    legend.position = "bottom"
  )

ggsave("figures/08_marginal_effects_comparison.png", p_me, width = 10, height = 6, dpi = 150)
cat("✓ Figure 8: Marginal effects comparison saved\n")

# --- 6. Save Models ----------------------------------------------------------

saveRDS(list(frac_logit = frac_logit, frac_probit = frac_probit),
        "data/processed/fractional_models.rds")

cat("\n✓ Script 04 complete.\n")