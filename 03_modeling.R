# =============================================================================
# 03_modeling.R — Lag selection & ARIMA/ARIMAX estimation
# =============================================================================

library(tidyverse)
library(lubridate)
library(tsibble)
library(fable)
library(fabletools)
library(feasts)

# Reload data
df_price <- read_csv("data/kbh_quarter_sqm_price.csv", show_col_types = FALSE) |>
  mutate(date = yq(gsub("K", "Q", Quarter))) |>
  filter(!is.na(date)) |>
  arrange(date)

df_bond <- read_csv("data/bondInt_quarterly.csv", show_col_types = FALSE) |>
  mutate(date = yq(gsub("K", "Q", Quarter))) |>
  filter(!is.na(date)) |>
  arrange(date)

df <- inner_join(df_price, df_bond, by = c("Quarter", "date")) |>
  mutate(qtr = yearquarter(date)) |>
  as_tsibble(index = qtr)

# Estimate optimal Box-Cox lambda using Guerrero's method
lambda <- df |>
  features(Price, features = guerrero) |>
  pull(lambda_guerrero)

# Apply Box-Cox transformation
df <- df |>
  mutate(price_transformed = box_cox(Price, lambda))

cutoff_qtr <- yearquarter(yq("2019 Q4"))
train <- df |> filter(qtr <= cutoff_qtr)
test  <- df |> filter(qtr > cutoff_qtr)


# =============================================================================
# STEP 1 — Lag selection for exogenous variable (bond rate)
# =============================================================================

cat("Lag selection for bond rate (AICc comparison):\n\n")

lag_results <- tibble(lag_k = c(0, 1, 2, 4)) |>
  mutate(
    data = map(lag_k, ~ {
      train |>
        mutate(rate_lag = lag(mean_rate, .x)) |>
        filter(!is.na(rate_lag))
    }),
    model = map(data, ~ {
      .x |> model(
        ARIMA(price_transformed ~ rate_lag,
              stepwise = FALSE, approximation = FALSE)
      )
    }),
    AICc = map_dbl(model, ~ glance(.x)$AICc)
  ) |>
  select(lag_k, AICc)

print(lag_results)
best_k <- lag_results$lag_k[which.min(lag_results$AICc)]
cat("\n✓ Best lag:", best_k, "quarter(s)\n\n")


# =============================================================================
# STEP 2 — Prepare dataset with chosen lag
# =============================================================================

train <- train |> mutate(rate_lag = lag(mean_rate, best_k))
test  <- test  |> mutate(rate_lag = lag(mean_rate, best_k))

# Remove NAs from lag creation
train_clean <- train |> filter(!is.na(rate_lag))
test_clean  <- test  |> filter(!is.na(rate_lag))

cat("Clean train:", nrow(train_clean), "| Clean test:", nrow(test_clean), "\n\n")


# =============================================================================
# STEP 3 — Fit ARIMA and ARIMAX
# =============================================================================

cat("=" %|% rep("-", 75) |> paste(collapse = "") %|% "=\n")
cat("MODEL ESTIMATION\n")
cat("=" %|% rep("-", 75) |> paste(collapse = "") %|% "=\n\n")

fit <- train_clean |>
  model(
    arima  = ARIMA(price_transformed, stepwise = FALSE, approximation = FALSE),
    arimax = ARIMA(price_transformed ~ rate_lag, stepwise = FALSE, approximation = FALSE)
  )

cat("--- ARIMA ---\n")
report(fit |> select(arima))

cat("\n--- ARIMAX (with rate lag) ---\n")
report(fit |> select(arimax))

# Coefficients
cat("\n--- ARIMAX coefficients ---\n")
print(tidy(fit |> select(arimax)))


# =============================================================================
# STEP 4 — Model comparison
# =============================================================================

cat("\n" %|% rep("=", 79) |> paste(collapse = "") %|% "\n")
cat("MODEL COMPARISON (AICc / BIC)\n")
cat(rep("=", 79) |> paste(collapse = "") %|% "\n\n")

print(glance(fit) |> select(.model, AICc, BIC))


# =============================================================================
# STEP 5 — Residual diagnostics
# =============================================================================

cat("\n" %|% rep("=", 79) |> paste(collapse = "") %|% "\n")
cat("RESIDUAL DIAGNOSTICS\n")
cat(rep("=", 79) |> paste(collapse = "") %|% "\n\n")

# ARIMA
fit |> select(arima) |> gg_tsresiduals() +
  ggtitle("ARIMA: residual diagnostics")
ggsave("plots/03_residuals_arima.png", width = 14, height = 10, units = "cm")

# ARIMAX
fit |> select(arimax) |> gg_tsresiduals() +
  ggtitle("ARIMAX: residual diagnostics")
ggsave("plots/03_residuals_arimax.png", width = 14, height = 10, units = "cm")


# =============================================================================
# STEP 6 — Ljung-Box test on residuals
# =============================================================================

cat("Ljung-Box test (lag = 8, H0: white noise):\n\n")

augment(fit) |>
  features(.innov, ljung_box, lag = 8, dof = 0) |>
  print()

cat("\nInterpretation: p > 0.05 → cannot reject white noise → model captures structure\n")


# =============================================================================
# Save models for forecasting
# =============================================================================

# Save as RDS for use in 04_forecast_validate.R
saveRDS(fit, "models/03_fit.rds")
saveRDS(test_clean, "models/03_test_clean.rds")

cat("\n✓ Models saved to models/03_*.rds\n")
