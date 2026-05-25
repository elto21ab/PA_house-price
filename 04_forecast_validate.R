# =============================================================================
# 04_forecast_validate.R — Out-of-sample forecast & accuracy metrics
# =============================================================================

library(tidyverse)
library(lubridate)
library(tsibble)
library(fable)
library(fabletools)

# Load models and test data from previous script
fit <- readRDS("models/03_fit.rds")
test_clean <- readRDS("models/03_test_clean.rds")


# =============================================================================
# STEP 1 — Generate forecasts on holdout period
# =============================================================================

cat("Generating forecasts for holdout period (2020 Q1 onwards):\n")
cat("Using observed rate values as perfect-foresight scenario\n\n")

fc <- fit |> forecast(new_data = test_clean)

cat("Forecasts generated:", nrow(fc), "steps\n\n")


# =============================================================================
# STEP 2 — Plot forecasts vs. actuals
# =============================================================================

# For ARIMA (prices only)
p_arima <- fc |>
  filter(.model == "arima") |>
  autoplot(test_clean, level = NULL) +
  autolayer(test_clean, price_transformed, colour = "black", linetype = "dashed") +
  labs(title = "ARIMA: forecast vs. realised",
       y = "Transformed price", x = NULL) +
  theme_minimal(base_size = 11)

ggsave("plots/04_forecast_arima.png", p_arima, width = 14, height = 8, units = "cm")

# For ARIMAX (with rate)
p_arimax <- fc |>
  filter(.model == "arimax") |>
  autoplot(test_clean, level = NULL) +
  autolayer(test_clean, price_transformed, colour = "black", linetype = "dashed") +
  labs(title = "ARIMAX (with rate): forecast vs. realised",
       y = "Transformed price", x = NULL) +
  theme_minimal(base_size = 11)

ggsave("plots/04_forecast_arimax.png", p_arimax, width = 14, height = 8, units = "cm")

cat("Forecast plots saved to plots/04_*.png\n\n")


# =============================================================================
# STEP 3 — Calculate accuracy metrics
# =============================================================================

cat("=" %|% rep("-", 75) |> paste(collapse = "") %|% "=\n")
cat("OUT-OF-SAMPLE ACCURACY (RMSE / MAE / MAPE / MASE)\n")
cat("=" %|% rep("-", 75) |> paste(collapse = "") %|% "=\n\n")

acc <- fc |> accuracy(test_clean)

print(
  acc |>
    select(.model, .type, RMSE, MAE, MAPE, MASE) |>
    arrange(RMSE)
)

# Find best model
best_model <- acc |>
  filter(.type == "Test") |>
  arrange(RMSE) |>
  slice(1) |>
  pull(.model)

cat("\n✓ Best model (lowest RMSE):", best_model, "\n\n")


# =============================================================================
# STEP 4 — Extract point forecasts & confidence intervals
# =============================================================================

cat("Forecast summary (point estimates + 80% PI):\n\n")

fc_summary <- fc |>
  filter(.model == best_model) |>
  select(qtr, .mean, .lower, .upper) |>
  left_join(test_clean |> select(qtr, log_price), by = "qtr")

print(fc_summary)


# =============================================================================
# STEP 5 — Interpretation
# =============================================================================

cat("\n" %|% rep("=", 79) |> paste(collapse = "") %|% "\n")
cat("SUMMARY & INTERPRETATION\n")
cat(rep("=", 79) |> paste(collapse = "") %|% "\n\n")

if (best_model == "arimax") {
  cat("✓ ARIMAX outperforms ARIMA\n")
  cat("  → Bond rate is a useful exogenous predictor of housing prices\n\n")
  cat("Interpretation:\n")
  cat("  Lagged bond rate has significant predictive power for log-price growth.\n")
  cat("  This suggests interest rates affect housing market dynamics with a lag.\n")
} else {
  cat("✓ ARIMA outperforms ARIMAX\n")
  cat("  → Bond rate does not improve out-of-sample forecast accuracy\n\n")
  cat("Interpretation:\n")
  cat("  Housing prices follow their own dynamic; lagged rate provides minimal signal.\n")
  cat("  (May reflect market adaptation or structural changes over time.)\n")
}

cat("\n")
