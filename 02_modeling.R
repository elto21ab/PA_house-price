# =============================================================================
# 02_modeling.R — Bivariate analysis & modelling (ARIMA, ARIMAX, VAR)
# Predictive Analytics, CBS
# =============================================================================
# Loads cleaned data saved by 01_combined_eda.R, applies the train/test split
# here, then runs the full modelling pipeline:
#
#   - Bivariate diagnostics (CCF, QLR break test on Δprice ~ Δrate)
#   - Lag selection for the rate
#   - ARIMA, ARIMAX, VAR estimation
#   - Residual diagnostics + Ljung-Box
#   - Forecast on holdout, plot, RMSE/MAE/MAPE comparison
# =============================================================================


# ── 0.  Packages ────────────────────────────────────────────────────────────
# install.packages(c("tidyverse","tsibble","fable","fabletools","feasts",
#                    "urca","strucchange","scales"))

library(tidyverse)
library(tsibble)
library(fable)
library(fabletools)
library(feasts)
library(urca)
library(strucchange)
library(scales)

# Defensive rebinds: stats::filter / stats::lag mask dplyr versions otherwise.
select <- dplyr::select
filter <- dplyr::filter
lag    <- dplyr::lag

theme_set(theme_minimal(base_size = 12))


# =============================================================================
# 1. LOAD CLEANED DATA AND JOIN
# =============================================================================

price  <- read_rds("data/price_clean.rds")
rate   <- read_rds("data/rate_clean.rds")
lambda <- read_rds("data/lambda.rds")

cat("Loaded Box-Cox λ from EDA:", round(lambda, 4), "\n")

df <- inner_join(as_tibble(price), as_tibble(rate), by = "qtr") |>
  arrange(qtr) |>
  as_tsibble(index = qtr)

cat("Joined sample:", format(min(df$qtr)), "to", format(max(df$qtr)),
    " (n =", nrow(df), ")\n")


# =============================================================================
# 2. TRAIN / TEST SPLIT (training ≤ 2019 Q4)
# =============================================================================
# Everything downstream — Box-Cox λ, lag selection, model fitting — uses
# the training set only. The test set is touched once, in the forecast step.

cutoff_qtr <- yearquarter("2019 Q4")

train <- df |> filter(qtr <= cutoff_qtr)
test  <- df |> filter(qtr >  cutoff_qtr)

cat("Train:", format(min(train$qtr)), "to", format(max(train$qtr)),
    " (n =", nrow(train), ")\n")
cat("Test: ", format(min(test$qtr)),  "to", format(max(test$qtr)),
    " (n =", nrow(test), ")\n")


# =============================================================================
# 3. BUILD log(Price) FOR MODELLING
# =============================================================================
# We use the Box-Cox λ estimated in 01_combined_eda.R (loaded above).
# Since that λ is very close to 0, we work with the natural log throughout
# for interpretability — a coefficient on Δlog(Price) reads as a percent
# change in the original DKK/m² scale.

train <- train |> mutate(log_price = log(Price))
test  <- test  |> mutate(log_price = log(Price))


# =============================================================================
# 4. CROSS-CORRELATION (CCF) — Δlog(Price) vs. Δrate
# =============================================================================
# Visualises the lead-lag relationship. Negative lags = rate leads price.

dlogp <- diff(train$log_price)
drate <- diff(train$rate)

ccf(drate, dlogp, lag.max = 12,
    main = "CCF: Δrate (x) vs. Δlog(Price) (y)")

png("plots/02-01_ccf_logprice_rate_diff.png",
    width = 12, height = 8, units = "cm", res = 150)
ccf(drate, dlogp, lag.max = 12,
    main = "CCF: Δrate (x) vs. Δlog(Price) (y)")
dev.off()
graphics.off()

# =============================================================================
# 5. STRUCTURAL BREAK TEST (BIVARIATE QLR / supF)
# =============================================================================
# Tests for an unknown break in the relationship Δlog(Price)_t = α + β·Δrate_t.
# This is the substantively interesting break test for our research question.

bp_data <- tibble(dlogp = diff(train$log_price),
                  drate = diff(train$rate))

fs <- Fstats(dlogp ~ drate, data = bp_data, from = 0.15)
plot(fs, main = "QLR (supF) test — Δlog(Price) ~ Δrate")
png("plots/02-02_qlr_logprice_rate.png",
    width = 12, height = 8, units = "cm", res = 150)
plot(fs, main = "QLR (supF) test — Δlog(Price) ~ Δrate")
dev.off()


print(sctest(fs))

break_idx  <- which.max(fs$Fstats) + floor(0.15 * nrow(bp_data))
break_date <- train$qtr[break_idx + 1]   # +1 because we differenced
cat("\nEstimated break date:", format(break_date), "\n")


# =============================================================================
# 6. LAG SELECTION FOR THE EXOGENOUS RATE
# =============================================================================
# Try ARIMAX with the rate entering at lags 0, 1, 2, 4 and pick the lag with
# the lowest AICc.

lag_grid <- tibble(k = c(0, 1, 2, 4)) |>
  mutate(model = map(k, ~ {
    d <- train |>
      mutate(rate_lag = lag(rate, .x)) |>
      filter(!is.na(rate_lag))
    d |> model(ARIMA(log_price ~ rate_lag,
                     stepwise = FALSE, approximation = FALSE))
  }),
  AICc = map_dbl(model, ~ glance(.x)$AICc))

print(lag_grid |> select(k, AICc))
best_k <- lag_grid$k[which.min(lag_grid$AICc)]
cat("\nBest lag for rate:", best_k, "Q\n")


# =============================================================================
# 7. ESTIMATE ARIMA AND ARIMAX
# =============================================================================
# Full AICc grid search (stepwise = FALSE, approximation = FALSE).

train <- train |> mutate(rate_lag = lag(rate, best_k))
test  <- test  |> mutate(rate_lag = lag(rate, best_k))

fit_uni <- train |>
  filter(!is.na(rate_lag)) |>
  model(
    arima  = ARIMA(log_price,
                   stepwise = FALSE, approximation = FALSE),
    arimax = ARIMA(log_price ~ rate_lag,
                   stepwise = FALSE, approximation = FALSE)
  )

report(fit_uni |> select(arima))
report(fit_uni |> select(arimax))


# =============================================================================
# 8. ESTIMATE BIVARIATE VAR
# =============================================================================
# fable::VAR is qualified explicitly because library(vars) (if loaded
# elsewhere) would mask it with the lower-level vars::VAR.

fit_var <- train |>
  model(var = fable::VAR(vars(log_price, rate), ic = "aicc"))
report(fit_var)


# =============================================================================
# 9. RESIDUAL DIAGNOSTICS
# =============================================================================

ARIMA <- fit_uni |> select(arima)  |> gg_tsresiduals() + ggtitle("ARIMA residuals")
ggsave("plots/02-03_ARIMA_residuals.png", plot = ARIMA,
       width = 12, height = 8, units = "cm")

ARIMAX <- fit_uni |> select(arimax) |> gg_tsresiduals() + ggtitle("ARIMAX residuals")
ggsave("plots/02-04_ARIMAX_residuals.png", plot = ARIMAX,
       width = 12, height = 8, units = "cm")


# =============================================================================
# 10. LJUNG-BOX TEST (white-noise residuals)
# =============================================================================
# H0: residuals are white noise. Want p > 0.05 → model captures the structure.

augment(fit_uni) |>
  features(.innov, ljung_box, lag = 8, dof = 0) |>
  print()


# =============================================================================
# 11. COMPARE ARIMA vs ARIMAX (in-sample AICc / BIC)
# =============================================================================
# Note: AICc/BIC are not directly comparable to the VAR's, since the VAR
# fits a joint likelihood over (log_price, rate). Out-of-sample RMSE in
# Step 13 is the apples-to-apples comparison.

glance(fit_uni) |> select(.model, AICc, BIC) |> print()


# =============================================================================
# 12. FORECAST HOLDOUT PERIOD
# =============================================================================
# For ARIMAX we supply the observed rate_lag values over the test window
# (perfect-foresight scenario), to isolate the rate channel's predictive
# contribution from forecast error in the rate itself.

future_x <- test |>
  filter(!is.na(rate_lag)) |>
  select(qtr, rate_lag)

fc_uni <- fit_uni |> forecast(new_data = future_x)

# VAR forecasts price and rate jointly; we feed it the test window length.
fc_var <- fit_var |> forecast(h = nrow(future_x))


# =============================================================================
# 13. PLOT FORECASTS
# =============================================================================

fc_uni_plot <- fc_uni |>
  autoplot(filter(train, qtr >= yearquarter("2010 Q1")), level = 80) +
  autolayer(test, log_price, colour = "black", linetype = "dashed") +
  labs(title = "Forecasts vs. realised log(Price)",
       y = "log(DKK/m²)") +
  facet_wrap(~ .model)
print(fc_uni_plot)
ggsave("plots/02-05_forecast_arima_arimax.png",
       plot = fc_uni_plot, width = 16, height = 8, units = "cm")

fc_var_plot <- fc_var |>
  autoplot(filter(train, qtr >= yearquarter("2010 Q1")), level = 80) +
  autolayer(test, log_price, colour = "black", linetype = "dashed") +
  labs(title = "VAR forecast vs. realised log(Price)",
       y = "log(DKK/m²)")
print(fc_var_plot)
ggsave("plots/02-06_forecast_var.png",
       plot = fc_var_plot, width = 14, height = 8, units = "cm")


# =============================================================================
# 14. COMPARE FORECAST ACCURACY (RMSE / MAE / MAPE)
# =============================================================================

acc_uni <- fc_uni |> accuracy(test |> mutate(log_price = log(Price)))
acc_var <- fc_var |> accuracy(test |> mutate(log_price = log(Price)))

bind_rows(acc_uni, acc_var) |>
  select(.model, RMSE, MAE, MAPE, MASE) |>
  arrange(RMSE) |>
  print()


# =============================================================================
# 15. ARIMAX COEFFICIENTS (for interpretation in the paper)
# =============================================================================

fit_uni |> select(arimax) |> tidy() |> print()


# =============================================================================
# End of script
# =============================================================================
