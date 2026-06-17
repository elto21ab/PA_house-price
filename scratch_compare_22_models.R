# Script to compare model variations out-of-sample (original DKK/m² scale)
# Models: ARIMA (base + full history), ARIMAX (lag1 + lag2, base + dummy),
#         VAR (base + post-break, levels only). Log vs Box-Cox.
# Removed (unjustified): VAR diffs, ARIMAX contemp post-break, ARIMAX lag2 post-break.
library(tidyverse)
library(tsibble)
library(fable)
library(fabletools)
library(feasts)
library(forecast)

price_raw <- read_csv("data/kbh_quarter_sqm_price.csv", show_col_types = FALSE)
rate_raw  <- read_csv("data/bondint_quarterly.csv", show_col_types = FALSE)

price <- price_raw |>
  mutate(
    qtr = yearquarter(str_replace(Quarter, "K", " Q")),
    Price = as.numeric(Price)
  ) |>
  select(qtr, Price) |>
  arrange(qtr) |>
  as_tsibble(index = qtr)

rate <- rate_raw |>
  mutate(
    qtr = yearquarter(str_replace(Quarter, "K", " Q")),
    rate = as.numeric(mean_rate)
  ) |>
  select(qtr, rate) |>
  arrange(qtr) |>
  as_tsibble(index = qtr)

df <- inner_join(price, rate, by = "qtr") |>
  arrange(qtr) |>
  as_tsibble(index = qtr)

ratio <- floor(nrow(df) * 0.2)
train_raw <- df |> slice_head(n = nrow(df) - ratio)
test_raw  <- df |> slice_tail(n = ratio)

lambda_train <- 0.3226227
best_k <- 2

run_accuracy_check <- function(transform_type) {
  # transform_type can be "boxcox" or "log"
  
  if (transform_type == "boxcox") {
    trans_fn <- function(x) forecast::BoxCox(x, lambda_train)
    inv_fn   <- function(x) forecast::InvBoxCox(x, lambda_train)
  } else {
    trans_fn <- function(x) log(x)
    inv_fn   <- function(x) exp(x)
  }
  
  # Prepare datasets
  df_model <- df |>
    arrange(qtr) |>
    mutate(
      y = trans_fn(Price),
      y_d = difference(y),
      rate_d = difference(rate),
      rate_lag = dplyr::lag(rate, best_k),
      rate_lag1 = dplyr::lag(rate, 1)
    )
  
  train <- df_model |> filter(qtr %in% train_raw$qtr)
  test  <- df_model |> filter(qtr %in% test_raw$qtr)
  
  # A. ARIMA (Base)
  fit_arima <- train |> model(m = ARIMA(y, stepwise = FALSE, approximation = FALSE))
  fc_arima <- forecast(fit_arima, new_data = test)
  arima_pred <- inv_fn(fc_arima$.mean)
  
  # B. ARIMA (Full History)
  price_train_full <- price |> filter(qtr < yearquarter("2020 Q3"))
  price_test_full  <- price |> filter(qtr >= yearquarter("2020 Q3"))
  
  if (transform_type == "boxcox") {
    lambda_full <- price_train_full |>
      features(Price, features = guerrero) |>
      pull(lambda_guerrero)
    fit_arima_full <- price_train_full |>
      mutate(y = forecast::BoxCox(Price, lambda_full)) |>
      model(m = ARIMA(y, stepwise = FALSE, approximation = FALSE))
    fc_arima_full <- forecast(fit_arima_full, new_data = price_test_full)
    arima_full_pred <- forecast::InvBoxCox(fc_arima_full$.mean, lambda_full)
  } else {
    fit_arima_full <- price_train_full |>
      mutate(y = log(Price)) |>
      model(m = ARIMA(y, stepwise = FALSE, approximation = FALSE))
    fc_arima_full <- forecast(fit_arima_full, new_data = price_test_full)
    arima_full_pred <- exp(fc_arima_full$.mean)
  }
  
  # C. ARIMAX Contemp (Base, d=1)
  fit_arimax_d1 <- train |> model(m = ARIMA(y ~ pdq(d = 1) + rate, stepwise = FALSE, approximation = FALSE))
  fc_arimax_d1 <- forecast(fit_arimax_d1, new_data = test)
  arimax_d1_pred <- inv_fn(fc_arimax_d1$.mean)
  
  # D. ARIMAX Lag 2 (Base, d=1)
  fit_arimax_lag_d1 <- train |> drop_na(rate_lag) |> model(m = ARIMA(y ~ pdq(d = 1) + rate_lag, stepwise = FALSE, approximation = FALSE))
  fc_arimax_lag_d1 <- forecast(fit_arimax_lag_d1, new_data = test)
  arimax_lag_d1_pred <- inv_fn(fc_arimax_lag_d1$.mean)
  
  # E. ARIMAX Contemp (Post-Break, d=1)
  train_post <- train |> filter(qtr >= yearquarter("2006 Q3"))
  fit_arimax_contemp_post <- train_post |> model(m = ARIMA(y ~ pdq(d = 1) + rate, stepwise = FALSE, approximation = FALSE))
  fc_arimax_contemp_post <- forecast(fit_arimax_contemp_post, new_data = test)
  arimax_contemp_post_pred <- inv_fn(fc_arimax_contemp_post$.mean)
  
  # F. ARIMAX Lag 2 (Post-Break, d=1)
  fit_arimax_lag_post <- train_post |> model(m = ARIMA(y ~ pdq(d = 1) + rate_lag, stepwise = FALSE, approximation = FALSE))
  fc_arimax_lag_post <- forecast(fit_arimax_lag_post, new_data = test)
  arimax_lag_post_pred <- inv_fn(fc_arimax_lag_post$.mean)
  
  # G. ARIMAX Lag 2 (Dummy Model, d=1)
  train_dummy <- train |>
    mutate(
      break_dummy = if_else(qtr >= yearquarter("2006 Q3"), 1, 0),
      rate_lag_dummy = rate_lag * break_dummy
    )
  test_dummy <- test |>
    mutate(
      break_dummy = if_else(qtr >= yearquarter("2006 Q3"), 1, 0),
      rate_lag_dummy = rate_lag * break_dummy
    )
  fit_arimax_dummy <- train_dummy |> drop_na(rate_lag) |>
    model(m = ARIMA(y ~ pdq(d = 1) + rate_lag + break_dummy + rate_lag_dummy, stepwise = FALSE, approximation = FALSE))
  fc_arimax_dummy <- forecast(fit_arimax_dummy, new_data = test_dummy)
  arimax_dummy_pred <- inv_fn(fc_arimax_dummy$.mean)
  
  # H. VAR (Base, levels)
  fit_var <- train |> model(m = fable::VAR(vars(y, rate), ic = "aicc"))
  fc_var <- forecast(fit_var, h = nrow(test))
  var_pred <- inv_fn(fc_var$.mean[, "y"])
  
  # I. VAR (Post-Break, levels)
  train_post <- train |> filter(qtr >= yearquarter("2006 Q3"))
  fit_var_post <- train_post |> model(m = fable::VAR(vars(y, rate), ic = "aicc"))
  fc_var_post <- forecast(fit_var_post, h = nrow(test))
  var_post_pred <- inv_fn(fc_var_post$.mean[, "y"])
  
  # J. ARIMAX Lag 1 (Base, d=1) — rate_lag1 already computed on full series above
  fit_arimax_lag1 <- train |> drop_na(rate_lag1) |> model(m = ARIMA(y ~ pdq(d = 1) + rate_lag1, stepwise = FALSE, approximation = FALSE))
  fc_arimax_lag1 <- forecast(fit_arimax_lag1, new_data = test)
  arimax_lag1_pred <- inv_fn(fc_arimax_lag1$.mean)
  
  # K. ARIMAX Lag 1 (Dummy Model, d=1)
  train_lag1_dummy <- train |>
    mutate(break_dummy = if_else(qtr >= yearquarter("2006 Q3"), 1, 0),
           rate_lag1_dummy = rate_lag1 * break_dummy)
  test_lag1_dummy <- test |>
    mutate(break_dummy = if_else(qtr >= yearquarter("2006 Q3"), 1, 0),
           rate_lag1_dummy = rate_lag1 * break_dummy)
  fit_arimax_lag1_dummy <- train_lag1_dummy |> drop_na(rate_lag1) |>
    model(m = ARIMA(y ~ pdq(d = 1) + rate_lag1 + break_dummy + rate_lag1_dummy, stepwise = FALSE, approximation = FALSE))
  fc_arimax_lag1_dummy <- forecast(fit_arimax_lag1_dummy, new_data = test_lag1_dummy)
  arimax_lag1_dummy_pred <- inv_fn(fc_arimax_lag1_dummy$.mean)

  
  # Compute metrics in original price levels
  calc_metrics <- function(pred, actual, name) {
    err <- actual - pred
    rmse <- sqrt(mean(err^2))
    mae  <- mean(abs(err))
    mape <- mean(abs(err / actual)) * 100
    tibble(
      .model = name,
      RMSE = round(rmse, 0),
      MAE  = round(mae, 0),
      MAPE = round(mape, 2)
    )
  }
  
  actual_prices <- test_raw$Price
  
  bind_rows(
    calc_metrics(arima_pred,              actual_prices, "ARIMA (Inner-Join)"),
    calc_metrics(arima_full_pred,         actual_prices, "ARIMA (Full History)"),
    calc_metrics(arimax_d1_pred,          actual_prices, "ARIMAX Contemp (d=1)"),
    calc_metrics(arimax_lag1_pred,        actual_prices, "ARIMAX Lag 1 (d=1)"),
    calc_metrics(arimax_lag_d1_pred,      actual_prices, "ARIMAX Lag 2 (d=1)"),
    calc_metrics(arimax_dummy_pred,       actual_prices, "ARIMAX Lag 2 + Break Dummy (d=1)"),
    calc_metrics(arimax_lag1_dummy_pred,  actual_prices, "ARIMAX Lag 1 + Break Dummy (d=1)"),
    calc_metrics(var_pred,                actual_prices, "VAR"),
    calc_metrics(var_post_pred,           actual_prices, "VAR (Post-Break)")
  ) |>
    mutate(transformation = transform_type)
}

print("Running Box-Cox models...")
results_boxcox <- run_accuracy_check("boxcox")

print("Running Natural Log models...")
results_log <- run_accuracy_check("log")

all_results <- bind_rows(results_boxcox, results_log)

print_table <- function(df, title) {
  cat("\n=============================================================================\n")
  cat(title, "\n")
  cat("=============================================================================\n")
  cat(sprintf("%-45s %7s %7s %8s  %s\n", "Model", "RMSE", "MAE", "MAPE", "Transform"))
  cat(strrep("-", 80), "\n")
  df_sorted <- df |> arrange(RMSE)
  for (i in seq_len(nrow(df_sorted))) {
    r <- df_sorted[i, ]
    cat(sprintf("%-45s %7.0f %7.0f %7.2f%%  %s\n",
                r$.model, r$RMSE, r$MAE, r$MAPE, r$transformation))
  }
}

print_table(results_boxcox, "TABLE 1: BOX-COX TRANSFORMATION MODELS")
print_table(results_log, "TABLE 2: NATURAL LOG TRANSFORMATION MODELS")
print_table(all_results, "TABLE 3: COMBINED ACCURACY COMPARISON (ALL MODELS)")

