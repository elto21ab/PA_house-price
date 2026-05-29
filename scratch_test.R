# Scratch test to check results from Housing exam 2.qmd
library(tidyverse)
library(tsibble)
library(fable)
library(fabletools)
library(feasts)
library(urca)
library(strucchange)

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

df_raw <- inner_join(price, rate, by = "qtr") |>
  arrange(qtr) |>
  as_tsibble(index = qtr)

df_model_raw <- df_raw |>
  mutate(
    rate_lag2 = dplyr::lag(rate, 2)
  ) |>
  drop_na(rate_lag2)

n_test <- floor(nrow(df_model_raw) * 0.20)

train_raw <- df_model_raw |>
  slice_head(n = nrow(df_model_raw) - n_test)

test_raw <- df_model_raw |>
  slice_tail(n = n_test)

lambda_train <- train_raw |>
  features(Price, guerrero) |>
  pull(lambda_guerrero)

df_model <- df_model_raw |>
  mutate(
    price_bc = box_cox(Price, lambda_train),
    price_bc_diff = difference(price_bc),
    rate_diff = difference(rate)
  )

train <- df_model |>
  slice_head(n = nrow(df_model) - n_test)

test <- df_model |>
  slice_tail(n = n_test)

fit_models <- train |>
  model(
    arima = ARIMA(price_bc),
    arimax = ARIMA(price_bc ~ rate),
    arimax_lag2 = ARIMA(price_bc ~ rate_lag2)
  )

print(report(fit_models))

fc_models <- fit_models |>
  forecast(new_data = test)

print(accuracy(fc_models, test) |> select(.model, RMSE, MAE, MAPE, ACF1))
