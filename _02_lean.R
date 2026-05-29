# =============================================================================
# Modeling
# =============================================================================
library (tidyverse)
library(tsibble)
library (fable)
library (fabletools)
library (feasts)
library (ggtime)
library (urca)
library (strucchange)
library (scales)

select <- dplyr::select

train  <- read_rds("data/train.rds")
test   <- read_rds("data/test.rds")

# Rate leads price by 1 period: price_t uses rate_{t-1}.
#train <- train |>
#	mutate(rate_d = difference(rate))

#test <- test |>
#	mutate(rate_d = difference(rate))

train
test

cat("Train sample:", format(min(train$qtr)), "to", format(max(train$qtr)), " (n =", nrow(train), ")\n")
cat("Test sample:", format(min(test$qtr)), "to", format(max(test$qtr)), " (n =", nrow(test), ")\n")

# =============================================================================
# CCF: Δrate (x) vs. Δprice (y). Negative lags = rate leads price.
# =============================================================================

ccf_data <- train |>
	select(rate_d, price_bc_d) |>
	drop_na()

png("plots/02-01_ccf_price_bc_d_rate_d.png",
		width = 12, height = 8, units = "cm", res = 150)
ccf(ccf_data$rate_d, ccf_data$price_bc_d, lag.max = 12,
		main = "CCF: Δrate (x) vs. Δprice_bc (y)")
dev.off()

# Lag selection for rate in ARIMAX (AICc).
k_candidates <- c(0, 1, 2, 3, 4, 5)
lag_grid <- tibble(k = k_candidates) |>
	mutate(model = map(k, ~ {
		d <- train |>
			mutate(rate_lag = lag(rate, .x)) |>
			drop_na(rate_lag, price_bc)
		d |> model(ARIMA(price_bc ~ rate_lag,
										 stepwise = FALSE, approximation = FALSE))
	}),
	AICc = map_dbl(model, ~ glance(.x)$AICc))

print(lag_grid |> select(k, AICc))
best_k <- lag_grid$k[which.min(lag_grid$AICc)]
cat("\nBest lag (AICc):", best_k, "Q\n")
#best_k = 2 lags

# Keep full train/test; build lagged slices only where needed.
train_lag <- train |>
	mutate(rate_lag = lag(rate, best_k))

test_lag <- test |>
	mutate(rate_lag = lag(rate, best_k))

df_model <- bind_rows(train, test)

fit_arima <- train |> model(arima = ARIMA(price_bc))
fit_arimax <- train |> model(arimax = ARIMA(price_bc ~ rate))
fit_arimax_lag <- train_lag |> model(arimax_lag = ARIMA(price_bc ~ rate_lag))

report(fit_arima)
report(fit_arimax)
report(fit_arimax_lag)

fc_arima <- forecast(fit_arima, new_data = test)
fc_arimax <- forecast(fit_arimax, new_data = test)
fc_arimax_lag <- forecast(fit_arimax_lag, new_data = test_lag |> drop_na(rate_lag, price_bc))

fc_models <- bind_rows(fc_arima, fc_arimax, fc_arimax_lag)
fc_models

accuracy_models_clean <- bind_rows(
	accuracy(fc_arima, test),
	accuracy(fc_arimax, test),
	accuracy(fc_arimax_lag, test_lag |> drop_na(rate_lag, price_bc))
) |>
	select(.model, RMSE, MAE, MAPE, ACF1) |>
	arrange(RMSE)

accuracy_models_clean

fc_plot_full <- fc_models |>
	autoplot(df_model) +
	labs(
		title = "Forecast comparison: ARIMA vs ARIMAX models",
		x = "Quarter",
		y = "Box-Cox transformed housing price"
	)

ggsave("plots/02-02_forecast_compare_full.png", plot = fc_plot_full,
			 width = 12, height = 8, units = "cm")

fc_plot_test <- fc_models |>
	autoplot(df_model |> filter(qtr >= yearquarter("2018 Q1"))) +
	labs(
		title = "Forecast comparison on test period",
		x = "Quarter",
		y = "Box-Cox transformed housing price"
	)

ggsave("plots/02-03_forecast_compare_test.png", plot = fc_plot_test,
			 width = 12, height = 8, units = "cm")

png("plots/02-04_residuals_arima.png",
		width = 12, height = 8, units = "cm", res = 150)
fit_arima |> ggtime::gg_tsresiduals() |> print()
dev.off()

png("plots/02-05_residuals_arimax.png",
		width = 12, height = 8, units = "cm", res = 150)
fit_arimax |> ggtime::gg_tsresiduals() |> print()
dev.off()

png("plots/02-06_residuals_arimax_lag.png",
		width = 12, height = 8, units = "cm", res = 150)
fit_arimax_lag |> ggtime::gg_tsresiduals() |> print()
dev.off()

augment(fit_arima) |>
	features(.innov, ljung_box, lag = 8, dof = 1)
augment(fit_arimax) |>
	features(.innov, ljung_box, lag = 8, dof = 1)
augment(fit_arimax_lag) |>
	features(.innov, ljung_box, lag = 8, dof = 1)


