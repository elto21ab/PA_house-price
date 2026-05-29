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

# =============================================================================
# ARIMA and ARIMAX models
# =============================================================================

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


# =============================================================================
# VAR model (bivariate: differenced price_bc and differenced rate)
# =============================================================================
# Uses vars::VAR. Both series are I(1), so we fit the VAR on the first
# differences. Lag order is selected by SC (BIC).

# Build the VAR dataset (differenced series, no NAs)
var_data <- train |>
	filter(!is.na(price_bc_d), !is.na(rate_d)) |>
	as_tibble() |>
	select(price_bc_d, rate_d)

# --- VAR lag selection ---
lag_select <- vars::VARselect(var_data, lag.max = 8, type = "const")
print(lag_select$selection)

p_opt <- lag_select$selection["SC(n)"]
cat("\nSelected VAR lag order (SC):", p_opt, "\n")

# --- Fit the VAR ---
fit_var <- vars::VAR(var_data, p = p_opt, type = "const")
summary(fit_var)

# --- Residual diagnostics (Portmanteau / serial correlation test) ---
vars::serial.test(fit_var, lags.pt = 8, type = "PT.asymptotic")

# --- Forecast the test period ---
var_forecast <- predict(fit_var, n.ahead = nrow(test))

# Forecast is in DIFFERENCES; reconstruct the level by cumulative sum
# starting from the last observed train price
price_diff_fc <- var_forecast$fcst$price_bc_d[, "fcst"]
last_train_price <- tail(train$price_bc, 1)
price_bc_fc <- cumsum(c(last_train_price, price_diff_fc))[-1]

# --- VAR accuracy and joint comparison table ---
var_accuracy <- tibble(
	.model = "var",
	RMSE = sqrt(mean((price_bc_fc - test$price_bc)^2)),
	MAE  = mean(abs(price_bc_fc - test$price_bc)),
	MAPE = mean(abs((price_bc_fc - test$price_bc) / test$price_bc)) * 100
)

accuracy_all <- bind_rows(accuracy_models_clean, var_accuracy) |>
	arrange(RMSE)

print(accuracy_all)

# --- VAR forecast plot ---
var_plot <- tibble(
	qtr    = test$qtr,
	actual = test$price_bc,
	var_fc = price_bc_fc
) |>
	ggplot(aes(x = qtr)) +
	geom_line(aes(y = actual), colour = "black") +
	geom_line(aes(y = var_fc), colour = "steelblue", linetype = "dashed") +
	labs(
		title = "VAR forecast vs actual (test period)",
		x = "Quarter",
		y = "Box-Cox transformed housing price"
	)

ggsave("plots/02-07_var_forecast.png", plot = var_plot,
			 width = 12, height = 8, units = "cm")


# =============================================================================
# End of script
# =============================================================================
