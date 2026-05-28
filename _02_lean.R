# =============================================================================
# Modeling
# =============================================================================
library (tidyverse)
library(tsibble)
library (fable)
library (fabletools)
library (feasts)
library (urca)
library (strucchange)
library (scales)

train  <- read_rds("data/train.rds")
test   <- read_rds("data/test.rds")

# Rate leads price by 1 period: price_t uses rate_{t-1}.
train <- train |>
	mutate(rate_d = difference(rate))

test <- test |>
	mutate(rate_d = difference(rate))

train
test

cat("Train sample:", format(min(train$qtr)), "to", format(max(train$qtr)), " (n =", nrow(train), ")\n")
cat("Test sample:", format(min(test$qtr)), "to", format(max(test$qtr)), " (n =", nrow(test), ")\n")

# CCF: Δrate (x) vs. Δprice (y). Negative lags = rate leads price.
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

