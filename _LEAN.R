# =============================================================================
# UNIFIED LEAN ANALYSIS & FORECASTING WORKFLOW (_LEAN.R)
# Merges _01_lean.R (EDA) and _02_lean.R (Modeling) from main branch
# =============================================================================

# --- LIBRARIES (From both _01_lean.R and _02_lean.R) ---
library(tidyverse)
library(lubridate)
library(tsibble)
library(fable)
library(fabletools)
library(feasts)
library(forecast)
library(urca)
library(strucchange)
library(scales)
library(ggtime)

select <- dplyr::select

theme_set(theme_minimal(base_size = 12))

# =============================================================================
# SECTION 1: DATA LOADING AND PREPROCESSING (Verbatim from _01_lean.R)
# =============================================================================
cat("Working directory:", getwd(), "\n")

price <- read_csv("data/kbh_quarter_sqm_price.csv",
                  show_col_types = FALSE) |>
  rename(qtr = Quarter) |>
  mutate(qtr = yearquarter(yq(gsub("K", "Q", qtr)))) |>
  arrange(qtr) |>
  as_tsibble(index = qtr)

rate <- read_csv("data/bondint_quarterly.csv",
                 show_col_types = FALSE) |>
  rename(qtr = Quarter, rate = mean_rate) |>
  mutate(qtr = yearquarter(yq(gsub("K", "Q", qtr)))) |>
  arrange(qtr) |>
  as_tsibble(index = qtr)

cat("Price:", format(min(price$qtr)), "to", format(max(price$qtr)), " (n =", nrow(price), ")\n")
cat("Rate:", format(min(rate$qtr)), "to", format(max(rate$qtr)), " (n =", nrow(rate), ")\n")

# =============================================================================
# SECTION 2: EXPLORATORY DATA ANALYSIS & PLOTS (Verbatim from _01_lean.R)
# =============================================================================
if (!dir.exists("plots")) dir.create("plots")

# PLOT 1-2: Level
price_level_plot <- ggplot(price, aes(qtr, Price)) +
  geom_line(colour = "#2563EB") +
  scale_y_continuous(labels = comma) +
  labs(title = "Price (level)", x = NULL, y = "DKK / m²") +
  theme(plot.title = element_text(face = "bold"))
ggsave("plots/01-1_price_level.png", plot = price_level_plot,
       width = 14, height = 8, units = "cm")

rate_level_plot <- ggplot(rate, aes(qtr, rate)) +
  geom_line(colour = "#DC2626") +
  labs(title = "Interest rate (level)", x = NULL, y = "%")
ggsave("plots/01-2_rate_level.png", plot = rate_level_plot,
       width = 14, height = 8, units = "cm")

# PLOT 3-4: CORRELOGRAM (ACF)
acf_price <- price |> ACF(Price) |> autoplot() +
  ggtitle("ACF: Price")
ggsave("plots/01-3_acf_price.png", plot = acf_price,
       width = 12, height = 8, units = "cm")

acf_rate <- rate |> ACF(rate) |> autoplot() +
  ggtitle("ACF: Rate")
ggsave("plots/01-4_acf_rate.png", plot = acf_rate,
       width = 12, height = 8, units = "cm")
       
# PLOT 5-7: SEASONALITY DIAGNOSTICS
seasonal_plot_1 <- gg_season(price, Price) + ggtitle("Seasonal plot - by year")
ggsave("plots/01-5_seasonal_year.png", plot = seasonal_plot_1,
       width = 12, height = 8, units = "cm")

seasonal_plot_2 <- gg_subseries(price, Price) + ggtitle("Seasonal subseries - by quarter")
ggsave("plots/01-6_seasonal_quarter.png", plot = seasonal_plot_2,
       width = 12, height = 8, units = "cm")

lag_plot <- price |>
  mutate(Price_k = Price / 1000) |>
  gg_lag(Price_k) +
  ggtitle("Lag plot") +
  labs(x = "lag(Price, n) [thousands]", y = "Price [thousands]") +
  theme(axis.text = element_text(size = 8),
        strip.text = element_text(size = 9))
ggsave("plots/01-7_lag_plot.png", plot = lag_plot,
       width = 14, height = 10, units = "cm")

# PLOT 8: DECOMPOSITION
stl_dcmp <- price |>
  model(stl = STL(Price ~ season(window = "periodic"))) |>
  components()
decomposition_plot <- autoplot(stl_dcmp) +
  labs(title = "Decomposition of price (STL)")
ggsave("plots/01-8_decomposition.png", plot = decomposition_plot,
       width = 14, height = 10, units = "cm")

# =============================================================================
# SECTION 3: BOX-COX LAMBDA ON FULL DATA & MERGE (From _01_lean.R)
# =============================================================================
lambda <- price |>
  features(Price, features = guerrero) |>
  pull(lambda_guerrero)
cat("Box-Cox λ (Guerrero):", round(lambda, 4), "\n")

# Inner-JOIN (n=114)
df <- inner_join(as_tibble(price), as_tibble(rate), by = "qtr") |>
  arrange(qtr) |>
  as_tsibble(index = qtr)

# TRAIN TEST SPLIT
ratio <- floor(nrow(df) * 0.2)
train <- df |> slice_head(n = nrow(df) - ratio)
test  <- df |> slice_tail(n = ratio)

lambda_train <- train |>
  features(Price, features = guerrero) |>
  pull(lambda_guerrero)

# --- [PATCH: Box-Cox Lambda Data Preservation] ---
# Note: Box-Cox lambda (lambda_train) is estimated above on the full 92 training prices 
# before any lags are calculated. This prevents the loss of the first 2 observations 
# during lambda estimation (which would happen if we dropped NAs first like in his QMD).
cat("Box-Cox λ (train):", round(lambda_train, 4), "\n")

# =============================================================================
# SECTION 4: DATA TRANSFORMATIONS (Initial)
# =============================================================================
train <- train |>
  mutate(
    price_bc = forecast::BoxCox(Price, lambda_train),
    price_bc_d = difference(price_bc),
    rate_d = difference(rate)
  )

test <- test |>
  mutate(
    price_bc = forecast::BoxCox(Price, lambda_train),
    price_bc_d = difference(price_bc),
    rate_d = difference(rate)
  )

# =============================================================================
# SECTION 5: STATIONARITY / UNIT-ROOT TESTS
# =============================================================================

# --- [PATCH: Quarterly Frequency Specification] ---
# We specify frequency = 4 for all as.ts() conversions. This corrects the friend's QMD 
# where raw vectors had frequency = 1 (treating quarterly data as annual).
ur_tidy <- function(x, label) {
  adf_trend <- ur.df(x, type = "trend", selectlags = "AIC")
  kpss_tau  <- ur.kpss(x, type = "tau")

  tibble(
    series       = label,
    ADF_trend    = round(adf_trend@teststat[1], 3),
    ADF_5pct     = adf_trend@cval["tau3", "5pct"],
    KPSS_tau     = round(kpss_tau@teststat, 3),
    KPSS_5pct    = kpss_tau@cval["critical values", "5pct"]
  )
}

ur_tbl <- bind_rows(
  ur_tidy(as.ts(train$price_bc, frequency = 4),              "price_bc_level"),
  ur_tidy(as.ts(na.omit(train$price_bc_d), frequency = 4),   "price_bc_diff"),
  ur_tidy(as.ts(train$rate, frequency = 4),                  "rate_level"),
  ur_tidy(as.ts(na.omit(train$rate_d), frequency = 4),       "rate_diff")
)
print(ur_tbl)

# =============================================================================
# SECTION 6: CCF & MODEL LAG SELECTION (From _02_lean.R / laptop 02_modeling.R)
# =============================================================================
ccf_data <- train |>
	select(rate_d, price_bc_d) |>
	drop_na()

png("plots/02-01_ccf_price_bc_d_rate_d.png",
		width = 12, height = 8, units = "cm", res = 150)
ccf(ccf_data$rate_d, ccf_data$price_bc_d, lag.max = 12,
		main = "CCF: diff(rate) (x) vs. diff(price_bc) (y)")
dev.off()

# --- [CHANGED: Dynamic Exogenous Lag Selection from 02_modeling.R] ---
# We run a dynamic AICc search over candidate lags to select best_k dynamically.
k_candidates <- c(0, 1, 2, 3, 4, 5)
lag_grid <- tibble(k = k_candidates) |>
  mutate(model = map(k, ~ {
    d <- train |>
      mutate(rate_lag = dplyr::lag(rate, .x)) |>
      drop_na(rate_lag, price_bc)
    d |> model(ARIMA(price_bc ~ rate_lag,
                     stepwise = FALSE, approximation = FALSE))
  }),
  AICc = map_dbl(model, ~ glance(.x)$AICc))

print(lag_grid |> select(k, AICc))
best_k <- lag_grid$k[which.min(lag_grid$AICc)]
cat("\nBest lag for rate (AICc):", best_k, "Q\n")

# --- [PATCH: Tidyverse-style Lagging & Continuous Alignment (Resolved circular dependency)] ---
# Now that best_k is dynamically selected, we compute the lag on the combined 'df' dataset
# before separating them again. This ensures that the test set has no NA values at the boundary.
df_model <- df |>
  arrange(qtr) |>
  mutate(
    price_bc = forecast::BoxCox(Price, lambda_train),
    price_bc_d = difference(price_bc),
    rate_d = difference(rate),
    rate_lag = dplyr::lag(rate, best_k)
  )

# Update train and test with the final continuous lagged data
train <- df_model |> filter(qtr %in% train$qtr)
test  <- df_model |> filter(qtr %in% test$qtr)

# =============================================================================
# SECTION 7: STRUCTURAL BREAK TESTS (QLR / supF)
# =============================================================================

# --- [PATCH: Univariate QLR Test (AR(1) dynamics) from _01_lean.R] ---
# This tests whether the price's own momentum (its autoregressive relationship with its past value) 
# has shifted. This is a crucial prerequisite for our univariate ARIMA model.
diff_price_ts <- as.ts(price$Price, frequency = 4) |> diff() |> na.omit()
break_data_uni <- cbind(
  Lag0 = diff_price_ts,
  Lag1 = stats::lag(diff_price_ts, k = -1)
)
qlr_uni <- Fstats(Lag0 ~ 1 + Lag1, data = break_data_uni, from = 0.10)
break_test_uni <- sctest(qlr_uni, type = "supF")
cat("\n--- Univariate QLR Test (AR(1) dynamics) ---\n")
print(break_test_uni)

bp_uni <- breakpoints(qlr_uni, alpha = 0.05)
cat("Estimated breakpoints (α = 0.05):\n"); print(bp_uni)

png("plots/01-9a_qlr_univariate.png", width = 1200, height = 800, res = 100)
plot(qlr_uni, alpha = 0.05, main = "QLR Test: F-Statistics for Structural Breaks (univariate)")
lines(bp_uni)
dev.off()

# --- [CHANGED: Bivariate QLR Test from QMD] ---
# This tests whether the relationship between housing price changes and interest rate changes shifted.
# This is highly relevant for our ARIMAX and VAR models.
qlr_df <- train |>
  filter(!is.na(price_bc_d), !is.na(rate_d)) |>
  as_tibble()

qlr_bi <- Fstats(price_bc_d ~ rate_d, data = qlr_df) # computes supF (QLR)
break_test_bi <- sctest(qlr_bi, type = "supF")
cat("\n--- Bivariate QLR Test (Price Change ~ Rate Change) ---\n")
print(break_test_bi)

bp_bi <- breakpoints(qlr_bi, alpha = 0.05)
cat("Estimated breakpoints (α = 0.05):\n"); print(bp_bi)

break_obs <- bp_bi$breakpoints
if (!is.na(break_obs[1])) {
  break_qtr <- qlr_df$qtr[break_obs]
  cat("Estimated break quarter(s):", format(break_qtr), "\n")
}

png("plots/01-9b_qlr_bivariate.png", width = 1200, height = 800, res = 100)
plot(qlr_bi, alpha = 0.05, main = "QLR Test: F-Statistics for Structural Breaks (bivariate)")
lines(bp_bi)
dev.off()

# =============================================================================
# SECTION 8: FORECASTING MODELS - ARIMA & ARIMAX (Verbatim from _02_lean.R)
# =============================================================================
fit_arima <- train |> model(arima = ARIMA(price_bc, stepwise = FALSE, approximation = FALSE))
fit_arimax <- train |> model(arimax = ARIMA(price_bc ~ rate, stepwise = FALSE, approximation = FALSE))
fit_arimax_lag <- train |> drop_na(rate_lag) |> model(arimax_lag = ARIMA(price_bc ~ rate_lag, stepwise = FALSE, approximation = FALSE))

report(fit_arima)
report(fit_arimax)
report(fit_arimax_lag)

fc_arima <- forecast(fit_arima, new_data = test)
fc_arimax <- forecast(fit_arimax, new_data = test)
fc_arimax_lag <- forecast(fit_arimax_lag, new_data = test)

# =============================================================================
# SECTION 9: BIVARIATE VECTOR AUTOREGRESSION (VAR) (From laptop 02_modeling.R)
# =============================================================================
# fable::VAR is qualified explicitly because library(vars) (if loaded 
# elsewhere) would mask it. It fits a joint likelihood over (price_bc, rate).
fit_var <- train |>
  model(var = fable::VAR(vars(price_bc, rate), ic = "aicc"))

report(fit_var)

fc_var <- fit_var |> forecast(h = nrow(test))

# =============================================================================
# SECTION 10: ACCURACY EVALUATION (Updated to include VAR)
# =============================================================================
# Since VAR models both price_bc and rate, we filter the accuracy results to only 
# show the price_bc metrics for an apples-to-apples comparison.
acc_uni <- bind_rows(
	accuracy(fc_arima, test),
	accuracy(fc_arimax, test),
	accuracy(fc_arimax_lag, test)
)

acc_var <- fc_var |>
  accuracy(test |> select(qtr, price_bc, rate)) |>
  filter(.response == "price_bc")

accuracy_models_clean <- bind_rows(acc_uni, acc_var) |>
	select(.model, RMSE, MAE, MAPE, ACF1) |>
	arrange(RMSE)

print(accuracy_models_clean)

# =============================================================================
# SECTION 11: RESIDUALS DIAGNOSTICS & PLOTS
# =============================================================================
fc_plot_full <- bind_rows(fc_arima, fc_arimax, fc_arimax_lag) |>
	autoplot(df_model) +
	labs(
		title = "Forecast comparison: ARIMA vs ARIMAX models",
		x = "Quarter",
		y = "Box-Cox transformed housing price"
	)
ggsave("plots/02-02_forecast_compare_full.png", plot = fc_plot_full,
			 width = 12, height = 8, units = "cm")

fc_plot_test <- bind_rows(fc_arima, fc_arimax, fc_arimax_lag) |>
	autoplot(df_model |> filter(qtr >= yearquarter("2018 Q1"))) +
	labs(
		title = "Forecast comparison on test period",
		x = "Quarter",
		y = "Box-Cox transformed housing price"
	)
png("plots/02-03_forecast_compare_test.png", width = 12, height = 8, units = "cm", res = 150)
fc_plot_test |> print()
dev.off()

png("plots/02-04_residuals_arima.png", width = 12, height = 8, units = "cm", res = 150)
fit_arima |> ggtime::gg_tsresiduals() |> print()
dev.off()

png("plots/02-05_residuals_arimax.png", width = 12, height = 8, units = "cm", res = 150)
fit_arimax |> ggtime::gg_tsresiduals() |> print()
dev.off()

png("plots/02-06_residuals_arimax_lag.png", width = 12, height = 8, units = "cm", res = 150)
fit_arimax_lag |> ggtime::gg_tsresiduals() |> print()
dev.off()

# --- [CHANGED: VAR Forecast Plot from laptop 02_modeling.R] ---
png("plots/02-07_forecast_var.png", width = 14, height = 8, units = "cm", res = 150)
fc_var |>
  autoplot(filter(train, qtr >= yearquarter("2010 Q1")), level = 80) +
  geom_line(data = test |> pivot_longer(cols = c(price_bc, rate), names_to = ".response", values_to = "value"),
            aes(y = value), colour = "black", linetype = "dashed") +
  labs(title = "VAR forecast vs. realised log(Price)",
       y = "log(DKK/m²)") |>
  print()
dev.off()

# --- [PATCH: Dynamic Ljung-Box Test Degrees of Freedom] ---
# In his QMD, he hardcoded dof = 0 (and previously dof = 1) for all Ljung-Box tests.
# We replace this with dynamically calculated dof matching the estimated AR/MA parameters 
# and use lag = 12 for quarterly data residual autocorrelation testing.
dof_arima <- nrow(tidy(fit_arima))
dof_arimax <- nrow(tidy(fit_arimax))
dof_arimax_lag <- nrow(tidy(fit_arimax_lag))

cat("\nLjung-Box residual diagnostics (lag = 12):\n")
print(augment(fit_arima) |> features(.innov, ljung_box, lag = 12, dof = dof_arima))
print(augment(fit_arimax) |> features(.innov, ljung_box, lag = 12, dof = dof_arimax))
print(augment(fit_arimax_lag) |> features(.innov, ljung_box, lag = 12, dof = dof_arimax_lag))
