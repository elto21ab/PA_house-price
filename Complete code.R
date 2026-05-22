# =============================================================================
# Exam project — Copenhagen housing prices & obligationsrenten
# Predictive Analytics (CDSCO1051C), CBS
# =============================================================================

# ── 0.  Packages ────────────────────────────────────────────────────────────
# install.packages(c("tidyverse","lubridate","tsibble","fable","fabletools",
#                    "feasts","urca","strucchange","vars","forecast","scales",
#                    "patchwork"))

library(tidyverse)
library(lubridate)
library(tsibble)
library(fable)
library(fabletools)
library(feasts)
library(urca)         # ADF / KPSS
library(strucchange)  # QLR / Chow / Fstats
library(vars)         # VAR + Granger
library(forecast)     # BoxCox.lambda
library(scales)

# `vars` and `MASS` (loaded by vars) both define select(), which masks
# dplyr::select() and breaks the |> select(...) calls below. Re-bind it.
select <- dplyr::select
filter <- dplyr::filter

theme_set(theme_minimal(base_size = 12))


# ── 0.1  Load & join data ───────────────────────────────────────────────────

df_price <- read_csv("kbh_quarter_sqm_price.csv", show_col_types = FALSE) |>
  mutate(date = yq(gsub("K", "Q", Quarter))) |>
  filter(!is.na(date)) |>
  arrange(date)

df_bond <- read_csv("bondInt_quarterly.csv", show_col_types = FALSE) |>
  mutate(date = yq(gsub("K", "Q", Quarter))) |>
  filter(!is.na(date)) |>
  arrange(date)

# Joined data: 1997 Q3 onwards (bond rate is the binding series).
df <- inner_join(df_price, df_bond, by = c("Quarter", "date")) |>
  mutate(qtr = yearquarter(date)) |>
  as_tsibble(index = qtr)

cat("Sample:", format(min(df$date)), "to", format(max(df$date)),
    "  n =", nrow(df), "\n")


# ── 0.2  Train / test split ─────────────────────────────────────────────────

train <- df |> filter(date <= as.Date("2019-12-31"))
test  <- df |> filter(date >  as.Date("2019-12-31"))

cat("Train:", nrow(train), " Test:", nrow(test), "\n")


# =============================================================================
# STEP 1 — Visual analysis
# =============================================================================

# Dual-axis plot: housing price on the left axis (blue), obligationsrente
# on the right axis (red). Both series are drawn on the same panel by
# linearly mapping the rate onto the price scale; the secondary axis
# inverts the transform to show real %-values.

price_min <- min(df$Price);     price_max <- max(df$Price)
rate_min  <- min(df$mean_rate); rate_max  <- max(df$mean_rate)

scale_rate   <- function(x) (x - rate_min) / (rate_max - rate_min) *
                            (price_max - price_min) + price_min
unscale_rate <- function(x) (x - price_min) / (price_max - price_min) *
                            (rate_max - rate_min) + rate_min

ggplot(df, aes(x = date)) +
  geom_line(aes(y = Price,                  colour = "Price per m²"),
            linewidth = 0.9) +
  geom_line(aes(y = scale_rate(mean_rate),  colour = "Obligationsrente"),
            linewidth = 0.9) +
  scale_y_continuous(
    name     = "DKK / m²",
    labels   = comma,
    sec.axis = sec_axis(~ unscale_rate(.),
                        name   = "Obligationsrente (%)",
                        labels = function(x) sprintf("%.1f%%", x))
  ) +
  scale_colour_manual(values = c("Price per m²"     = "#2563EB",
                                 "Obligationsrente" = "#DC2626")) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(title = "Copenhagen housing prices and obligationsrenten",
       x = NULL, colour = NULL) +
  theme(legend.position    = "bottom",
        axis.title.y.left  = element_text(colour = "#2563EB"),
        axis.text.y.left   = element_text(colour = "#2563EB"),
        axis.title.y.right = element_text(colour = "#DC2626"),
        axis.text.y.right  = element_text(colour = "#DC2626"))

# Quick numerical summary (look for variance changes / outliers)
summary(df[, c("Price", "mean_rate")])


# =============================================================================
# STEP 2 — Box-Cox suitability
# =============================================================================
# Lambda close to 0 → log transform; close to 1 → no transform.
# Rates can go negative (2015–2021) so Box-Cox is NOT appropriate for them.

lambda_price <- forecast::BoxCox.lambda(train$Price, method = "guerrero")
cat("Guerrero λ (price):", round(lambda_price, 3), "\n")

# If λ ∈ roughly (-0.25, 0.25), use log for interpretability.
# Save both versions so we can compare in Step 12.
train <- train |> mutate(log_price = log(Price),
                         bc_price  = box_cox(Price, lambda_price))
test  <- test  |> mutate(log_price = log(Price),
                         bc_price  = box_cox(Price, lambda_price))


# =============================================================================
# STEP 3 — Post-transformation visualisation
# =============================================================================

ggplot(train, aes(date, log_price)) +
  geom_line(colour = "#2563EB") +
  labs(title = "log(Price) — variance should look more stable",
       x = NULL, y = "log(DKK/m²)")


# =============================================================================
# STEP 4 — Decomposition of housing series
# =============================================================================
# STL on log(Price). Quarterly data, periodic seasonality.

stl_fit <- train |>
  model(STL(log_price ~ trend(window = 21) + season(window = "periodic"),
            robust = TRUE)) |>
  components()

autoplot(stl_fit) +
  labs(title = "STL decomposition of log(Price)")


# =============================================================================
# STEP 5 — ACF & PACF on housing
# =============================================================================
# Used here for DIAGNOSTICS / context, not strict ARIMA order identification
# (we let auto-ARIMA pick orders via AICc grid search in Step 11).

train |> ACF(log_price, lag_max = 24)  |> autoplot() +
  labs(title = "ACF of log(Price)")
train |> PACF(log_price, lag_max = 24) |> autoplot() +
  labs(title = "PACF of log(Price)")

# Also look at the first difference, since log price is clearly I(1).
train |> ACF(difference(log_price),  lag_max = 24) |> autoplot() +
  labs(title = "ACF of Δlog(Price)")
train |> PACF(difference(log_price), lag_max = 24) |> autoplot() +
  labs(title = "PACF of Δlog(Price)")


# =============================================================================
# STEP 6 — Cross-correlation (CCF) between Δlog(Price) and Δrate
# =============================================================================
# Look at correlations between price growth and rate changes at various lags.
# Positive lag k means: rate change today correlates with price change k
# quarters from now (i.e. rate leads price).

dlogp <- diff(train$log_price)
drate <- diff(train$mean_rate)
ccf(drate, dlogp, lag.max = 12,
    main = "CCF: Δrate (x) vs. Δlog(Price) (y)")


# =============================================================================
# STEP 7 — Stationarity tests on both series
# =============================================================================
# Use BOTH tests (different nulls):
#   ADF  H0: unit root         → small p ⇒ stationary
#   KPSS H0: stationarity      → small p ⇒ unit root
# If they disagree we report both honestly.

cat("\n--- 7.1  Housing series (log_price) ---\n")
summary(ur.df(train$log_price,  type = "drift", selectlags = "AIC"))
summary(ur.kpss(train$log_price, type = "mu"))

cat("\n--- ... and Δlog(Price) ---\n")
summary(ur.df(diff(train$log_price),  type = "drift", selectlags = "AIC"))
summary(ur.kpss(diff(train$log_price), type = "mu"))

cat("\n--- 7.2  Obligationsrente ---\n")
summary(ur.df(train$mean_rate,  type = "drift", selectlags = "AIC"))
summary(ur.kpss(train$mean_rate, type = "mu"))

cat("\n--- ... and Δrate ---\n")
summary(ur.df(diff(train$mean_rate),  type = "drift", selectlags = "AIC"))
summary(ur.kpss(diff(train$mean_rate), type = "mu"))


# =============================================================================
# STEP 8 — Structural break tests (QLR / supF)
# =============================================================================
# Tests for a single unknown break in the relationship Δlog(Price) ~ Δrate.
# We trim 15% at each end (default).

bp_data <- tibble(dlogp = diff(train$log_price),
                  drate = diff(train$mean_rate))

fs <- Fstats(dlogp ~ drate, data = bp_data, from = 0.15)
plot(fs, main = "QLR (supF) test — Δlog(Price) ~ Δrate")
sctest(fs)                          # supF test
break_idx <- which.max(fs$Fstats) + floor(0.15 * nrow(bp_data))
break_date <- train$date[break_idx + 1]   # +1 because we differenced
cat("Estimated break date:", format(break_date), "\n")


# =============================================================================
# STEP 9 — Re-check stationarity on a restricted post-break sample
# =============================================================================
# If a clear break is found (e.g. 2008-09 or 2020), refit ADF/KPSS on either
# the pre- or post-break sub-sample. Adjust the cutoff after you see Step 8.

post_break <- train |> filter(date >= as.Date("2010-01-01"))

cat("\n--- 9. Post-break ADF/KPSS on log(Price) ---\n")
summary(ur.df(post_break$log_price,  type = "drift", selectlags = "AIC"))
summary(ur.kpss(post_break$log_price, type = "mu"))


# =============================================================================
# STEP 10 — Lag specification for the exogenous variable
# =============================================================================
# Decide whether to use contemporaneous rate, lag 1Q, 2Q, or 4Q.
# Compare AICc of ARIMAX(log_price ~ rate at lag k) for k = 0, 1, 2, 4.

lag_grid <- tibble(k = c(0, 1, 2, 4)) |>
  mutate(model = map(k, ~ {
    d <- train |>
      mutate(rate_lag = lag(mean_rate, .x)) |>
      filter(!is.na(rate_lag))
    d |> model(ARIMA(log_price ~ rate_lag,
                     stepwise = FALSE, approximation = FALSE))
  }),
  AICc = map_dbl(model, ~ glance(.x)$AICc))

print(lag_grid |> select(k, AICc))
best_k <- lag_grid$k[which.min(lag_grid$AICc)]
cat("Best lag for rate:", best_k, "Q\n")


# =============================================================================
# STEP 11 — Estimate models on the untransformed log-price
# =============================================================================
# We let ARIMA() pick orders by AICc over a grid (stepwise = FALSE,
# approximation = FALSE = full search), matching the friend's approach.

# Add the chosen rate lag to the training set
train <- train |> mutate(rate_lag = lag(mean_rate, best_k))
test  <- test  |> mutate(rate_lag = lag(mean_rate, best_k))

fit_nt <- train |>
  filter(!is.na(rate_lag)) |>
  model(
    arima  = ARIMA(log_price,
                   stepwise = FALSE, approximation = FALSE),
    arimax = ARIMA(log_price ~ rate_lag,
                   stepwise = FALSE, approximation = FALSE)
  )

report(fit_nt |> select(arima))
report(fit_nt |> select(arimax))

# --- VAR via fable (bivariate: log_price + mean_rate). We qualify
# fable::VAR explicitly because library(vars) masks it with the
# lower-level vars::VAR which has a different argument signature. ---
fit_var <- train |>
  model(var = fable::VAR(vars(log_price, mean_rate), ic = "aicc"))
report(fit_var)


# ── 11.1  Residual diagnostics ──────────────────────────────────────────────
fit_nt |> select(arima)  |> gg_tsresiduals() + ggtitle("ARIMA residuals")
fit_nt |> select(arimax) |> gg_tsresiduals() + ggtitle("ARIMAX residuals")

# Ljung-Box (set lag = 8 for quarterly data, dof = number of estimated AR+MA terms)
augment(fit_nt) |>
  features(.innov, ljung_box, lag = 8)


# ── 11.2  Compare model specifications ──────────────────────────────────────
glance(fit_nt) |> select(.model, AICc, BIC)


# =============================================================================
# STEP 12 — Same models but using the Box-Cox transformed series
# =============================================================================
# fable lets us pass box_cox(Price, λ) directly inside the formula.

fit_bc <- train |>
  filter(!is.na(rate_lag)) |>
  model(
    arima_bc  = ARIMA(box_cox(Price, lambda_price),
                      stepwise = FALSE, approximation = FALSE),
    arimax_bc = ARIMA(box_cox(Price, lambda_price) ~ rate_lag,
                      stepwise = FALSE, approximation = FALSE)
  )

report(fit_bc |> select(arima_bc))
report(fit_bc |> select(arimax_bc))


# ── 12.1  Residual diagnostics ──────────────────────────────────────────────
fit_bc |> select(arima_bc)  |> gg_tsresiduals() + ggtitle("ARIMA (Box-Cox) residuals")
fit_bc |> select(arimax_bc) |> gg_tsresiduals() + ggtitle("ARIMAX (Box-Cox) residuals")


# ── 12.2  Compare transformed vs untransformed ──────────────────────────────
# Note: AICc is NOT comparable across different transformations of the
# dependent variable. So compare here via *out-of-sample* RMSE in Step 16
# rather than by AICc alone.
glance(fit_bc) |> select(.model, AICc, BIC)


# =============================================================================
# STEP 13 — Ljung-Box tests on residuals
# =============================================================================
# H0: residuals are white noise (no autocorrelation up to lag h).
# Want p > 0.05 → cannot reject white noise → model has captured the structure.

all_fits <- bind_cols(
  fit_nt |> as_tibble(),
  fit_bc |> as_tibble()
)

# Ljung-Box for every model
bind_rows(
  augment(fit_nt) |> features(.innov, ljung_box, lag = 8, dof = 0),
  augment(fit_bc) |> features(.innov, ljung_box, lag = 8, dof = 0)
)


# =============================================================================
# STEP 14 — Forecast holdout period using observed obligationsrente
# =============================================================================
# For ARIMAX / VAR you need future values of the exogenous regressor.
# We use the OBSERVED rate over 2020 Q1 – end of test as a "perfect-foresight"
# scenario. That isolates predictive power of the rate channel from forecast
# error in the rate itself.

future_x <- test |>
  filter(!is.na(rate_lag)) |>
  select(qtr, rate_lag)

fc_nt <- fit_nt |>
  forecast(new_data = future_x)

fc_bc <- fit_bc |>
  forecast(new_data = future_x)


# =============================================================================
# STEP 15 — Plot forecasts
# =============================================================================

# Plot the untransformed (log-price) forecasts vs. realised log(Price).
fc_nt |>
  autoplot(filter(train, date >= as.Date("2010-01-01")), level = 80) +
  autolayer(test, log_price, colour = "black", linetype = "dashed") +
  labs(title = "Forecasts vs. realised log(Price) — untransformed models",
       y = "log(DKK/m²)") +
  facet_wrap(~ .model)

# Plot the Box-Cox-based forecasts vs. realised Price.
# fable automatically back-transforms Box-Cox forecasts to the original
# DKK/m² scale, so we overlay the raw Price series (not bc_price).
fc_bc |>
  autoplot(filter(train, date >= as.Date("2010-01-01")), level = 80) +
  autolayer(test, Price, colour = "black", linetype = "dashed") +
  labs(title = "Forecasts vs. realised Price — Box-Cox models",
       y = "DKK / m²") +
  facet_wrap(~ .model)


# =============================================================================
# STEP 16 — Compare forecast accuracy
# =============================================================================
# Need to back-transform forecasts to the original DKK/m² scale to compute
# meaningful RMSE / MAE / MAPE. fable's forecast objects carry the transform.

acc_nt <- fc_nt |> accuracy(test |> mutate(log_price = log(Price)))
acc_bc <- fc_bc |> accuracy(test |> mutate(log_price = log(Price)))

# Pool into one table
bind_rows(acc_nt, acc_bc) |>
  select(.model, RMSE, MAE, MAPE, MASE) |>
  arrange(RMSE)


# =============================================================================
# STEP 17 — Select best model & discuss economic interpretability
# =============================================================================

# Coefficients of the best ARIMAX:
fit_nt |> select(arimax) |> tidy()


# =============================================================================
# End of script
# =============================================================================
