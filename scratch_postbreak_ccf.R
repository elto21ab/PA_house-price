# =============================================================================
# POST-BREAK CCF ANALYSIS
# Step 1: Confirm break date via QLR test
# Step 2: Compute CCF only on post-break training data
# Step 3: AICc grid search on post-break subsample to confirm lag
# Step 4: Fit optimal ARIMAX on full training data with break dummy
# Step 5: Compare out-of-sample accuracy against best current models
# =============================================================================
library(tidyverse)
library(tsibble)
library(fable)
library(fabletools)
library(feasts)
library(forecast)
library(strucchange)

price_raw <- read_csv("data/kbh_quarter_sqm_price.csv", show_col_types = FALSE)
rate_raw  <- read_csv("data/bondint_quarterly.csv", show_col_types = FALSE)

price <- price_raw |>
  mutate(qtr = yearquarter(str_replace(Quarter, "K", " Q")), Price = as.numeric(Price)) |>
  select(qtr, Price) |> arrange(qtr) |> as_tsibble(index = qtr)

rate <- rate_raw |>
  mutate(qtr = yearquarter(str_replace(Quarter, "K", " Q")), rate = as.numeric(mean_rate)) |>
  select(qtr, rate) |> arrange(qtr) |> as_tsibble(index = qtr)

df <- inner_join(price, rate, by = "qtr") |> arrange(qtr) |> as_tsibble(index = qtr)

lambda_train <- 0.3226227

df_model <- df |>
  arrange(qtr) |>
  mutate(
    price_bc   = forecast::BoxCox(Price, lambda_train),
    price_bc_d = difference(price_bc),
    rate_d     = difference(rate)
  )

# Train/test split (same as _LEAN.R)
ratio     <- floor(nrow(df) * 0.2)
train_all <- df_model |> slice_head(n = nrow(df) - ratio)
test      <- df_model |> slice_tail(n = ratio)

cat("Training period:", format(min(train_all$qtr)), "to", format(max(train_all$qtr)), "\n")
cat("Test period    :", format(min(test$qtr)), "to", format(max(test$qtr)), "\n\n")

# =============================================================================
# STEP 1: Confirm structural break (QLR test on full training data)
# =============================================================================
cat("--- Step 1: QLR Test on Full Training Data ---\n")
qlr_df <- train_all |>
  filter(!is.na(price_bc_d), !is.na(rate_d)) |>
  as_tibble()

qlr_bi <- Fstats(price_bc_d ~ rate_d, data = qlr_df)
bp     <- breakpoints(qlr_bi, alpha = 0.05)
break_qtr <- qlr_df$qtr[bp$breakpoints]
cat("Structural break detected at:", format(break_qtr), "\n\n")

# =============================================================================
# STEP 2: Post-break CCF
# =============================================================================
cat("--- Step 2: Post-Break CCF (training data after break only) ---\n")
train_post <- train_all |> filter(qtr > break_qtr)
cat("Post-break training window:", format(min(train_post$qtr)),
    "to", format(max(train_post$qtr)),
    "(n =", nrow(train_post), ")\n\n")

ccf_full <- train_all |>
  filter(!is.na(price_bc_d), !is.na(rate_d)) |>
  as_tibble()

ccf_post <- train_post |>
  filter(!is.na(price_bc_d), !is.na(rate_d)) |>
  as_tibble()

# Full-sample CCF (reference)
png("plots/03-01_ccf_fullsample.png", width = 1200, height = 700, res = 150)
ccf(ccf_full$rate_d, ccf_full$price_bc_d, lag.max = 12,
    main = paste0("CCF: diff(rate) vs diff(price_bc) — FULL training sample (",
                  format(min(train_all$qtr)), " to ", format(max(train_all$qtr)), ")"))
dev.off()

# Post-break CCF
png("plots/03-02_ccf_postbreak.png", width = 1200, height = 700, res = 150)
ccf(ccf_post$rate_d, ccf_post$price_bc_d, lag.max = 12,
    main = paste0("CCF: diff(rate) vs diff(price_bc) — POST-BREAK (",
                  format(min(train_post$qtr)), " to ", format(max(train_post$qtr)), ")"))
dev.off()
cat("CCF plots saved to plots/03-01 and plots/03-02\n\n")

# Print raw CCF values at negative lags for inspection
cat("Raw CCF values at negative lags (past rate -> current price):\n")
ccf_vals <- ccf(ccf_post$rate_d, ccf_post$price_bc_d, lag.max = 12, plot = FALSE)
n_post <- nrow(ccf_post)
ci_bound <- qnorm(0.975) / sqrt(n_post)
cat("95% CI bound (±):", round(ci_bound, 3), "\n")
for (k in -1:-12) {
  idx   <- which(ccf_vals$lag == k)
  val   <- round(ccf_vals$acf[idx], 3)
  sig   <- if (abs(val) > ci_bound) "  *** SIGNIFICANT" else ""
  cat(sprintf("  lag = %3d:  CCF = %6.3f%s\n", k, val, sig))
}

# =============================================================================
# STEP 3: AICc grid search on POST-BREAK training subsample
# =============================================================================
cat("\n--- Step 3: AICc Grid Search on Post-Break Subsample ---\n")
k_candidates <- 0:6
lag_grid_post <- tibble(k = k_candidates) |>
  mutate(
    model = map(k, ~ {
      d <- train_post |>
        mutate(rate_lag = dplyr::lag(rate, .x)) |>
        drop_na(rate_lag, price_bc)
      d |> model(ARIMA(price_bc ~ pdq(d = 1) + rate_lag,
                       stepwise = FALSE, approximation = FALSE))
    }),
    AICc = map_dbl(model, ~ glance(.x)$AICc)
  )

print(lag_grid_post |> select(k, AICc))
# AICc grid ties k=2, 5, 6 at ~113 — but post-break CCF shows ONLY k=1 and k=2
# are statistically significant. k=5 (CCF=-0.183) and k=6 (CCF=-0.021) are
# within the confidence bands. We therefore override the numerical argmin and
# use k=2 as the economically and statistically justified choice.
best_k_post_aicc <- lag_grid_post$k[which.min(lag_grid_post$AICc)]
best_k_post <- 2   # overriding k=6 (tied AICc) with k=2 (only CCF-significant lag at this distance)
cat("AICc best lag     :", best_k_post_aicc, "Q  (tied with k=2 and k=5 at AICc=113)\n")
cat("CCF-justified lag :", best_k_post, "Q  (only lags 1 and 2 are significant post-break)\n\n")

# =============================================================================
# STEP 4: Fit models using post-break-informed lag
# =============================================================================
cat("--- Step 4: Fit Models Using Post-Break Lag =", best_k_post, "---\n")

# IMPORTANT FIX: compute lag on FULL df_model before splitting.
# If we lag inside the test set alone, the first best_k_post rows become NA.
df_model_lagged <- df_model |>
  arrange(qtr) |>
  mutate(
    rate_lag_pb    = dplyr::lag(rate, best_k_post),
    break_dummy    = if_else(qtr > break_qtr, 1, 0),
    rate_lag_inter = rate_lag_pb * break_dummy
  )

train_final <- df_model_lagged |> filter(qtr %in% train_all$qtr)
test_final  <- df_model_lagged |> filter(qtr %in% test$qtr)

# Model A: Full sample, post-break lag, NO dummy (baseline comparison)
fit_arimax_pb_nodummy <- train_final |> drop_na(rate_lag_pb) |>
  model(m = ARIMA(price_bc ~ pdq(d = 1) + rate_lag_pb,
                  stepwise = FALSE, approximation = FALSE))

# Model B: Full sample, post-break lag, WITH dummy (the principled approach)
fit_arimax_pb_dummy <- train_final |> drop_na(rate_lag_pb) |>
  model(m = ARIMA(price_bc ~ pdq(d = 1) + rate_lag_pb + break_dummy + rate_lag_inter,
                  stepwise = FALSE, approximation = FALSE))

cat("Model A (no dummy) specification:\n"); report(fit_arimax_pb_nodummy)
cat("\nModel B (with dummy) specification:\n"); report(fit_arimax_pb_dummy)

# =============================================================================
# STEP 5: Compare out-of-sample accuracy (on original DKK/m2 scale)
# =============================================================================
cat("\n--- Step 5: Out-of-Sample Forecast Accuracy (DKK/m²) ---\n")

inv_bc <- function(x) forecast::InvBoxCox(x, lambda_train)
inv_log <- function(x) exp(x)
actual  <- test$Price

calc_m <- function(pred, name) {
  e <- actual - pred
  tibble(.model = name,
         RMSE = round(sqrt(mean(e^2)), 0),
         MAE  = round(mean(abs(e)), 0),
         MAPE = round(mean(abs(e / actual)) * 100, 2))
}

# Reference models (from scratch_compare_22_models results)
# Re-fit for clean comparison
price_df       <- price |> mutate(price_bc = forecast::BoxCox(Price, lambda_train),
                                   y_log = log(Price))
p_train_full   <- price_df |> filter(qtr < yearquarter("2020 Q3"))
p_test_full    <- price_df |> filter(qtr >= yearquarter("2020 Q3"))

fit_arima_base     <- train_all |> model(m = ARIMA(price_bc, stepwise=F, approximation=F))
fit_arima_full_bc  <- p_train_full |> model(m = ARIMA(price_bc, stepwise=F, approximation=F))
fit_arima_full_log <- p_train_full |> model(m = ARIMA(y_log, stepwise=F, approximation=F))

fc_base     <- forecast(fit_arima_base, new_data = test)
fc_full_bc  <- forecast(fit_arima_full_bc, new_data = p_test_full)
fc_full_log <- forecast(fit_arima_full_log, new_data = p_test_full)
fc_pb_nd    <- forecast(fit_arimax_pb_nodummy, new_data = test_final)
fc_pb_d     <- forecast(fit_arimax_pb_dummy, new_data = test_final)

results <- bind_rows(
  calc_m(inv_bc(fc_base$.mean),    "ARIMA (Base, Box-Cox)"),
  calc_m(inv_bc(fc_full_bc$.mean), "ARIMA (Full History, Box-Cox)"),
  calc_m(inv_log(fc_full_log$.mean), "ARIMA (Full History, Log)  *** current best"),
  calc_m(inv_bc(fc_pb_nd$.mean),   paste0("ARIMAX Lag", best_k_post, " (Post-Break CCF, no dummy)")),
  calc_m(inv_bc(fc_pb_d$.mean),    paste0("ARIMAX Lag", best_k_post, " (Post-Break CCF + dummy)"))
) |> arrange(RMSE)

print(results)
