# =================================================================
# 01_pricing_eda.R — Exploratory data analysis & prep
# =================================================================
library(tidyverse)
library(lubridate)
library(tsibble)
library(forecast)
library(feasts)
library(urca)
library(scales)

theme_set(theme_minimal(base_size = 12))

# setwd("")
getwd()

# ── Load price data ─────────────────────────────────────────────────────────
df <- read_csv("data/kbh_quarter_sqm_price.csv", show_col_types = FALSE) |>
  rename(qtr = Quarter) |>
  mutate(qtr = yearquarter(yq(gsub("K", "Q", qtr)))) |>
  arrange(qtr) |>
  as_tsibble(index = qtr)

# ── PLOT #1: Price level ─────────────────────────────────────────────────────────
ggplot(df, aes(qtr, Price)) +
  geom_line(colour = "#2563EB") +
  scale_y_continuous(labels = comma) +
  labs(title = "Price (level)",
       x = NULL, y = "DKK / m²") +
  theme(plot.title = element_text(face = "bold"))

ggsave("plots/01-1_price_level.png", width = 14, height = 8, units = "cm")


# ── 0.2  Train / test split ─────────────────────────────────────────────────
train <- df |> filter(qtr <= yearquarter("2019 Q4"))
test  <- df |> filter(qtr >  yearquarter("2019 Q4"))

# Estimate optimal Box-Cox lambda using Guerrero's method
lambda <- train |>
  features(Price, features = guerrero) |>
  pull(lambda_guerrero)

# Apply Box-Cox transformation
train <- train |>
  mutate(price_transformed = box_cox(Price, lambda))

### SANITY CHECK
cat("\n--- TRAIN/TEST SPLIT ---\nTrain n =", nrow(train), "| Test n =", nrow(test), "\n")
cat(lambda)
train
### SANITY CHECK

# Calculate volatility metrics
vol_raw <- sd(diff(train$Price), na.rm = TRUE)
vol_transformed <- sd(diff(train$price_transformed), na.rm = TRUE)
vol_reduction <- (1 - vol_transformed / vol_raw) * 100

# ── Summary statistics ──────────────────────────────────────────────────────────
sprintf("\n--- HOUSING PRICE SERIES (%s – %s | n = %d) ---\nRange: %d – %d DKK/m²\n  Min @ %s | Max @ %s\nTotal growth in period: %.1f%%\nBox-Cox λ (Guerrero): %.4f\nVolatility (raw): %.3f | (transformed): %.3f | Reduction: %.5f%%\n",
        format(min(train$qtr)), format(max(train$qtr)), nrow(train),
        min(train$Price), max(train$Price),
        format(train$qtr[which.min(train$Price)]), format(train$qtr[which.max(train$Price)]),
        (max(train$Price) - min(train$Price)) / min(train$Price) * 100,
        lambda, vol_raw, vol_transformed, vol_reduction) |> cat()

# ── PLOT #2a: Transformed price level & non-differenced ──────────────────────────────────────
ggplot(train, aes(qtr, price_transformed)) +
  geom_line(colour = "#2563EB") +
  labs(title = paste0("Price (transformed, λ = ", round(lambda, 4), ")"),
       x = NULL, y = "Transformed price") +
  theme(plot.title = element_text(face = "bold"))

ggsave("plots/01-2a_price_trans.png", width = 14, height = 8, units = "cm")

# =================================================================
# DIFFERENCING ORDER SELECTION (d = 0, 1, 2), ADF & KPSS comparison
# =================================================================
# Prepare series for each d
series_d0 <- train$price_transformed
series_d1 <- diff(train$price_transformed)
series_d2 <- diff(train$price_transformed, differences = 2)

# ADF tests
adf_d0 <- ur.df(series_d0, type = "drift", selectlags = "AIC")
adf_d1 <- ur.df(series_d1, type = "drift", selectlags = "AIC")
adf_d2 <- ur.df(series_d2, type = "drift", selectlags = "AIC")

# KPSS tests
kpss_d0 <- ur.kpss(series_d0, type = "mu")
kpss_d1 <- ur.kpss(series_d1, type = "mu")
kpss_d2 <- ur.kpss(series_d2, type = "mu")

# Create summary table
results <- tibble(
  d = 0:2,
  adf_stat = c(adf_d0@teststat[1], adf_d1@teststat[1], adf_d2@teststat[1]),
  adf_cv_5pct = c(adf_d0@cval[2, 2], adf_d1@cval[2, 2], adf_d2@cval[2, 2]),
  adf_reject = adf_stat < adf_cv_5pct,
  kpss_stat = c(kpss_d0@teststat, kpss_d1@teststat, kpss_d2@teststat),
  kpss_cv_5pct = c(kpss_d0@cval[2], kpss_d1@cval[2], kpss_d2@cval[2]),
  kpss_reject = kpss_stat < kpss_cv_5pct
)

cat("Lags selected (AIC):\n")
cat("d=0:", adf_d0@lags, "lags\n")
cat("d=1:", adf_d1@lags, "lags\n")
cat("d=2:", adf_d2@lags, "lags\n")

cat("Results (5% significance level):\n")
results

# Decide on d: both ADF & KPSS should agree
best_d <- results |>
  filter(adf_reject & kpss_reject) |>
  slice(1) |>
  pull(d)

if (is.na(best_d)) {
  best_d <- results |> filter(adf_reject) |> slice(1) |> pull(d)
  print("⚠ WARNING: No consensus between ADF & KPSS. Using ADF result.")
}

cat("\n✓ DECISION: d =", best_d)

# =================================================================
# STATIONARITY CONFIRMATION
# =================================================================
cat("\nSTATIONARITY CONFIRMATION (d=", best_d, ")\n", sep = "")

# Get differenced series for chosen d
diff_price <- switch(best_d + 1,
  train$price_transformed,  # d=0
  diff(train$price_transformed),  # d=1
  diff(train$price_transformed, differences = 2)  # d=2
)

cat("\nADF test (H0: unit root):\n")
summary(ur.df(diff_price, type = "none", selectlags = "AIC"))

cat("\nKPSS test (H0: stationarity):\n")
summary(ur.kpss(diff_price, type = "mu"))


# ── PLOT #2b: Transformed price level & differenced ──────────────────────────────────────
train_diff <- train |>
  mutate(price_diff = c(NA, diff(price_transformed))) |>
  drop_na(price_diff)

ggplot(train_diff, aes(qtr, price_diff)) +
  geom_line(colour = "#2563EB") +
  labs(title = paste0("Price (transformed, d=1, λ = ", round(lambda, 4), ")"),
       x = NULL, y = "Differenced price") +
  theme(plot.title = element_text(face = "bold"))

ggsave("plots/01-2b_price_trans_diff.png", width = 14, height = 8, units = "cm")


# ── ACF / PACF for d=1 (differenced) ──────────────────────────────────────────
# Get differenced series for chosen d
diff_price <- switch(best_d + 1,
  train$price_transformed,  # d=0
  diff(train$price_transformed),  # d=1
  diff(train$price_transformed, differences = 2)  # d=2
)

train |> ACF(difference(price_transformed, differences = best_d), lag_max = 24) |> autoplot() +
  labs(title = paste0("ACF: Δ price (d=", best_d, ")"))
ggsave(paste0("plots/01-3_price_trans_diff_acf", best_d, ".png"), width = 12, height = 8, units = "cm")

train |> PACF(difference(price_transformed, differences = best_d), lag_max = 24) |> autoplot() +
  labs(title = paste0("PACF: Δ price (d=", best_d, ")"))
ggsave(paste0("plots/01-4_price_trans_diff_pacf", best_d, ".png"), width = 12, height = 8, units = "cm")
