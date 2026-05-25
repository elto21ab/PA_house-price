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
  mutate(qtr = yearquarter(yq(gsub("K", "Q", Quarter)))) |>
  arrange(qtr) |>
  as_tsibble(index = qtr)

# Estimate optimal Box-Cox lambda using Guerrero's method
lambda <- df |>
  features(Price, features = guerrero) |>
  pull(lambda_guerrero)

# Apply Box-Cox transformation
df <- df |>
  mutate(price_transformed = box_cox(Price, lambda))

# Calculate volatility metrics
vol_raw <- sd(diff(df$Price), na.rm = TRUE)
vol_transformed <- sd(diff(df$price_transformed), na.rm = TRUE)
vol_reduction <- (1 - vol_transformed / vol_raw) * 100

# ── Summary statistics ──────────────────────────────────────────────────────────
sprintf("\n--- HOUSING PRICE SERIES (%s – %s | n = %d) ---\nRange: %d – %d DKK/m²\n  Min @ %s | Max @ %s\nTotal growth in period: %.1f%%\nBox-Cox λ (Guerrero): %.4f\nVolatility (raw): %.3f | (transformed): %.3f | Reduction: %.5f%%\n",
        format(min(df$qtr)), format(max(df$qtr)), nrow(df),
        min(df$Price), max(df$Price),
        format(df$qtr[which.min(df$Price)]), format(df$qtr[which.max(df$Price)]),
        (max(df$Price) - min(df$Price)) / min(df$Price) * 100,
        lambda, vol_raw, vol_transformed, vol_reduction) |> cat()

# ── Price level (raw) ─────────────────────────────────────────────────────────
ggplot(df, aes(qtr, Price)) +
  geom_line(colour = "#2563EB") +
  scale_y_continuous(labels = comma) +
  labs(title = "Price (level)",
       x = NULL, y = "DKK / m²") +
  theme(plot.title = element_text(face = "bold"))

ggsave("plots/01-1_price_level.png", width = 14, height = 8, units = "cm")

# ── Transformed price level & differenced ──────────────────────────────────────
ggplot(df, aes(qtr, price_transformed)) +
  geom_line(colour = "#2563EB") +
  labs(title = paste0("Price (Box-Cox transformed, λ = ", round(lambda, 4), ")"),
       x = NULL, y = "Transformed price") +
  theme(plot.title = element_text(face = "bold"))

ggsave("plots/01-2_price_transformed.png", width = 14, height = 8, units = "cm")

# =================================================================
# DIFFERENCING ORDER SELECTION (d = 0, 1, 2), ADF & KPSS comparison
# =================================================================
# Prepare series for each d
series_d0 <- df$price_transformed
series_d1 <- diff(df$price_transformed)
series_d2 <- diff(df$price_transformed, differences = 2)

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

# ── ACF / PACF for d=1 (differenced) ──────────────────────────────────────────
# Get differenced series for chosen d
diff_price <- switch(best_d + 1,
  df$price_transformed,  # d=0
  diff(df$price_transformed),  # d=1
  diff(df$price_transformed, differences = 2)  # d=2
)

df |> ACF(difference(price_transformed, differences = best_d), lag_max = 24) |> autoplot() +
  labs(title = paste0("ACF: Δ price (d=", best_d, ")"))
ggsave(paste0("plots/01-3_acf_d", best_d, ".png"), width = 12, height = 8, units = "cm")

df |> PACF(difference(price_transformed, differences = best_d), lag_max = 24) |> autoplot() +
  labs(title = paste0("PACF: Δ price (d=", best_d, ")"))
ggsave(paste0("plots/01-4_pacf_d", best_d, ".png"), width = 12, height = 8, units = "cm")
# =================================================================
# STATIONARITY CONFIRMATION
# =================================================================
cat("STATIONARITY CONFIRMATION (d=", best_d, ")", sep = "")

cat("ADF test (H0: unit root):\n")
summary(ur.df(diff_price, type = "none", selectlags = "AIC"))

cat("\nKPSS test (H0: stationarity):\n")
summary(ur.kpss(diff_price, type = "mu"))
