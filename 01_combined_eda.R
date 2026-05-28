# =============================================================================
# 01_combined_eda.R — Exploratory data analysis for both series
# Predictive Analytics, CBS
# =============================================================================
# Steps are organised by analytical step rather than by variable, so the same
# step on price and rate appears right after each other. Uses the full sample.
# =============================================================================

library(tidyverse)
library(lubridate)
library(tsibble)
library(feasts)
library(forecast)
library(urca)
library(strucchange)
library(scales)

theme_set(theme_minimal(base_size = 12))
cat("Working directory:", getwd(), "\n")


# =============================================================================
# 1. LOAD DATA
# =============================================================================

# Price data
price <- read_csv("data/kbh_quarter_sqm_price.csv",
                  show_col_types = FALSE) |>
  rename(qtr = Quarter) |>
  mutate(qtr = yearquarter(yq(gsub("K", "Q", qtr)))) |>
  arrange(qtr) |>
  as_tsibble(index = qtr)

# Rate data, aligned to price quarters
rate_raw <- read_csv("data/bondint_quarterly.csv",
                     show_col_types = FALSE) |>
  rename(qtr = Quarter, rate = mean_rate) |>
  mutate(qtr = yearquarter(yq(gsub("K", "Q", qtr)))) |>
  arrange(qtr) |>
  as_tsibble(index = qtr)

rate <- rate_raw |> filter(qtr %in% price$qtr)


# =============================================================================
# 2. SAMPLE SUMMARY
# =============================================================================

cat("Price sample:", format(min(price$qtr)), "to", format(max(price$qtr)),
    " (n =", nrow(price), ")\n")
cat("Rate sample: ", format(min(rate$qtr)),  "to", format(max(rate$qtr)),
    " (n =", nrow(rate), ")\n")


# =============================================================================
# 3. PLOT LEVELS — price, then rate
# =============================================================================

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


# =============================================================================
# 4. CORRELOGRAM (ACF) — price, then rate
# =============================================================================

acf_price <- price |> ACF(Price) |> autoplot() +
  ggtitle("ACF: Price")
ggsave("plots/01-3_acf_price.png", plot = acf_price,
       width = 12, height = 8, units = "cm")

acf_rate <- rate |> ACF(rate) |> autoplot() +
  ggtitle("ACF: Rate")
ggsave("plots/01-4_acf_rate.png", plot = acf_rate,
       width = 12, height = 8, units = "cm")


# =============================================================================
# 5. SEASONALITY DIAGNOSTICS  (price only — rate has no seasonal hypothesis)
# =============================================================================

seasonal_plot_1 <- gg_season(price, Price) + ggtitle("Seasonal plot — by year")
ggsave("plots/01-5_seasonal_year.png", plot = seasonal_plot_1,
       width = 12, height = 8, units = "cm")

seasonal_plot_2 <- gg_subseries(price, Price) + ggtitle("Seasonal subseries — by quarter")
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


# =============================================================================
# 6. DECOMPOSITION  (price only)
# =============================================================================

if (requireNamespace("seasonal", quietly = TRUE)) {
  seats_dcmp <- price |>
    model(seats = X_13ARIMA_SEATS(Price ~ x11())) |>
    components()
  decomposition_plot <- autoplot(seats_dcmp) +
    labs(title = "Decomposition of price (X-13ARIMA-SEATS)")
} else {
  message("Package 'seasonal' not installed; using STL.")
  stl_dcmp <- price |>
    model(stl = STL(Price ~ season(window = "periodic"))) |>
    components()
  decomposition_plot <- autoplot(stl_dcmp) +
    labs(title = "Decomposition of price (STL)")
}
ggsave("plots/01-8_decomposition.png", plot = decomposition_plot,
       width = 14, height = 10, units = "cm")


# =============================================================================
# 7. BOX-COX  (price only — rate can go negative so Box-Cox is not applicable)
# =============================================================================

lambda <- price |>
  features(Price, features = guerrero) |>
  pull(lambda_guerrero)

cat("Box-Cox λ (Guerrero):", round(lambda, 4), "\n")

# Apply transformation. If λ ≈ 0, use natural log for interpretability.
if (abs(lambda) < 0.001) {
  price <- price |> mutate(price_bc = log(Price))
} else {
  price <- price |> mutate(price_bc = (Price^lambda - 1) / lambda)
}


# =============================================================================
# 8. UNIT-ROOT TESTS — price (level + diff), then rate (level + diff)
# =============================================================================

ur_tidy <- function(x, label) {
  adf_trend <- ur.df(x, type = "trend", selectlags = "AIC")
  adf_drift <- ur.df(x, type = "drift", selectlags = "AIC")
  adf_none  <- ur.df(x, type = "none",  selectlags = "AIC")
  kpss_tau  <- ur.kpss(x, type = "tau")
  kpss_mu   <- ur.kpss(x, type = "mu")
  tibble(
    series    = label,
    ADF_trend = round(adf_trend@teststat[1], 3),
    ADF_drift = round(adf_drift@teststat[1], 3),
    ADF_none  = round(adf_none@teststat[1], 3),
    KPSS_tau  = round(kpss_tau@teststat, 3),
    KPSS_mu   = round(kpss_mu@teststat, 3)
  )
}

ur_tbl <- bind_rows(
  ur_tidy(as.ts(price$Price),                       "price_level"),
  ur_tidy(as.ts(na.omit(diff(price$Price))),        "price_diff1"),
  ur_tidy(as.ts(rate$rate),                         "rate_level"),
  ur_tidy(as.ts(na.omit(diff(rate$rate))),          "rate_diff1")
)
print(ur_tbl)


# =============================================================================
# 9. FIRST DIFFERENCES  (price_bc and rate)
# =============================================================================

price <- price |> mutate(price_bc_diff = difference(price_bc))
rate  <- rate  |> mutate(rate_diff      = difference(rate))


# =============================================================================
# 10. STRUCTURAL BREAK TEST (QLR / supF) — price-only AR(1) dynamics
# =============================================================================
# Tests whether the AR(1) dynamics of Δprice are stable.
# (The bivariate QLR on Δprice ~ Δrate belongs in the joint-modelling file.)

diff_price_ts <- as.ts(price$Price) |> diff() |> na.omit()
break_data <- cbind(
  Lag0 = diff_price_ts,
  Lag1 = stats::lag(diff_price_ts)
)

qlr <- Fstats(Lag0 ~ 1 + Lag1, data = break_data, from = 0.10)
break_test <- sctest(qlr, type = "supF")
print(break_test)

bp <- breakpoints(qlr, alpha = 0.05)
cat("\nEstimated breakpoints (α = 0.05):\n"); print(bp)

png("plots/01-9_qlr_fstats.png", width = 1200, height = 800, res = 100)
plot(qlr, alpha = 0.05, main = "F-Statistics for Structural Breaks (univariate)")
lines(bp)
dev.off()

if (break_test$p.value < 0.05) {
  cat("\n⚠ STRUCTURAL BREAK DETECTED (p =", round(break_test$p.value, 4), ")\n")
} else if (break_test$p.value < 0.10) {
  cat("\n⚠ WEAK EVIDENCE of break (p =", round(break_test$p.value, 4), ") — borderline\n")
} else {
  cat("\n✓ NO STRUCTURAL BREAKS (p =", round(break_test$p.value, 4), ")\n")
}


# =============================================================================
# 11. ACF / PACF OF DIFFERENCED SERIES — price, then rate
# =============================================================================
# These guide ARIMA(p, d, q) order identification in the modelling file.

acf_price_diff <- price |>
  ACF(price_bc_diff, lag_max = 24) |>
  autoplot() +
  labs(title = paste0("ACF: Δprice_bc (λ = ", round(lambda, 4), ", d = 1)"))
ggsave("plots/01-10_acf_price_diff.png", plot = acf_price_diff,
       width = 12, height = 8, units = "cm")

pacf_price_diff <- price |>
  PACF(price_bc_diff, lag_max = 24) |>
  autoplot() +
  labs(title = paste0("PACF: Δprice_bc (λ = ", round(lambda, 4), ", d = 1)"))
ggsave("plots/01-11_pacf_price_diff.png", plot = pacf_price_diff,
       width = 12, height = 8, units = "cm")

acf_rate_diff <- rate |>
  ACF(rate_diff, lag_max = 24) |>
  autoplot() +
  labs(title = "ACF: Δrate")
ggsave("plots/01-12_acf_rate_diff.png", plot = acf_rate_diff,
       width = 12, height = 8, units = "cm")

pacf_rate_diff <- rate |>
  PACF(rate_diff, lag_max = 24) |>
  autoplot() +
  labs(title = "PACF: Δrate")
ggsave("plots/01-13_pacf_rate_diff.png", plot = pacf_rate_diff,
       width = 12, height = 8, units = "cm")


# =============================================================================
# 12. SAVE CLEANED DATA FOR DOWNSTREAM FILES
# =============================================================================

if (!dir.exists("data")) dir.create("data")
write_rds(price, "data/price_clean.rds")   # incl. price_bc and price_bc_diff
write_rds(rate,  "data/rate_clean.rds")    # incl. rate_diff

cat("\n✓ Saved:\n")
cat("   data/price_clean.rds  (with price_bc and price_bc_diff)\n")
cat("   data/rate_clean.rds   (with rate_diff)\n")
