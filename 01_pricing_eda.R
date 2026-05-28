# =================================================================
# 01_pricing_eda.R — Exploratory data analysis & prep
# =================================================================
# install.packages("tidyverse")
#fpp3?
library(tidyverse)
library(lubridate)
library(tsibble)
library(forecast)
library(feasts)
library(urca)
library(scales)
library(strucchange)

# theme_set(theme_minimal(base_size = 8))
theme_set(
  theme_minimal(base_size = 10) + 
  theme_economist() +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  )
)

# Set working directory. U PROBABLY NEED TO CHANGE DIRECTORY!
# wd <- "/Users/e/Documents/_UNI/PA_house-price"
# setwd(wd)
cat("Working directory:", getwd(), "\n")

# ── Load price data ─────────────────────────────────────────────────────────
df <- read_csv("data/kbh_quarter_sqm_price.csv", show_col_types = FALSE) |>
  rename(qtr = Quarter) |>
  mutate(qtr = yearquarter(yq(gsub("K", "Q", qtr)))) |>
  arrange(qtr) |>
  as_tsibble(index = qtr)

# Estimate optimal Box-Cox lambda using Guerrero's method
lambda <- df |>
  features(Price, features = guerrero) |>
  pull(lambda_guerrero)

cat("Box-Cox λ (Guerrero):", lambda, "\n")

# VIZ LEVEL
price_level_plot <- ggplot(df, aes(qtr, Price)) +
  geom_line(colour = "#2563EB") +
  scale_y_continuous(labels = comma) +
  labs(title = "Price (level)",
       x = NULL, y = "DKK / m²") +
  theme(plot.title = element_text(face = "bold"))
ggsave("plots/01-1_price_level.png", plot = price_level_plot, width = 14, height = 8, units = "cm")

## Plot the autocorrelation function
correlgram_plot <- df |> ACF(Price) |> autoplot() + 
  ggtitle("Correlgram")
ggsave("plots/01-2_correlgram.png", plot = correlgram_plot, width = 12, height = 8, units = "cm")

## Plot seasonal patterns by year
seasonal_plot_1 <- gg_season(df, Price) + 
  ggtitle("Seasonal plot 1")
ggsave("plots/01-3_seasonal_1.png", plot = seasonal_plot_1, width = 12, height = 8, units = "cm")

## Plot seasonal subseries by quarter
seasonal_plot_2 <- gg_subseries(df, Price) + 
  ggtitle("Seasonal plot 2")
ggsave("plots/01-4_seasonal_2.png", plot = seasonal_plot_2, width = 12, height = 8, units = "cm")

## Plot the series against its lagged values
df_lag <- df |>
  mutate(Price_k = Price / 1000)

lag_plot <- gg_lag(df_lag, Price_k) + 
  ggtitle("Lag plot") +
  labs(x = "lag(Price, n) [thousands]", y = "Price [thousands]") +
  theme(
    axis.text = element_text(size = 8),
    strip.text = element_text(size = 9)
  )
ggsave("plots/01-5_lag_plot.png", plot = lag_plot, width = 14, height = 10, units = "cm")

## Decompose the series using X-13ARIMA-SEATS
if (requireNamespace("seasonal", quietly = TRUE)) {
  seats_dcmp <- df %>%
    model(seats = X_13ARIMA_SEATS(Price ~ x11())) %>%
    components()
  decomposition_plot <- autoplot(seats_dcmp) +
    labs(title = "Decomposition of price using X-13ARIMA-SEATS")
} else {
  message("Package 'seasonal' is not installed; using STL decomposition for plot 6.")
  stl_dcmp <- df %>%
    model(stl = STL(Price ~ season(window = "periodic"))) %>%
    components()
  decomposition_plot <- autoplot(stl_dcmp) +
    labs(title = "Decomposition of price using STL")
}
ggsave("plots/01-6_decomposition.png", plot = decomposition_plot, width = 14, height = 10, units = "cm")

# =================================================================
# UNIT-ROOT TESTS ON LEVELS and FIRST DIFFERENCE (d=1)
# =================================================================

ur_tidy <- function(x, label) {
  # ADF: trend, drift, none (AIC auto-selects lags)
  adf_trend <- ur.df(x, type = "trend", selectlags = "AIC")
  adf_drift <- ur.df(x, type = "drift", selectlags = "AIC")
  adf_none  <- ur.df(x, type = "none", selectlags = "AIC")
  
  # KPSS: tau, mu
  kpss_tau  <- ur.kpss(x, type = "tau")
  kpss_mu   <- ur.kpss(x, type = "mu")
  
  tibble(
    series      = label,
    ADF_trend   = round(adf_trend@teststat[1], 3),
    ADF_drift   = round(adf_drift@teststat[1], 3),
    ADF_none    = round(adf_none@teststat[1], 3),
    KPSS_tau    = round(kpss_tau@teststat, 3),
    KPSS_mu     = round(kpss_mu@teststat, 3)
  )
}

bind_rows(
  ur_tidy(as.ts(df$Price),              "levels"),
  ur_tidy(as.ts(na.omit(diff(df$Price))), "diff_1")
)

# =================================================================
# STRUCTURAL BREAK TEST (QLR / Quandt-Likelihood Ratio)
# =================================================================
# Test for parameter instability in differenced series
# Prepares data: Lag0 = current diff, Lag1 = lagged diff
diff_price_ts <- as.ts(df$Price) %>% diff() %>% na.omit()

break_data <- cbind(
  Lag0 = diff_price_ts,
  Lag1 = stats::lag(diff_price_ts)
)

# Compute recursive F-statistics (tests intercept & lag coef stability)
qlr <- Fstats(Lag0 ~ 1 + Lag1, data = break_data, from = 0.10)

cat("\n✓ STRUCTURAL BREAK TEST (Quandt-Likelihood Ratio):\n")

# supF test: H0 = no structural break
break_test <- sctest(qlr, type = "supF")
print(break_test)

# Estimate breakpoints (α=0.05)
bp <- breakpoints(qlr, alpha = 0.05)
cat("\nEstimated breakpoints (α=0.05):\n")
print(bp)

# Plot & interpret
png("plots/02-1_qlr_fstats.png", width = 1200, height = 800, res = 100)
plot(qlr, alpha = 0.05, main = "F-Statistics for Structural Breaks")
lines(bp)
dev.off()

# Interpretation
if (break_test$p.value < 0.05) {
  cat("\n⚠ STRUCTURAL BREAK DETECTED (p =", round(break_test$p.value, 4), ")\n")
  cat("   Model coefficients unstable. Consider regime-switching or sample split.\n")
} else if (break_test$p.value < 0.10) {
  cat("\n⚠ WEAK EVIDENCE of break (p =", round(break_test$p.value, 4), ") — borderline\n")
} else {
  cat("\n✓ NO STRUCTURAL BREAKS (p =", round(break_test$p.value, 4), ")\n")
}

# =================================================================
# I DON'T FULLY UNDERTSTAND THE QLR (ABOVE) TEST YET
# =================================================================

# Apply Box-Cox transformation & first difference
if (abs(lambda) < 0.001) {
  df <- df |>
    mutate(price_bc = log(Price))
} else {
  df <- df |>
    mutate(price_bc = (Price^lambda - 1) / lambda)
}

df <- df |>
  mutate(price_bc_diff = difference(price_bc))

cat("\n✓ Box-Cox transformation (λ=", round(lambda, 4), ") & differencing applied.\n")

# ── ACF / PACF: Box-Cox transformed & differenced (d=1) ──────────────────────
# ACF plot
acf_plot <- df |>
  ACF(price_bc_diff, lag_max = 24) |>
  autoplot() +
  labs(title = paste0("ACF: Price (BC λ=", round(lambda, 4), ", d=1)"))
ggsave("plots/02-2_acf_bc_diff.png", plot = acf_plot, width = 12, height = 8, units = "cm")

# PACF plot
pacf_plot <- df |>
  PACF(price_bc_diff, lag_max = 24) |>
  autoplot() +
  labs(title = paste0("PACF: Price (BC λ=", round(lambda, 4), ", d=1)"))
ggsave("plots/02-3_pacf_bc_diff.png", plot = pacf_plot, width = 12, height = 8, units = "cm")
