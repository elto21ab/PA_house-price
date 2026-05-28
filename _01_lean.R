# =============================================================================
# EDA
# =============================================================================
library(tidyverse)
library(lubridate)
library(tsibble)
library(feasts)
library(forecast)
library(urca)
library(strucchange)
library(scales)

cat("Working directory:", getwd(), "\n")

price <- read_csv("data/kbh_quarter_sqm_price.csv",
                  show_col_types = FALSE) |>
  rename(qtr = Quarter) |>
  mutate(qtr = yearquarter(yq(gsub("K", "Q", qtr)))) |>
  arrange(qtr) |>
  as_tsibble(index = qtr)
price

rate <- read_csv("data/bondint_quarterly.csv",
                     show_col_types = FALSE) |>
  rename(qtr = Quarter, rate = mean_rate) |>
  mutate(qtr = yearquarter(yq(gsub("K", "Q", qtr)))) |>
  arrange(qtr) |>
  as_tsibble(index = qtr)
rate

cat("Price:", format(min(price$qtr)), "to", format(max(price$qtr)), " (n =", nrow(price), ")\n")
cat("Rate:", format(min(rate$qtr)), "to", format(max(rate$qtr)), " (n =", nrow(rate), ")\n")

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
       
# PLOT 5-7: SEASONALITY DIAGNOSTICS  (price only)
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

# PLOT 8: DECOMPOSITION  (price only)
stl_dcmp <- price |>
  model(stl = STL(Price ~ season(window = "periodic"))) |>
  components()
decomposition_plot <- autoplot(stl_dcmp) +
  labs(title = "Decomposition of price (STL)")
ggsave("plots/01-8_decomposition.png", plot = decomposition_plot,
       width = 14, height = 10, units = "cm")

# BOX-COX. If abs(λ)>0.2 ==> transform. Else, use natural log.
lambda <- price |>
  features(Price, features = guerrero) |>
  pull(lambda_guerrero)
cat("Box-Cox λ (Guerrero):", round(lambda, 4), "\n")
# RESULT: λ ≈ -0.148 → JUST USE REGULAR LOG

# UNIT-ROOT TESTS — price (level + diff), then rate (level + diff)
# ur_tidy <- function(x, label) {
#   adf_trend <- ur.df(x, type = "trend", selectlags = "AIC")
#   adf_drift <- ur.df(x, type = "drift", selectlags = "AIC")
#   adf_none  <- ur.df(x, type = "none",  selectlags = "AIC")
#   kpss_tau  <- ur.kpss(x, type = "tau")
#   kpss_mu   <- ur.kpss(x, type = "mu")
#   tibble(
#     series    = label,
#     ADF_trend = round(adf_trend@teststat[1], 3),
#     ADF_drift = round(adf_drift@teststat[1], 3),
#     ADF_none  = round(adf_none@teststat[1], 3),
#     KPSS_tau  = round(kpss_tau@teststat, 3),
#     KPSS_mu   = round(kpss_mu@teststat, 3)
#   )
# }
# 5% critical values:
#   ADF_trend  : -3.43
#   ADF_drift  : -2.89
#   ADF_none   : -1.95
#   KPSS_tau   :  0.146
#   KPSS_mu    :  0.463
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
  ur_tidy(as.ts(price$Price),                       "price_level"),
  ur_tidy(as.ts(na.omit(diff(price$Price))),        "price_diff1"),
  ur_tidy(as.ts(rate$rate),                         "rate_level"),
  ur_tidy(as.ts(na.omit(diff(rate$rate))),          "rate_diff1")
)
print(ur_tbl)

# RESULTS:
# price_level: ADF_trend −0.754 not negative enough → fail to reject unit root.
    # KPSS_tau 0.306 > ~0.146 → reject stationarity. ⇒ Non‑stationary.
# price_diff1:  ADF_trend −5.50 < critical → reject unit root.
    # KPSS_tau 0.098 < ~0.146 → fail to reject stationarity. ⇒ Stationary.
# rate_level: ADF_trend −1.80 not enough; KPSS_tau 0.253 > ~0.146. ⇒ Non‑stationary.
# rate_diff1: ADF_trend −7.25 strong reject; KPSS_tau 0.06 < ~0.146. ⇒ Stationary.



######################
# STRUCTURAL BREAK TEST (QLR / supF) — price-only AR(1) dynamics

diff_price_ts <- as.ts(price$Price) |> diff() |> na.omit()
break_data <- cbind(
  Lag0 = diff_price_ts,
  Lag1 = stats::lag(diff_price_ts)
)

qlr <- Fstats(Lag0 ~ 1 + Lag1, data = break_data, from = 0.10) # computes supF (QLR) for unknown break
break_test <- sctest(qlr, type = "supF") #formal test.
print(break_test)
# result sup.F = 7.63, p = 0.282 ==> no structural break

bp <- breakpoints(qlr, alpha = 0.05)
cat("\nEstimated breakpoints (α = 0.05):\n"); print(bp)
# most likely breakpoint is price$qtr[121] = 2022-Q1, but not significant at 5% level.

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


# Inner-JOIN (n=114)
df <- inner_join(as_tibble(price), as_tibble(rate), by = "qtr") |>
  arrange(qtr) |>
  as_tsibble(index = qtr)
df

# TRAIN TEST SPLIT
ratio <- floor(nrow(df) * 0.2)
train <- df |>
    slice_head(n = nrow(df) - ratio)
train

test <- df |>
    slice_tail(n = ratio)
test

# Box cox transformation vs Log?
lambda_train <- train |>
  features(Price, features = guerrero) |>
  pull(lambda_guerrero)
cat("Box-Cox λ (train):", round(lambda_train, 4), "\n")
# RESULT: λ = 0.322 → USE BC TRANSFORM ≠ LOG

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


# ACF & PACF OF DIFFERENCED SERIES — price, then rate
# These guide ARIMA(p, d, q) order identification in the modelling file.
acf_price_diff <- train |>
  filter(!is.na(price_bc_d)) |>
  ACF(price_bc_d, lag_max = 24) |>
  autoplot() +
  labs(title = paste0("ACF: Δprice_bc (λ = ", round(lambda_train, 4), ", d = 1)"))
ggsave("plots/01-10_acf_price_diff.png", plot = acf_price_diff,
       width = 12, height = 8, units = "cm")

pacf_price_diff <- train |>
  filter(!is.na(price_bc_d)) |>
  PACF(price_bc_d, lag_max = 24) |>
  autoplot() +
  labs(title = paste0("PACF: Δprice_bc (λ = ", round(lambda_train, 4), ", d = 1)"))
ggsave("plots/01-11_pacf_price_diff.png", plot = pacf_price_diff,
       width = 12, height = 8, units = "cm")

acf_rate_diff <- train |>
  filter(!is.na(rate_d)) |>
  ACF(rate_d, lag_max = 24) |>
  autoplot() +
  labs(title = "ACF: Δrate")
ggsave("plots/01-12_acf_rate_diff.png", plot = acf_rate_diff,
       width = 12, height = 8, units = "cm")

pacf_rate_diff <- train |>
  filter(!is.na(rate_d)) |>
  PACF(rate_d, lag_max = 24) |>
  autoplot() +
  labs(title = "PACF: Δrate")
ggsave("plots/01-13_pacf_rate_diff.png", plot = pacf_rate_diff,
       width = 12, height = 8, units = "cm")

# Save datasets for modeling
if (!dir.exists("data")) dir.create("data")
write_rds(train, "data/train.rds")   # incl. price_bc and price_bc_d
write_rds(test,  "data/test.rds")    # incl. price_bc and price_bc_d
train
test
