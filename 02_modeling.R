# =============================================================================
# 02_stationarity_breaks.R — Structural break tests (housing ∼ bond rate)
# =============================================================================

# # ── 0.2  Train / test split ─────────────────────────────────────────────────
# train <- df |> filter(qtr <= yearquarter("2019 Q4"))
# test  <- df |> filter(qtr >  yearquarter("2019 Q4"))

# cat("\n--- TRAIN/TEST SPLIT ---\nTrain n =", nrow(train), "| Test n =", nrow(test), "\n")
# train
### SANITY CHECK

# ── PLOT #2b: price level differenced ──────────────────────────────────────
# df_diff <- df |>
#   mutate(df = c(NA, diff(price))) |>
#   drop_na(df)

# ggplot(train_diff, aes(qtr, price_diff)) +
#   geom_line(colour = "#2563EB") +
#   labs(title = paste0("Price (d=1, transformedλ = ", round(lambda, 4), ")"),
#        x = NULL, y = "Differenced price") +
#   theme(plot.title = element_text(face = "bold"))

# ggsave("plots/01-2c_price_trans_diff.png", width = 14, height = 8, units = "cm")


#===========================================================================#

# # Calculate volatility metrics
# vol_raw <- sd(diff(train$Price), na.rm = TRUE)
# vol_transformed <- sd(diff(train$price_transformed), na.rm = TRUE)
# vol_reduction <- (1 - vol_transformed / vol_raw) * 100

# # ── Summary statistics ──────────────────────────────────────────────────────────
# sprintf("\n--- HOUSING PRICE SERIES (%s – %s | n = %d) ---\nRange: %d – %d DKK/m²\n  Min @ %s | Max @ %s\nTotal growth in period: %.1f%%\nBox-Cox λ (Guerrero): %.4f\nVolatility (raw): %.3f | (transformed): %.3f | Reduction: %.5f%%\n",
#         format(min(train$qtr)), format(max(train$qtr)), nrow(train),
#         min(train$Price), max(train$Price),
#         format(train$qtr[which.min(train$Price)]), format(train$qtr[which.max(train$Price)]),
#         (max(train$Price) - min(train$Price)) / min(train$Price) * 100,
#         lambda, vol_raw, vol_transformed, vol_reduction) |> cat()

# ── PLOT #2a: Transformed price level & non-differenced ──────────────────────────────────────
# ggplot(train, aes(qtr, price_transformed)) +
#   geom_line(colour = "#2563EB") +
#   labs(title = paste0("Price (transformed, λ = ", round(lambda, 4), ")"),
#        x = NULL, y = "Transformed price") +
#   theme(plot.title = element_text(face = "bold"))
# ggsave("plots/01-2a_price_trans.png", width = 14, height = 8, units = "cm")


# =================================================================
# 04_models.R — ARIMA, ARIMAX, VAR/Granger
# =================================================================
library(tidyverse)
library(tsibble)
library(forecast)
library(vars)
library(urca)

# -- Load & align ------------------------------------------------
price <- readRDS("data/price_clean.rds") |> select(qtr, price_bc)
rate  <- readRDS("data/rate_clean.rds")  |> select(qtr, rate_diff = rate_diff)
lambda<- readRDS("data/lambda.rds")

df <- price |>
  inner_join(rate, by = "qtr") |>
  as_tsibble(index = qtr) |>
  mutate(price_diff = difference(price_bc),
         rate_diff  = rate_diff) |>
  filter(!is.na(price_diff), !is.na(rate_diff))

cat("Aligned obs:", nrow(df), "\n")

# -- Cointegration & causality -----------------------------------
joh <- ca.jo(as.matrix(df[, c("price_bc","rate_diff")]), type = "trace", ecdet = "const", K = 2)
summary(joh)   # rank=0 → no cointegration

# VAR in diffs for Granger (valid under I(1) regardless)
vard <- VAR(as.matrix(df[, c("price_diff","rate_diff")]), p = 2, type = "const")
causality(vard, cause = "rate_diff")  # H0: rate does NOT Granger-cause price

# -- Train / test split ------------------------------------------
n   <- nrow(df)
trn <- 1:floor(n * 0.8)
tst <- (floor(n * 0.8) + 1):n

y_train <- df$price_bc[trn]
y_test  <- df$price_bc[tst]

# -- Univariate fits ---------------------------------------------
fit_naive <- rwf(y_train, drift = TRUE)
fit_110   <- Arima(y_train, order = c(1,1,0), lambda = lambda)
fit_auto  <- auto.arima(y_train, lambda = lambda, stepwise = TRUE)

# -- ARIMAX (xreg = differenced rate) ----------------------------
X_train <- cbind(rate_diff = df$rate_diff[trn])
X_test  <- cbind(rate_diff = df$rate_diff[tst])

fit_x110  <- Arima(y_train, order = c(1,1,0), xreg = X_train, lambda = lambda)
fit_xauto <- auto.arima(y_train, xreg = X_train, lambda = lambda, stepwise = TRUE)

# -- Forecasts -------------------------------------------------
h <- length(tst)

fc_naive  <- forecast(fit_naive,  h = h)
fc_110    <- forecast(fit_110,   h = h)
fc_auto   <- forecast(fit_auto,  h = h)
fc_x110   <- forecast(fit_x110,  h = h, xreg = X_test)
fc_xauto  <- forecast(fit_xauto, h = h, xreg = X_test)

# -- Accuracy ----------------------------------------------------
accuracy <- tibble(
  model = c("naive","arima110","auto","arimax110","arimaxAuto"),
  RMSE  = map_dbl(list(fc_naive,fc_110,fc_auto,fc_x110,fc_xauto),
                  ~ sqrt(mean((.x$mean - y_test)^2))),
  MAPE  = map_dbl(list(fc_naive,fc_110,fc_auto,fc_x110,fc_xauto),
                  ~ mean(abs((.x$mean - y_test)/y_test))*100)
)
print(accuracy)

# -- Diagnostics -------------------------------------------------
checkresiduals(fit_110)
checkresiduals(fit_x110)