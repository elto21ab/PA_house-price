# Script to generate two high-quality forecast comparison plots on the original DKK/m² scale.
# Focuses strictly on 2020Q1 to 2025Q4 and includes back-transformed 95% confidence intervals.
# Model selection is 100% logically consistent across Box-Cox and Log plots (both use break dummies).

library(tidyverse)
library(tsibble)
library(fable)
library(fabletools)
library(feasts)
library(forecast)

# Load data
price_raw <- read_csv("data/kbh_quarter_sqm_price.csv", show_col_types = FALSE)
rate_raw  <- read_csv("data/bondint_quarterly.csv", show_col_types = FALSE)

price <- price_raw |>
  mutate(
    qtr = yearquarter(str_replace(Quarter, "K", " Q")),
    Price = as.numeric(Price)
  ) |>
  select(qtr, Price) |>
  arrange(qtr) |>
  as_tsibble(index = qtr)

rate <- rate_raw |>
  mutate(
    qtr = yearquarter(str_replace(Quarter, "K", " Q")),
    rate = as.numeric(mean_rate)
  ) |>
  select(qtr, rate) |>
  arrange(qtr) |>
  as_tsibble(index = qtr)

df <- inner_join(price, rate, by = "qtr") |>
  arrange(qtr) |>
  as_tsibble(index = qtr)

# Split parameters
ratio <- floor(nrow(df) * 0.2)
train_raw <- df |> slice_head(n = nrow(df) - ratio)
test_raw  <- df |> slice_tail(n = ratio)

lambda_train <- 0.3226227
best_k <- 2

# Create lagged variables on full data first
df_model <- df |>
  arrange(qtr) |>
  mutate(
    y_log = log(Price),
    y_bc = forecast::BoxCox(Price, lambda_train),
    rate_lag1 = dplyr::lag(rate, 1),
    rate_lag2 = dplyr::lag(rate, best_k)
  )

train <- df_model |> filter(qtr %in% train_raw$qtr)
test  <- df_model |> filter(qtr %in% test_raw$qtr)

# -----------------------------------------------------------------------------
# 1. Box-Cox Models (with Break Dummies)
# -----------------------------------------------------------------------------
# A. ARIMA (Full History, Box-Cox) - ESTIMATED WITH PROPER lambda_full!
price_train_full <- price |> filter(qtr < yearquarter("2020 Q3"))
price_test_full  <- price |> filter(qtr >= yearquarter("2020 Q3"))
lambda_full <- price_train_full |> features(Price, features = guerrero) |> pull(lambda_guerrero)

fit_arima_full_bc <- price_train_full |>
  model(m = ARIMA(box_cox(Price, lambda_full), stepwise = FALSE, approximation = FALSE))
fc_arima_full_bc <- forecast(fit_arima_full_bc, new_data = price_test_full)

# B. ARIMAX Lag 2 + Break Dummy (d=1, Box-Cox)
train_dummy_bc <- train |>
  mutate(break_dummy = if_else(qtr >= yearquarter("2006 Q3"), 1, 0),
         rate_lag_dummy = rate_lag2 * break_dummy)
test_dummy_bc <- test |>
  mutate(break_dummy = if_else(qtr >= yearquarter("2006 Q3"), 1, 0),
         rate_lag_dummy = rate_lag2 * break_dummy)

fit_arimax_dummy_bc <- train_dummy_bc |> drop_na(rate_lag2) |>
  model(m = ARIMA(box_cox(Price, lambda_train) ~ pdq(d = 1) + rate_lag2 + break_dummy + rate_lag_dummy, stepwise = FALSE, approximation = FALSE))
fc_arimax_dummy_bc <- forecast(fit_arimax_dummy_bc, new_data = test_dummy_bc)


# -----------------------------------------------------------------------------
# 2. Log Models (with Break Dummies)
# -----------------------------------------------------------------------------
# A. ARIMA (Full History, Log)
fit_arima_full_log <- price_train_full |>
  model(m = ARIMA(log(Price), stepwise = FALSE, approximation = FALSE))
fc_arima_full_log <- forecast(fit_arima_full_log, new_data = price_test_full)

# B. ARIMAX Lag 1 + Break Dummy (d=1, Log)
train_lag1_dummy_log <- train |>
  mutate(break_dummy = if_else(qtr >= yearquarter("2006 Q3"), 1, 0),
         rate_lag1_dummy = rate_lag1 * break_dummy)
test_lag1_dummy_log <- test |>
  mutate(break_dummy = if_else(qtr >= yearquarter("2006 Q3"), 1, 0),
         rate_lag1_dummy = rate_lag1 * break_dummy)

fit_arimax_lag1_dummy_log <- train_lag1_dummy_log |> drop_na(rate_lag1) |>
  model(m = ARIMA(log(Price) ~ pdq(d = 1) + rate_lag1 + break_dummy + rate_lag1_dummy, stepwise = FALSE, approximation = FALSE))
fc_arimax_lag1_dummy_log <- forecast(fit_arimax_lag1_dummy_log, new_data = test_lag1_dummy_log)

# C. ARIMAX Lag 2 + Break Dummy (d=1, Log)
train_lag2_dummy_log <- train |>
  mutate(break_dummy = if_else(qtr >= yearquarter("2006 Q3"), 1, 0),
         rate_lag_dummy = rate_lag2 * break_dummy)
test_lag2_dummy_log <- test |>
  mutate(break_dummy = if_else(qtr >= yearquarter("2006 Q3"), 1, 0),
         rate_lag_dummy = rate_lag2 * break_dummy)

fit_arimax_lag2_dummy_log <- train_lag2_dummy_log |> drop_na(rate_lag2) |>
  model(m = ARIMA(log(Price) ~ pdq(d = 1) + rate_lag2 + break_dummy + rate_lag_dummy, stepwise = FALSE, approximation = FALSE))
fc_arimax_lag2_dummy_log <- forecast(fit_arimax_lag2_dummy_log, new_data = test_lag2_dummy_log)


# -----------------------------------------------------------------------------
# Data Prep for Plots (from 2020 Q1 onwards)
# -----------------------------------------------------------------------------
actual_line_data <- df |>
  filter(qtr >= yearquarter("2020 Q1") & qtr <= yearquarter("2025 Q4")) |>
  as_tibble() |>
  select(qtr, Price) |>
  mutate(Model = "Actual Prices")

# -----------------------------------------------------------------------------
# PLOT 1: BOX-COX FORECASTS
# -----------------------------------------------------------------------------
# Extract intervals (95% only)
int_arima_full_bc <- fc_arima_full_bc |>
  hilo(level = 95) |> unpack_hilo("95%") |> as_tibble() |>
  select(qtr, `95%_lower`, `95%_upper`) |>
  mutate(Model = "ARIMA (Full History)")

int_arimax_dummy_bc <- fc_arimax_dummy_bc |>
  hilo(level = 95) |> unpack_hilo("95%") |> as_tibble() |>
  select(qtr, `95%_lower`, `95%_upper`) |>
  mutate(Model = "ARIMAX Lag 2 + Break Dummy (d=1)")

intervals_bc <- bind_rows(int_arima_full_bc, int_arimax_dummy_bc)

# Forecast means
means_bc <- tibble(
  qtr = rep(test_raw$qtr, 2),
  Price = c(
    fc_arima_full_bc$.mean,
    fc_arimax_dummy_bc$.mean
  ),
  Model = rep(c(
    "ARIMA (Full History)",
    "ARIMAX Lag 2 + Break Dummy (d=1)"
  ), each = nrow(test_raw))
)

# Colors and styles
colors_bc <- c(
  "Actual Prices" = "black",
  "ARIMA (Full History)" = "#2563EB",            # Blue
  "ARIMAX Lag 2 + Break Dummy (d=1)" = "#F59E0B" # Amber
)

p_bc <- ggplot() +
  # Confidence bands (95% only)
  geom_ribbon(data = intervals_bc, aes(x = qtr, ymin = `95%_lower`, ymax = `95%_upper`, fill = Model), alpha = 0.15, inherit.aes = FALSE) +
  scale_fill_manual(values = c("ARIMA (Full History)" = "#2563EB", "ARIMAX Lag 2 + Break Dummy (d=1)" = "#F59E0B"), guide = "none") +
  # Mean lines for forecasts
  geom_line(data = means_bc, aes(x = qtr, y = Price, color = Model, linetype = Model), linewidth = 0.8) +
  # Actual prices line
  geom_line(data = actual_line_data, aes(x = qtr, y = Price, color = Model, linetype = Model), linewidth = 1.0) +
  # Boundary line
  geom_vline(xintercept = yearquarter("2020 Q2"), linetype = "dashed", color = "grey40") +
  # Labels and scales
  scale_color_manual(values = colors_bc) +
  scale_linetype_manual(values = c(
    "Actual Prices" = "solid",
    "ARIMA (Full History)" = "dashed",
    "ARIMAX Lag 2 + Break Dummy (d=1)" = "dotdash"
  )) +
  scale_y_continuous(labels = scales::comma) +
  coord_cartesian(ylim = c(20000, 95000)) + # Expanded range prevents cutting off confidence bands!
  labs(
    title = "Box-Cox Forecasts & 95% Confidence Bands",
    subtitle = "Test period comparison on original scale (DKK/m²)",
    x = NULL, y = "DKK / m²", color = NULL, linetype = NULL
  ) +
  theme_minimal(base_size = 9) +
  theme(
    legend.position = "bottom",
    legend.direction = "vertical",
    plot.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 7),
    legend.key.height = unit(0.3, "cm"),
    legend.margin = margin(t = -5)
  )

ggsave("plots/03-03_forecast_compare_boxcox.png", plot = p_bc, width = 12, height = 8, units = "cm", dpi = 300)


# -----------------------------------------------------------------------------
# PLOT 2: LOG FORECASTS
# -----------------------------------------------------------------------------
# Extract intervals for best model: ARIMAX Lag 1 + Break Dummy (95% only)
int_arimax_lag1_dummy_log <- fc_arimax_lag1_dummy_log |>
  hilo(level = 95) |> unpack_hilo("95%") |> as_tibble() |>
  select(qtr, `95%_lower`, `95%_upper`) |>
  mutate(Model = "ARIMAX Lag 1 + Break Dummy (d=1)")

# Forecast means
means_log <- tibble(
  qtr = rep(test_raw$qtr, 3),
  Price = c(
    fc_arima_full_log$.mean,
    fc_arimax_lag1_dummy_log$.mean,
    fc_arimax_lag2_dummy_log$.mean
  ),
  Model = rep(c(
    "ARIMA (Full History)",
    "ARIMAX Lag 1 + Break Dummy (d=1)",
    "ARIMAX Lag 2 + Break Dummy (d=1)"
  ), each = nrow(test_raw))
)

# Colors and styles
colors_log <- c(
  "Actual Prices" = "black",
  "ARIMAX Lag 1 + Break Dummy (d=1)" = "#10B981",# Green
  "ARIMA (Full History)" = "#2563EB",            # Blue
  "ARIMAX Lag 2 + Break Dummy (d=1)" = "#EF4444" # Red
)

p_log <- ggplot() +
  # Confidence bands for ARIMAX Lag 1 + Break Dummy (95% only)
  geom_ribbon(data = int_arimax_lag1_dummy_log, aes(x = qtr, ymin = `95%_lower`, ymax = `95%_upper`), fill = "#10B981", alpha = 0.15, inherit.aes = FALSE) +
  # Mean lines for forecasts
  geom_line(data = means_log, aes(x = qtr, y = Price, color = Model, linetype = Model), linewidth = 0.8) +
  # Actual prices line
  geom_line(data = actual_line_data, aes(x = qtr, y = Price, color = Model, linetype = Model), linewidth = 1.0) +
  # Boundary line
  geom_vline(xintercept = yearquarter("2020 Q2"), linetype = "dashed", color = "grey40") +
  # Labels and scales
  scale_color_manual(values = colors_log) +
  scale_linetype_manual(values = c(
    "Actual Prices" = "solid",
    "ARIMAX Lag 1 + Break Dummy (d=1)" = "solid",
    "ARIMA (Full History)" = "dashed",
    "ARIMAX Lag 2 + Break Dummy (d=1)" = "dotted"
  )) +
  scale_y_continuous(labels = scales::comma) +
  coord_cartesian(ylim = c(20000, 95000)) + # Expanded range prevents cutting off confidence bands!
  labs(
    title = "Natural Log Forecasts & 95% Confidence Bands",
    subtitle = "Test period comparison on original scale (DKK/m²)",
    x = NULL, y = "DKK / m²", color = NULL, linetype = NULL
  ) +
  theme_minimal(base_size = 9) +
  theme(
    legend.position = "bottom",
    legend.direction = "vertical",
    plot.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 7),
    legend.key.height = unit(0.3, "cm"),
    legend.margin = margin(t = -5)
  )

ggsave("plots/03-04_forecast_compare_log.png", plot = p_log, width = 12, height = 8, units = "cm", dpi = 300)

cat("Successfully generated updated forecast plots in plots/ directory!\n")
