# =============================================================================
# 02_stationarity_breaks.R — Structural break tests (housing ∼ bond rate)
# =============================================================================

library(tidyverse)
library(lubridate)
library(tsibble)
library(urca)
library(strucchange)

# Load the data
df_price <- read_csv("data/kbh_quarter_sqm_price.csv", show_col_types = FALSE) |>
  mutate(date = yq(gsub("K", "Q", Quarter))) |>
  filter(!is.na(date)) |>
  arrange(date)

df_bond <- read_csv("data/bondInt_quarterly.csv", show_col_types = FALSE) |>
  mutate(date = yq(gsub("K", "Q", Quarter))) |>
  filter(!is.na(date)) |>
  arrange(date)

# Join on date for later structural break testing
df <- inner_join(df_price, df_bond, by = c("Quarter", "date")) |>
  mutate(qtr = yearquarter(date)) |>
  as_tsibble(index = qtr)

# Estimate optimal Box-Cox lambda using Guerrero's method
lambda <- df |>
  features(Price, features = guerrero) |>
  pull(lambda_guerrero)

# Apply Box-Cox transformation
df <- df |>
  mutate(price_transformed = box_cox(Price, lambda))

cutoff_qtr <- yearquarter(yq("2019 Q4"))
train <- df |> filter(qtr <= cutoff_qtr)


# =============================================================================
# STEP 1 — Structural break test (QLR / supF)
# =============================================================================

cat("=" %|% rep("=", 79) |> paste(collapse = "") %|% "\n")
cat("STRUCTURAL BREAK TEST: QLR on Δtransformed(Price) ~ Δmean_rate\n")
cat(rep("=", 79) |> paste(collapse = "") %|% "\n\n")

dtransformed <- diff(train$price_transformed)
drate <- diff(train$mean_rate)
bp_data <- tibble(dtransformed = dtransformed, drate = drate)

fs <- Fstats(dtransformed ~ drate, data = bp_data, from = 0.15)

png("plots/02_qlr_fstats.png", width = 14, height = 8, units = "cm", res = 100)
plot(fs, main = "QLR (supF) test — Δtransformed(Price) ~ Δmean_rate")
dev.off()

cat("supF test result:\n")
print(sctest(fs))

# Estimate breakpoint
break_idx <- which.max(fs$Fstats) + floor(0.15 * nrow(bp_data))
break_date <- train$date[break_idx + 1]
cat("\nEstimated structural break date:", format(break_date), "\n")

cat("\n✓ Structural break analysis complete.\n")


# =============================================================================
# STEP 4 — Post-break stationarity re-check
# =============================================================================

# Adjust cutoff based on QLR result. Here: post-2010 to focus on crisis aftermath.
post_break <- train |> filter(date >= as.Date("2010-01-01"))

cat("\n" %|% rep("=", 79) |> paste(collapse = "") %|% "\n")
cat("POST-BREAK SAMPLE: log(Price) stationarity tests\n")
cat("(from", format(min(post_break$date)), ")\n")
cat(rep("=", 79) |> paste(collapse = "") %|% "\n\n")

cat("ADF test (drift):\n")
summary(ur.df(post_break$log_price, type = "drift", selectlags = "AIC"))

cat("\nKPSS test:\n")
summary(ur.kpss(post_break$log_price, type = "mu"))

cat("\n✓ Stationarity analysis complete.\n")
