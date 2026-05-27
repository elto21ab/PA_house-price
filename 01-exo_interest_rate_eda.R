# =================================================================
# 03_rate_eda.R — Interest rate: minimal exploration & alignment
# =================================================================
library(tidyverse)
library(lubridate)
library(tsibble)
library(urca)
library(scales)

theme_set(theme_minimal(base_size = 12))
cat("Working directory:", getwd(), "\n")

# ── Load rate ─────────────────────────────────────────────────────
# Adjust filename / column names to your data
rate_raw <- read_csv("data/bondint_quarterly.csv", show_col_types = FALSE) |>
  rename(qtr = Quarter, rate = mean_rate) |>
  mutate(qtr = yearquarter(yq(gsub("K", "Q", qtr)))) |>
  arrange(qtr) |>
  as_tsibble(index = qtr)

# ── Align w/ price quarters ──────────────────────────────────────
price_dates <- read_csv("data/kbh_quarter_sqm_price.csv", show_col_types = FALSE) |>
  rename(qtr = Quarter) |>
  mutate(qtr = yearquarter(yq(gsub("K", "Q", qtr)))) |>
  distinct(qtr) |>
  pull(qtr)

rate <- rate_raw |>
  filter(qtr %in% price_dates)

if (nrow(rate) != length(price_dates)) {
  warning("Rate coverage missing for some price quarters.")
}

# ── 1. Plot levels ──────────────────────────────────────────────
rate_plot <- ggplot(rate, aes(qtr, rate)) +
  geom_line(colour = "#DC2626") +
  scale_y_continuous(labels = comma) +
  labs(title = "Interest rate (level)", x = NULL, y = "Rate")
ggsave("plots/03-1_rate_level.png", rate_plot, width = 14, height = 8, units = "cm")

# ── 4. Unit-root tests ──────────────────────────────────────────
ur_tidy <- function(x, label) {
  adf_trend <- ur.df(x, type = "trend", selectlags = "AIC")
  adf_drift <- ur.df(x, type = "drift", selectlags = "AIC")
  adf_none  <- ur.df(x, type = "none", selectlags = "AIC")
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

ur_tbl <- bind_rows(
  ur_tidy(as.ts(rate$rate),              "rate_level"),
  ur_tidy(as.ts(na.omit(diff(rate$rate))), "rate_diff1")
)

print(ur_tbl)

# ── 6. Diff if needed ───────────────────────────────────────────
rate <- rate |>
  mutate(rate_diff = difference(rate))

write_rds(rate, "data/rate_clean.rds")
cat("\n✓ Saved: data/rate_clean.rds (qtr, rate, rate_diff)\n")
