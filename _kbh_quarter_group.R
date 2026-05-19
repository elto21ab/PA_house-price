# Install required packages (uncomment if you haven't installed them yet)
# install.packages("dplyr")
# install.packages("readr")
# install.packages("lubridate")
# install.packages("ggplot2")
# install.packages("scales")

library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(scales)

# ── Load data ─────────────────────────────────────────────────────────────────

# Quarterly housing prices (DKK / m²)
df_price <- read_csv('kbh_quarter_sqm_price.csv', show_col_types = FALSE) |>
  mutate(date = yq(gsub('K', 'Q', Quarter))) |>
  filter(!is.na(date)) |>
  arrange(date)

# Quarterly mean bond interest rate (%)
df_bond <- read_csv('bondInt_quarterly.csv', show_col_types = FALSE) |>
  mutate(date = yq(gsub('K', 'Q', Quarter))) |>
  filter(!is.na(date)) |>
  arrange(date)

# Inner join → keeps only quarters present in both datasets (drops pre-1997K3)
df <- inner_join(df_price, df_bond, by = c("Quarter", "date"))

# ── Dual-axis scaling ─────────────────────────────────────────────────────────
# Linearly map the bond rate onto the price axis range so both series fit the
# same panel. The secondary axis inverts the transform to show true rate values.

price_min <- min(df$Price)
price_max <- max(df$Price)
rate_min  <- min(df$mean_rate)
rate_max  <- max(df$mean_rate)

scale_rate   <- function(x) (x - rate_min) / (rate_max - rate_min) * (price_max - price_min) + price_min
unscale_rate <- function(x) (x - price_min) / (price_max - price_min) * (rate_max - rate_min) + rate_min

# ── Plot ──────────────────────────────────────────────────────────────────────

ggplot(df, aes(x = date)) +
  # Housing price (left axis)
  geom_line(aes(y = Price, color = "Price per m\u00b2"),
            linewidth = 0.9) +
  geom_point(aes(y = Price, color = "Price per m\u00b2"),
             size = 1.2) +
  # Bond interest rate scaled to price axis (right axis)
  geom_line(aes(y = scale_rate(mean_rate), color = "Bond rate (mean)"),
            linewidth = 0.9, linetype = "dashed") +
  geom_point(aes(y = scale_rate(mean_rate), color = "Bond rate (mean)"),
             size = 1.2) +
  scale_y_continuous(
    name   = "DKK / m\u00b2",
    labels = comma,
    sec.axis = sec_axis(
      transform = unscale_rate,
      name      = "Mean bond rate (%)",
      labels    = function(x) sprintf("%.1f%%", x)
    )
  ) +
  scale_color_manual(
    values = c("Price per m\u00b2" = "#2563EB", "Bond rate (mean)" = "#DC2626")
  ) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    title   = "Copenhagen Housing Prices vs. Bond Interest Rate (quarterly)",
    caption = "Overlapping period only (1997K3 \u2013 present). Bond rate = weekly mean collapsed to quarter.",
    x       = NULL,
    color   = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title         = element_text(face = "bold", size = 14),
    legend.position    = "bottom",
    panel.grid.minor   = element_line(color = "gray92"),
    axis.title.y.right = element_text(color = "#DC2626"),
    axis.text.y.right  = element_text(color = "#DC2626"),
    axis.title.y.left  = element_text(color = "#2563EB"),
    axis.text.y.left   = element_text(color = "#2563EB")
  )

# ── Summary ───────────────────────────────────────────────────────────────────
cat(sprintf("Overlapping quarters: %d  (%s to %s)\n",
            nrow(df), min(df$Quarter), max(df$Quarter)))
summary(df[, c("Price", "mean_rate")])
