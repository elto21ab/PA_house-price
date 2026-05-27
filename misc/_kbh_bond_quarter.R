# _kbh_eda_prices.R
# Box-Cox on prices
# Stationarity tests on prices
# Outputs: lambda, d (differencing order)

# _kbh_breaks_prices.R
# QLR test on differenced prices
# Outputs: breakpoint date, restricted sample?

# _kbh_acf_pacf.R
# ACF/PACF on prices
# Outputs: candidate (p,q)

# _kbh_check_rate.R
# Quick: ADF/KPSS on bond rate
# Outputs: Is rate I(0) or I(1)?

# _kbh_modeling.R
# Fit ARIMA(p,d,q) 
# Fit ARIMAX(p,d,q) with rate
# Compare, validate
# Outputs: chosen model, forecasts

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
df_price <- read_csv('data/kbh_quarter_sqm_price.csv', show_col_types = FALSE) |>
  mutate(date = yq(gsub('K', 'Q', Quarter))) |>
  filter(!is.na(date)) |>
  arrange(date)

# Quarterly mean bond interest rate (%)
df_bond <- read_csv('data/bondInt_quarterly.csv', show_col_types = FALSE) |>
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

r_val <- cor(df$Price, df$mean_rate)
cor_label <- sprintf("r = %.3f", r_val)

p <- ggplot(df, aes(x = date)) +
  # Housing price (left axis)
  geom_line(aes(y = Price, color = "Price per m\u00b2"), 
            linewidth = 0.6) +
  geom_point(aes(y = Price, color = "Price per m\u00b2"),
             size = 1.2) +
  # Bond interest rate scaled to price axis (right axis)
  geom_line(aes(y = scale_rate(mean_rate), color = "Bond rate (mean)"),
            linewidth = 0.6) + 
            # , linetype = "dashed") +
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
  annotate("text",
           x     = min(df$date) + (max(df$date) - min(df$date)) / 2,
           y     = price_max * 0.97,
           label = cor_label,
           hjust = 0.5, vjust = 1,
           size  = 4, color = "gray30",
           fontface = "italic") +
  labs(
    title   = "Copenhagen Housing Prices vs. Bond Interest Rate (quarterly)",
    # caption = "Overlapping period only (1997K3 \u2013 2025K4). Bond rate = weekly mean collapsed to quarter.",
    x       = NULL,
    color   = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    # aspect.ratio       = 12/24,          # locks preview to match ggsave (height/width)
    plot.title         = element_text(face = "bold", size = 14),
    legend.position    = "bottom",
    panel.grid.minor   = element_line(color = "gray92"),
    axis.title.y.right = element_text(color = "#DC2626"),
    axis.text.y.right  = element_text(color = "#DC2626"),
    axis.title.y.left  = element_text(color = "#2563EB"),
    axis.text.y.left   = element_text(color = "#2563EB")
  )

# print(p)

# Save as wide, short figure suitable for a paper
ggsave("plots/kbh_price_vs_bond_rate.png", plot = p,
       width = 32, height = 14, units = "cm")

# ── Summary statistics ────────────────────────────────────────────────────────
writeLines(c(
  sprintf("Overlapping quarters: %d (%s to %s)", nrow(df), min(df$Quarter), max(df$Quarter)),
  "",
  "Housing Price (DKK / m2)",
  sprintf("  N:       %d",    nrow(df)),
  sprintf("  Mean:    %.0f",  mean(df$Price)),
  sprintf("  Median:  %.0f",  median(df$Price)),
  sprintf("  Std dev: %.0f",  sd(df$Price)),
  sprintf("  Min:     %.0f",  min(df$Price)),
  sprintf("  Max:     %.0f",  max(df$Price)),
  sprintf("  Growth:  %.1f%% (first to last)", (tail(df$Price,1)/head(df$Price,1)-1)*100),
  "",
  "Bond Interest Rate (%)",
  sprintf("  N:       %d",    nrow(df)),
  sprintf("  Mean:    %.2f",  mean(df$mean_rate)),
  sprintf("  Median:  %.2f",  median(df$mean_rate)),
  sprintf("  Std dev: %.2f",  sd(df$mean_rate)),
  sprintf("  Min:     %.2f",  min(df$mean_rate)),
  sprintf("  Max:     %.2f",  max(df$mean_rate)),
  "",
  "Correlation",
  sprintf("  Price ~ rate:      %.3f", cor(df$Price, df$mean_rate)),
  sprintf("  Price ~ log(rate): %.3f", cor(df$Price, log(df$mean_rate)))
))

