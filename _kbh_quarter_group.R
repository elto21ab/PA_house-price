# Install required packages (uncomment if you haven't installed them yet)
# install.packages("dplyr")
# install.packages("readr")
# install.packages("lubridate")
# install.packages("ggplot2")

library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)

# Load and prep quarterly housing data
df <- read_csv('kbh_quarter_sqm_price.csv', show_col_types = FALSE)

# Format date and set price per m2 (no percentile filter applied)
df_clean <- df %>%
  mutate(
    date = yq(gsub('K', 'Q', Quarter)),
    price_per_m2 = Price
  ) %>%
  filter(!is.na(date) & !is.na(price_per_m2)) %>%
  arrange(date)

# Since each quarter has a single price value, the trend is the data itself
median_trend <- df_clean

# Plotting
ggplot() +
  geom_point(data = df_clean, aes(x = date, y = price_per_m2), 
             size = 1.5, color = 'steelblue') +
  geom_line(data = median_trend, aes(x = date, y = price_per_m2), 
            color = 'red', linewidth = 1) +
  labs(
    title = "Copenhagen Quarterly Housing: Price per m²",
    x = "Year",
    y = "DKK / m²"
  ) +
  theme_minimal() +
  theme(panel.grid.minor = element_line(color = "gray90"),
        plot.title = element_text(face = "bold", size = 14))

# Print data summary
summary(df_clean)
