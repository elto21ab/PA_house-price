# Install required packages (uncomment if you haven't installed them yet)
# install.packages("dplyr")
# install.packages("readr")
# install.packages("lubridate")
# install.packages("ggplot2")

library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)

# Load and prep housing data
df <- read_delim('Housing_data_CPH_original.csv', delim = ';', show_col_types = FALSE)

# Format date and calculate price per m2
df <- df %>%
  mutate(
    date = ym(as.character(Årmåned_salg)),
    price_per_m2 = tinglyst_pris / boligareal
  ) %>%
  filter(!is.na(date) & !is.na(price_per_m2))

# 99th percentile filter
threshold <- quantile(df$price_per_m2, 0.99, na.rm = TRUE)
df_clean <- df %>%
  filter(price_per_m2 <= threshold) %>%
  arrange(date)

# Monthly median line
median_trend <- df_clean %>%
  group_by(date) %>%
  summarize(median_price_per_m2 = median(price_per_m2, na.rm = TRUE), .groups = 'drop')

# Plotting
ggplot() +
  geom_point(data = df_clean, aes(x = date, y = price_per_m2), 
             size = 0.5, alpha = 0.2, color = 'steelblue') +
  geom_line(data = median_trend, aes(x = date, y = median_price_per_m2), 
            color = 'red', linewidth = 1) +
  labs(
    title = "Copenhagen Housing: Price per m² (Outliers Removed)",
    x = "Year",
    y = "DKK / m²"
  ) +
  theme_minimal() +
  theme(panel.grid.minor = element_line(color = "gray90"),
        plot.title = element_text(face = "bold", size = 14))

cat(sprintf("Removed transactions above %s DKK/m²\n", format(threshold, big.mark = ",", scientific = FALSE)))

# View unique zip codes
unique(df$zip_code_name)
