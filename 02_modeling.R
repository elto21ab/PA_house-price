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
