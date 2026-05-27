########################
#       PA-EXAM       #
########################

# Load packages
library(fpp3)
library(xtable)
library(tidyverse)
library(AER)
library(readxl)
library(GGally)       
library(purrr)
library(tsibble)
library(gridExtra)
library(urca)
library(knitr)
library(kableExtra)
library(vars)
library(dplyr)


### Load data
data<- read_excel('/Users/camillapedersen/Library/Mobile Documents/com~apple~CloudDocs/CM mat/2.sem/PA/Eksamen/Data til eksamen/KVMPRISER-kopi.xlsx', sheet = 1)
head(data, 5) 

### Transform to tsibble
data %>%
  mutate(TID = yearquarter(TID)) %>%
  as_tsibble(index=TID) -> tsKVMP

head(tsKVMP, 5) 

# Log-transform all numeric variables
log_prices <- tsKVMP %>%
  mutate(across(where(is.numeric), log))


##########################
## Preliminary Analysis ##

### Plot raw data and ACF
plot1a<- tsKVMP%>%
  autoplot(Kbh_S) +
  labs(title = "Square meter prices in Kbh S", y="Price in DKK", x="Time") +  
  scale_x_yearquarter(
    date_breaks = "2 years",
    expand      = c(0, 0)
  )+
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13),
  )

plot1b<- tsKVMP %>%
  ACF(Kbh_S)%>%
  autoplot() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13),
  )

grid.arrange(plot1a,plot1b,nrow=2, heights= c(1.25,1))


### Plot Logdiff data and ACF
plot2a<- tsKVMP%>%
  autoplot(difference(log(Kbh_S))) +
  labs(title = "Logdiff Square meter prices in Kbh S", y="Log diff Prices", x="Time") +  
  scale_x_yearquarter(
    date_breaks = "2 years",
    expand      = c(0, 0)
  )+
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13),
  )

plot2b<- tsKVMP %>%
  ACF(difference(log(Kbh_S)))%>%
  autoplot() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13),
  )

grid.arrange(plot2a,plot2b,nrow=2, heights=c(1.25,1))


###### Stationarity for all areas ########
log_prices_diff <- log_prices %>%
  mutate(across(-TID, difference)) %>%
  filter(!is.na(Kbh_S))  # drop first row after differencing

areas <- colnames(log_prices_diff)[colnames(log_prices_diff) != "TID"]

stationarity_results <- data.frame(
  Area = character(),
  ADF_stat = numeric(),
  KPSS_stat = numeric(),
  ADF_crit_5pct = numeric(),
  KPSS_crit_5pct = numeric(),
  Stationary = character(),
  stringsAsFactors = FALSE
)

for (area in areas) {
  series <- log_prices_diff[[area]]
  
  # ADF test
  adf <- ur.df(series, type = "drift", selectlags = "AIC")
  adf_stat <- adf@teststat[1]
  adf_crit <- adf@cval[1,1]
  
  
  # KPSS test
  kpss <- ur.kpss(series, type = "mu")
  kpss_stat <- kpss@teststat
  kpss_crit <- kpss@cval[4]
  
  # Decision logic (require both tests to return valid values)
  if (adf_stat < adf_crit) {
    is_adf_stationary <- TRUE
  } else {
    is_adf_stationary <- FALSE
  }
  if (kpss_stat < kpss_crit){
    is_kpss_stationary <- TRUE
  } else {
    is_kpss_stationary <- FALSE
  }
  stationary_flag <- if (is_adf_stationary & is_kpss_stationary) "Yes" else "No"
  
  stationarity_results <- rbind(
    stationarity_results,
    data.frame(
      Area = area,
      ADF_stat = round(adf_stat, 3),
      KPSS_stat = round(kpss_stat, 3),
      ADF_crit_1pct = round(adf_crit, 3),
      KPSS_crit_1pct = round(kpss_crit, 3),
      Stationary = stationary_flag
    )
  )
}

print(stationarity_results)
stationarity_results%>%
  dplyr::select(-ADF_crit_1pct,-KPSS_crit_1pct)%>%
  t()%>%
  kable(, format = "latex", digits = 3, booktabs = TRUE)

######## Cross - correlations ########
# Set up
lags <- -4:4
target <- "Kbh_S"
predictors <- setdiff(names(log_prices_diff),"TID")

# Define single-lag correlation function
ccf_single <- function(x, y, lag) {
  if (lag >= 0) cor(dplyr::lag(x, lag), y, use = "pairwise.complete.obs")
  else          cor(dplyr::lead(x, -lag), y, use = "pairwise.complete.obs")
}


# Compute all correlations with Kbh_S as target
ccf_kbhs <- expand_grid(var_x = predictors, lag = lags) %>%
  mutate(cc = map2_dbl(var_x, lag, ~ ccf_single(log_prices_diff[[.x]], log_prices_diff[[target]], .y)))


# View top correlations
ccf_kbhs_use <- ccf_kbhs %>%
  filter(var_x != target, lag > 0)

predictive_lags2 <- ccf_kbhs %>%
  filter(var_x != target, lag > 0) %>%            # only usable predictors
  filter(abs(cc) > 0.3) %>%                       # minimum correlation strength
  arrange(desc(abs(cc))) %>%                      # sort by strongest correlation
  slice_head(n = 6) 

acf(log_prices_diff$Kbh_S, lag.max = 4, plot = FALSE)

knitr::kable(t(predictive_lags2), format = "latex", digits = 3, booktabs = TRUE)

#########################
### ARIMA and ARIMAX ####
### log data ############
#########################

### Split data
train_data <- log_prices %>% filter_index(. ~ "2019 Q4")   # Up to and including 2019 Q4
test_data  <- log_prices %>% filter_index("2020 Q1" ~ "2024 Q4")  # 2020 Q1 to 2024 Q4

### Fit ARIMA and ARIMAX models on log-levels
models <- train_data %>%
  model(
    ARIMA_baseline= ARIMA(Kbh_S),
    ARIMAX_model6 = ARIMA(
      formula = Kbh_S ~ lag(Kbh_Ø, 1) + lag(Frederiksberg, 1) + lag(Kbh_N, 1) + lag(Frederiksberg_C, 2)+ lag(Frederiksberg_C, 1)+ lag(Kbh_K, 1)+ pdq(0:5, 1, 0:5)  
    ),
    ARIMAX_model5 = ARIMA(
      formula = Kbh_S ~ lag(Kbh_Ø, 1) + lag(Frederiksberg, 1) + lag(Kbh_N, 1) + lag(Frederiksberg_C, 2)+ lag(Frederiksberg_C, 1)+ pdq(0:5, 1, 0:5)  
    ),
    ARIMAX_model4 = ARIMA(
      formula = Kbh_S ~ lag(Kbh_Ø, 1) + lag(Frederiksberg, 1) + lag(Kbh_N, 1) + lag(Frederiksberg_C, 2)+ pdq(0:5, 1, 0:5) 
    ),
    ARIMAX_model3 = ARIMA(
      formula = Kbh_S ~ lag(Kbh_Ø, 1) + lag(Frederiksberg, 1) + lag(Kbh_N, 1) + pdq(0:5, 1, 0:5)
    ),
    ARIMAX_model2 = ARIMA(
      formula = Kbh_S ~ lag(Kbh_Ø, 1) + lag(Frederiksberg, 1) + pdq(0:5, 1, 0:5)  
    ),
    ARIMAX_model1 = ARIMA(
      formula = Kbh_S ~ lag(Kbh_Ø, 1) + pdq(0:5, 1, 0:5)  
    )
  )

### Baseline model report
models%>%
  dplyr::select(ARIMA_baseline)%>%
  report()

### Baseline model residual plot
models%>% 
  dplyr::select(ARIMA_baseline) %>% 
  gg_tsresiduals()

### All models glance
glance(models)

### Forecast on test set using ACUTAL FUTURE VALUES ###
forecast_values <- models %>%
  forecast(new_data = test_data)

### Forecast accuracy on test set
accuracy(forecast_values, test_data)


##### Comparison table#####
accuracy_table <- accuracy(forecast_values, test_data) %>%
  dplyr::select(.model, RMSE, MAE, MAPE, ACF1)

fit_table <- glance(models) %>%
  dplyr::select(.model, AIC, BIC)

### Join accuracy and fit summaries
model_summary <- left_join(accuracy_table, fit_table, by = ".model") %>%
  mutate(
    Num_Predictors = case_when(
      .model == "ARIMA_baseline" ~ 0,
      .model == "ARIMAX_model1" ~ 1,
      .model == "ARIMAX_model2" ~ 2,
      .model == "ARIMAX_model3" ~ 3,
      .model == "ARIMAX_model4" ~ 4,
      .model == "ARIMAX_model5" ~ 5,
      .model == "ARIMAX_model6" ~ 6,
      TRUE ~ NA_real_
    )
  ) %>%
  dplyr::select(Model = .model, Num_Predictors, RMSE, MAE, MAPE, ACF1, AIC, BIC)%>%
  mutate(
    AIC = round(AIC,1),
    BIC= round(BIC,1))%>%
  arrange(RMSE)

### View table and print to latex
print(model_summary)
model_summary %>%
  arrange(RMSE) %>%
  kable(format = "latex", booktabs = TRUE, digits = 4,
        caption = "Forecast Accuracy and Model Fit Comparison") 

### Model 3 report
models%>%
  dplyr::select(ARIMAX_model3)%>%
  report()

### Model 3 residual plots
models%>% 
  dplyr::select(ARIMAX_model3) %>% 
  gg_tsresiduals() 


### Forecast plots - ACTUAL FUTURE VALUES ### 
p1<-forecast_values %>%
  filter(.model=="ARIMA_baseline")%>%
  autoplot(log_prices, level = c(80, 95)) +
  labs(
    title = "Forecasts of log(Kbh_S) using ARIMA_baseline",
    y = "Log Price (DKK/m²)",
    x = "Time"
  )

p2<-forecast_values %>%
  filter(.model=="ARIMAX_model3")%>%
  autoplot(log_prices, level = c(80, 95)) +
  labs(
    title = "Forecasts of log(Kbh_S) using ARIMAX_model3 and known values of regressors",
    y = "Log Price (DKK/m²)",
    x = "Time"
  )

# Plot of all models using future values # 
forecast_values %>%
  autoplot(log_prices, level = c(80, 95)) +
  labs(
    title = "Forecasts of log(Kbh_S)",
    y = "Log Price (DKK/m²)",
    x = "Time"
  )


### Forecast using FORECASTED FUTURE VALUES ###

### modelling predictors
model_KbhØ <- train_data %>% model(Kbh_Ø = ARIMA(Kbh_Ø))
model_Frb  <- train_data %>% model(Frederiksberg = ARIMA(Frederiksberg))
model_KbhN <- train_data %>% model(Kbh_N = ARIMA(Kbh_N))

### forecasting predictors 
fc_KbhØ <- forecast(model_KbhØ, h = 20) %>%
  as_tibble() %>%
  dplyr::select(TID, Kbh_Ø = .mean)

fc_Frb <- forecast(model_Frb, h = 20) %>%
  as_tibble() %>%
  dplyr::select(TID, Frederiksberg = .mean)

fc_KbhN <- forecast(model_KbhN, h = 20) %>%
  as_tibble() %>%
  dplyr::select(TID, Kbh_N = .mean)

## combine forecasted predictors 
predictor_forecasts <- reduce(
  list(fc_KbhØ, fc_Frb, fc_KbhN),
  full_join,
  by = "TID"
)
predictor_forecasts <- predictor_forecasts %>%
  as_tsibble(index = TID)


### Forecast using ARIMAX_model3 with forecasted predictors
forecast_ex_ante <- models %>%
  dplyr::select(ARIMAX_model3, ARIMA_baseline) %>%
  forecast(new_data = predictor_forecasts)

### Plot forecast with forecasted future values 
p3<-forecast_ex_ante %>%
  filter(.model=="ARIMAX_model3")%>%
  autoplot(log_prices, level = c(80, 95)) +
  labs(
    title = "Forecasts of log(Kbh_S) using ARIMAX_model3 and forecasted values of regressors",
    y = "Log Price (DKK/m²)",
    x = "Time"
  )

### All test forecasts combined 
grid.arrange(p1,p2,p3,ncol=3)

### Accuracy of model 3 and baseline using predicted values
accuracy(forecast_ex_ante, test_data) %>%
  dplyr::select(Model = .model, RMSE, MAE, MAPE, ACF1)%>%
  arrange(RMSE) %>%
  kable(format = "latex", booktabs = TRUE, digits = 4,
        caption = "Forecast Accuracy - Forecasted regressors") 



#####################################
# Full Data ARIMA fit and forecast #

# Re-fit ARIMA baseline model to full dataset
full_model <- log_prices %>%
  model(ARIMA_baseline_full = ARIMA(Kbh_S))
report(full_model)

# Forecast 5 years (20 quarters) ahead
final_forecast <- full_model %>%
  forecast(h = 20)

# Plot
autoplot(final_forecast, log_prices) +
  labs(
    title = "Final 5-Year Forecast of Kbh_S ",
    y = "log(DKK/m²)",
    x = "Time"
  )

glance(full_model)


####################################################################################################################################
################################# END OF ARIMA ##################################################################
####################################################################################################################################

########################
### VAR selection #####
### LOG DIFF DATA #####
########################


### Setup
train_data <- log_prices_diff %>% filter(TID <= yearquarter("2019 Q4"))
test_data  <- log_prices_diff %>% filter(TID >= yearquarter("2020 Q1"))

train_matrix <- train_data %>%
  dplyr::select(-TID) %>%
  as.data.frame()

all_series <- colnames(train_matrix)
target <- "Kbh_S"
top_predictors <- c("Kbh_Ø", "Frederiksberg", "Kbh_N")  # Based on correlation

### Create combinations: all 2- and 3-variable combos including Kbh_S
combos <- c(
  lapply(top_predictors, function(x) c(target, x)),
  combn(top_predictors, 2, simplify = FALSE) |> lapply(function(x) c(target, x)),
  list(c(target, top_predictors))
)

### Get last log(Kbh_S) from training set and actual log(Kbh_S) from test set
last_log <- log_prices %>% filter(TID == max(train_data$TID)) %>% pull(Kbh_S)
actual_log <- log_prices %>% filter(TID >= yearquarter("2020 Q1")) %>% slice(1:20) %>% pull(Kbh_S)

### Rrepare result matrix
results <- data.frame(
  variables = character(),
  p = integer(),
  residual_p = numeric(),
  rmse = numeric(),
  mae = numeric(),
  AIC = numeric(),
  HQ = numeric(),
  SC = numeric(),
  FPE = numeric(),
  stringsAsFactors = FALSE
)

#Loop through combos
for (vars in combos) {
  sub_matrix <- train_matrix %>%
    dplyr::select(all_of(vars)) %>%
    filter(if_all(everything(), is.finite))
  
  try({
    cat("\n🔍 Trying model with:", paste(vars, collapse = ", "), "\n")
    
    var_selection <- VARselect(sub_matrix, lag.max = 5, type = "const")
    selected_p <- as.integer(var_selection$selection["AIC(n)"])
    cat("✔ Selected lag:", selected_p, "\n")
    
    if (is.na(selected_p) || !(selected_p %in% 1:ncol(var_selection$criteria))) {
      cat("⚠️ Invalid lag — skipping\n")
      next
    }
    
    var_model <- VAR(sub_matrix, p = selected_p, type = "const")
    cat("✔ VAR model fitted\n")
    
    resid_p <- serial.test(var_model, lags.pt = 16, type = "PT.asymptotic")$serial$p.value
    cat("✔ Residual p-value:", round(resid_p, 4), "\n")
    
    fc <- predict(var_model, n.ahead = 20)
    delta_fc <- fc$fcst$Kbh_S[, 1]
    log_fc <- cumsum(c(last_log, delta_fc))[-1]
    rmse <- sqrt(mean((actual_log - log_fc)^2))
    mae <- mean(abs(actual_log - log_fc))
    cat("✔ RMSE:", round(rmse, 4), "\n")
    
    ic <- var_selection$criteria
    aic <- ic["AIC(n)", selected_p]
    hq  <- ic["HQ(n)", selected_p]
    sc  <- ic["SC(n)", selected_p]
    fpe  <- ic["FPE(n)", selected_p]
    cat("✔ Criteria extracted\n")
    
    results <- rbind(results, data.frame(
      variables = paste(vars, collapse = ", "),
      p = selected_p,
      residual_p = round(resid_p, 4),
      rmse = round(rmse, 4),
      mae = round(mae, 4),
      AIC = round(aic, 2),
      HQ = round(hq, 2),
      SC = round(sc, 2),
      FPE = round(fpe, 6)
    ))
    
    cat("✅ Model stored\n")
  }, silent = FALSE)
}

### Print sorted results
results_sorted <- results %>% arrange(rmse)
print(results_sorted)
results_sorted %>%
  arrange(rmse) %>%
  kable(format = "latex", booktabs = TRUE,  digits = c(NA, 0, 4, 4, 4, 2, 2, 2, 8),
        caption = "VAR Model Comparison – Forecast Accuracy and Fit",
        row.names = FALSE)


######## SELECTED MODEL
### Identify best model's variable combination
best_vars <- strsplit(results_sorted$variables[1], ", ")[[1]]
best_p <- results_sorted$p[1]

### Subset the data accordingly
sub_matrix <- train_matrix %>%
  dplyr::select(all_of(best_vars)) %>%
  filter(if_all(everything(), is.finite))

### Fit the best VAR model and save it
best_model <- VAR(sub_matrix, p = best_p, type = "const")

### Extract residuals from the VAR model
residuals_var <- residuals(best_model)

### Plot residuals for Kbh_S
ts.plot(residuals_var[, "Kbh_S"], main = "Residuals of VAR model for Kbh_S", ylab = "Residuals")

### Autocorrelation plot
acf(residuals_var[, "Kbh_S"], main = "ACF of VAR Residuals for Kbh_S")

### Histogram
hist(residuals_var[, "Kbh_S"], breaks = 20, main = "Histogram of Residuals", xlab = "Residual", col = "gray")


######### PLOT VAR FORECAST ON TEST DATA 
### Extract forecast and confidence intervals
fc <- predict(best_model, n.ahead = 20, ci = 0.95)

### Pull point forecast and standard errors
point_fc <- fc$fcst$Kbh_S[, 1]
lower_95 <- fc$fcst$Kbh_S[, 2]
upper_95 <- fc$fcst$Kbh_S[, 3]

# approximate 80% CI (1.28 * SE instead of 1.96)
se <- (upper_95 - point_fc) / 1.96
lower_80 <- point_fc - 1.28 * se
upper_80 <- point_fc + 1.28 * se

# Reconstruct log-level forecast from differences
log_fc <- cumsum(c(last_log, point_fc))[-1]
log_lower_95 <- cumsum(c(last_log, lower_95))[-1]
log_upper_95 <- cumsum(c(last_log, upper_95))[-1]
log_lower_80 <- cumsum(c(last_log, lower_80))[-1]
log_upper_80 <- cumsum(c(last_log, upper_80))[-1]

#get faorecast dates
forecast_dates <- test_data$TID[1:20]

# assemble 
df_var_forecast <- tibble(
  TID = forecast_dates,
  value = log_fc,
  model = "VAR(2) with Kbh_Ø on test",
  lower_80 = log_lower_80,
  upper_80 = log_upper_80,
  lower_95 = log_lower_95,
  upper_95 = log_upper_95
)

# Actual historical values
df_actual <- log_prices %>%
  dplyr::select(TID, value = Kbh_S) %>%
  filter(TID <= max(forecast_dates)) %>%
  as_tibble()

# Plot
ggplot() +
  geom_line(data = df_actual, aes(x = TID, y = value), color = "black") +
  geom_ribbon(data = df_var_forecast, aes(x = TID, ymin = lower_95, ymax = upper_95), fill = "blue", alpha = 0.2) +
  geom_ribbon(data = df_var_forecast, aes(x = TID, ymin = lower_80, ymax = upper_80), fill = "blue", alpha = 0.4) +
  geom_line(data = df_var_forecast, aes(x = TID, y = value), color = "blue") +
  labs(
    title = "Forecasts of log(Kbh_S) using VAR(2) with Kbh_Ø",
    y = "Log Price (DKK/m²)",
    x = "Time"
  ) +
  theme_minimal()

######### Plot test forecast with ARIMA
# Extract ARIMA_baseline forecast
fc_arima <- forecast_values %>%
  filter(.model == "ARIMA_baseline")

# Extract 80% and 95% intervals separately
fc_arima_80 <- fc_arima %>%
  hilo(80) %>%
  mutate(lower_80 = `80%`$lower,
         upper_80 = `80%`$upper) %>%
  dplyr::select(TID, .mean, lower_80, upper_80)

fc_arima_95 <- fc_arima %>%
  hilo(95) %>%
  mutate(lower_95 = `95%`$lower,
         upper_95 = `95%`$upper) %>%
  dplyr::select(TID, lower_95, upper_95)

# Combine both intervals
df_arima_full <- fc_arima_80 %>%
  left_join(fc_arima_95, by = "TID") %>%
  transmute(
    TID,
    model = "ARIMA_baseline",
    value = .mean,
    lower_80,
    upper_80,
    lower_95,
    upper_95
  )


# Combine forecast datasets
df_bands <- bind_rows(
  as_tibble(df_arima_full),
  as_tibble(df_var_forecast)
)

# Plot - test combined
ggplot() +
  # Confidence bands
  geom_ribbon(data = df_bands, aes(x = TID, ymin = lower_95, ymax = upper_95, fill = model), alpha = 0.2) +
  geom_ribbon(data = df_bands, aes(x = TID, ymin = lower_80, ymax = upper_80, fill = model), alpha = 0.4) +
  # Forecast lines
  geom_line(data = df_bands, aes(x = TID, y = value, color = model, linetype = model), size = 1) +
  # Actual history
  geom_line(data = df_actual, aes(x = TID, y = value), color = "black", size = 1) +
  scale_color_manual(values = c("ARIMA_baseline" = "red", "VAR(2) with Kbh_Ø on test" = "blue")) +
  scale_fill_manual(values = c("ARIMA_baseline" = "red", "VAR(2) with Kbh_Ø on test" = "blue")) +
  scale_linetype_manual(values = c("ARIMA_baseline" = "solid", "VAR(2) with Kbh_Ø on test" = "dashed")) +
  labs(
    title = "Forecasts of log(Kbh_S): ARIMA vs. VAR",
    x = "Time",
    y = "Log Price (DKK/m²)",
    color = "Model",
    fill = "Model",
    linetype = "Model"
  ) +
  theme_minimal(base_size = 8)


##### FINAL VAR PREDICTION #####

### adjust to full dataset
train_data <- log_prices_diff
train_matrix <- train_data %>%
  dplyr::select(-TID) %>%
  as.data.frame()
last_tid<-log_prices %>% filter(TID == max(train_data$TID)) %>% pull(TID)

# Subset the data accordingly
sub_matrix <- train_matrix %>%
  dplyr::select(all_of(best_vars)) %>%
  filter(if_all(everything(), is.finite))

### Fit the best VAR model and save it - final
best_model_final <- VAR(sub_matrix, p = best_p, type = "const")

### AIC
VARselect(sub_matrix, lag.max = 2, type = "const")$criteria[1,2]

# Portmanteau test - Final 
serial.test(best_model_final, lags.pt = 16, type = "PT.asymptotic")

# Extract residuals from the VAR model - final
residuals_var <- residuals(best_model_final)

# Plot residuals for Kbh_S - final
ts.plot(residuals_var[, "Kbh_S"], main = "Residuals of VAR model for Kbh_S", ylab = "Residuals")

# Autocorrelation plot - final 
acf(residuals_var[, "Kbh_S"], main = "ACF of VAR Residuals for Kbh_S")

# Histogram
hist(residuals_var[, "Kbh_S"], breaks = 20, main = "Histogram of Residuals", xlab = "Residual", col = "gray")


######### PLOT FINAL VAR 
# Extract forecast and confidence intervals
fc <- predict(best_model_final, n.ahead = 20, ci = 0.95)

# Pull point forecast and standard errors
point_fc <- fc$fcst$Kbh_S[, 1]
lower_95 <- fc$fcst$Kbh_S[, 2]
upper_95 <- fc$fcst$Kbh_S[, 3]

# Approximate 80% CI (1.28 * SE instead of 1.96)
se <- (upper_95 - point_fc) / 1.96
lower_80 <- point_fc - 1.28 * se
upper_80 <- point_fc + 1.28 * se

# Reconstruct log-level forecast from differences
last_log <- log_prices %>% filter(TID == max(train_data$TID)) %>% pull(Kbh_S)
log_fc <- cumsum(c(last_log, point_fc))[-1]
log_lower_95 <- cumsum(c(last_log, lower_95))[-1]
log_upper_95 <- cumsum(c(last_log, upper_95))[-1]
log_lower_80 <- cumsum(c(last_log, lower_80))[-1]
log_upper_80 <- cumsum(c(last_log, upper_80))[-1]

### Find dates for forecast 
forecast_dates <- seq(as.Date(as.yearqtr(as.character(last_tid))) + months(3), by = "quarter", length.out = 20)
forecast_dates <- as.yearqtr(forecast_dates)

df_var_forecast <- tibble(
  TID = forecast_dates,
  model = "VAR(2): Kbh_Ø",
  value = log_fc,
  lower_80 = log_lower_80,
  upper_80 = log_upper_80,
  lower_95 = log_lower_95,
  upper_95 = log_upper_95
)

# 2. Actual historical values
df_actual <- log_prices %>%
  dplyr::select(TID, value = Kbh_S) %>%
  as_tibble()

# 3. Plot it all
ggplot() +
  geom_line(data = df_actual, aes(x = TID, y = value), color = "black") +
  geom_ribbon(data = df_var_forecast, aes(x = TID, ymin = lower_95, ymax = upper_95), fill = "blue", alpha = 0.2) +
  geom_ribbon(data = df_var_forecast, aes(x = TID, ymin = lower_80, ymax = upper_80), fill = "blue", alpha = 0.4) +
  geom_line(data = df_var_forecast, aes(x = TID, y = value), color = "blue") +
  labs(
    title = "Forecasts of log(Kbh_S) using VAR(2) with Kbh_Ø",
    y = "Log Price (DKK/m²)",
    x = "Time"
  ) +
  theme_minimal()



######### Plot with ARIMA
# Extract ARIMA_baseline final forecast
fc_arima <- final_forecast %>%
  filter(.model == "ARIMA_baseline_full")

# Extract 80% and 95% intervals separately
fc_arima_80 <- fc_arima %>%
  hilo(80) %>%
  mutate(lower_80 = `80%`$lower,
         upper_80 = `80%`$upper) %>%
  dplyr::select(TID, .mean, lower_80, upper_80)

fc_arima_95 <- fc_arima %>%
  hilo(95) %>%
  mutate(lower_95 = `95%`$lower,
         upper_95 = `95%`$upper) %>%
  dplyr::select(TID, lower_95, upper_95)

#  Combine both intervals
df_arima_full <- fc_arima_80 %>%
  left_join(fc_arima_95, by = "TID") %>%
  transmute(
    TID,
    model = "ARIMA_baseline_full",
    value = .mean,
    lower_80,
    upper_80,
    lower_95,
    upper_95
  )


# Combine forecast datasets
df_var_forecast <- df_var_forecast %>%
  mutate(TID = tsibble::yearquarter(as.character(TID)))
df_bands <- bind_rows(
  as_tibble(df_arima_full),
  as_tibble(df_var_forecast)
)

#  Plot
ggplot() +
  # Confidence bands
  geom_ribbon(data = df_bands, aes(x = TID, ymin = lower_95, ymax = upper_95, fill = model), alpha = 0.2) +
  geom_ribbon(data = df_bands, aes(x = TID, ymin = lower_80, ymax = upper_80, fill = model), alpha = 0.4) +
  # Forecast lines
  geom_line(data = df_bands, aes(x = TID, y = value, color = model, linetype = model), size = 0.7) +
  # Actual history
  geom_line(data = df_actual, aes(x = TID, y = value), color = "black", size = 0.7) +
  scale_color_manual(values = c("ARIMA_baseline_full" = "red", "VAR(2): Kbh_Ø" = "blue")) +
  scale_fill_manual(values = c("ARIMA_baseline_full" = "red", "VAR(2): Kbh_Ø" = "blue")) +
  scale_linetype_manual(values = c("ARIMA_baseline_full" = "solid", "VAR(2): Kbh_Ø" = "dashed")) +
  labs(
    title = "Forecasts of log(Kbh_S): ARIMA vs. VAR",
    x = "Time",
    y = "Log Price (DKK/m²)",
    color = "Model",
    fill = "Model",
    linetype = "Model"
  ) +
  theme_minimal(base_size = 13)

##########FINAL PLOT ARIMA AND VAR NOT LOG #########
####################################################
df_bands_exp <- df_bands %>%
  mutate(
    value = exp(value),
    lower_80 = exp(lower_80),
    upper_80 = exp(upper_80),
    lower_95 = exp(lower_95),
    upper_95 = exp(upper_95)
  )
df_actual_exp <- df_actual%>%
  mutate(
    value=exp(value)
  )
ggplot() +
  # Confidence bands
  geom_ribbon(data = df_bands_exp, aes(x = TID, ymin = lower_95, ymax = upper_95, fill = model), alpha = 0.2) +
  geom_ribbon(data = df_bands_exp, aes(x = TID, ymin = lower_80, ymax = upper_80, fill = model), alpha = 0.4) +
  # Forecast lines
  geom_line(data = df_bands_exp, aes(x = TID, y = value, color = model, linetype = model), size = 0.7) +
  # Actual history
  geom_line(data = df_actual_exp, aes(x = TID, y = value), color = "black", size = 0.7) +
  scale_color_manual(values = c("ARIMA_baseline_full" = "red", "VAR(2): Kbh_Ø" = "blue")) +
  scale_fill_manual(values = c("ARIMA_baseline_full" = "red", "VAR(2): Kbh_Ø" = "blue")) +
  scale_linetype_manual(values = c("ARIMA_baseline_full" = "solid", "VAR(2): Kbh_Ø" = "dashed")) +
  scale_y_continuous(labels = scales::comma)+
  labs(
    title = "Forecasts of Kbh_S: ARIMA vs. VAR",
    x = "Time",
    y = "Price (DKK/m²)",
    color = "Model",
    fill = "Model",
    linetype = "Model"
  ) +
  theme_minimal(base_size = 13)


##############################################################################################################
##############################################################################################################

