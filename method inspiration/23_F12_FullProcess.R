## ============================================================
## 1. Load packages
## ============================================================

## Load packages for time-series analysis, forecasting, unit-root tests,
## structural break tests, and tidy time-series workflows.
library(tseries)
library(forecast)
library(urca)
library(xts)
library(fpp3)
library(AER)
library(strucchange)


## ============================================================
## 2. Set working directory
## ============================================================

## Set path: personalize your working directory.
path <- 'C:/Users/tce.eco/OneDrive - CBS - Copenhagen Business School/01 DOCUMENTS/02 TEACHING/02 PREDICTIVE ANALYTICS - F26/Lectures/Lecture 12'
setwd(path)

## Check that the working directory has been set correctly.
getwd()


## ============================================================
## 3. Load and prepare data
## ============================================================

## Load monthly US house price data from CSV.
## The data is converted to a monthly time-series object and then to a tsibble.
## The relevant house price variable is renamed hp and selected for analysis.
ushp <- as_tsibble(ts(read.csv("US_HP_DI.csv", sep = ","), frequency = 12, start = c(1987, 1)), 
                      index = Date, pivot_longer = FALSE) %>% 
  mutate(hp = CSUSHPISA) %>%
  dplyr::select(hp)


## ============================================================
## 4. Check for possible Box-Cox transformation
## ============================================================

## Estimate the Box-Cox transformation parameter using Guerrero's method.
## This checks whether a variance-stabilising transformation may be useful.
lambda <- ushp |>
  features(hp, features = guerrero) |>
  pull(lambda_guerrero)

## ============================================================
## 5. Preliminary visual analysis
## ============================================================

## Plot the time series in levels.
autoplot(ushp) + 
  ggtitle("Time series")

## Plot the autocorrelation function.
## ggAcf() is the ggplot-based ACF plot from the fpp3 workflow.
ggAcf(ushp) + 
  ggtitle("Correlgram")

## Plot seasonal patterns by year.
## gg_season() overlays years to show whether months behave systematically differently.
gg_season(ushp, year.labels = TRUE, year.labels.left = TRUE) + 
  ggtitle("Seasonal plot 1")

## Plot seasonal subseries by month.
## gg_subseries() shows the development within each month across years.
gg_subseries(ushp) + 
  ggtitle("Seasonal plot 2")

## Plot the series against its lagged values.
## gg_lag() helps detect serial dependence and nonlinear lag relationships.
## do.lines = F suppresses connecting lines between points.
gg_lag(ushp, do.lines = F) + 
  ggtitle("Lag plot")


## ============================================================
## 6. Decompose the series
## ============================================================

## Decompose the series using X-13ARIMA-SEATS.
## model() estimates the decomposition model, and components() extracts
## trend, seasonal, and irregular components.
seats_dcmp <- ushp %>%
  model(seats = X_13ARIMA_SEATS(hp ~ x11())) %>%
  components()

## Plot the decomposed components.
## labs() adds a descriptive title to the ggplot output.
autoplot(seats_dcmp) +
  labs(title =
         "Decomposition of total US house price using SEATS")


## ============================================================
## 7. Unit-root tests on levels
## ============================================================

## Test for unit roots in the level of the series, using the urca package which
## allows for more control and more detailed output.
## ADF tests are run with trend, drift, and no deterministic terms.
## KPSS tests are run under trend-stationarity and level-stationarity assumptions.
summary(ur.df(as.ts(ushp), type = 'trend', lag = 24, selectlags = 'AIC'))
summary(ur.kpss(as.ts(ushp), type = 'tau'))
summary(ur.df(as.ts(ushp), type = 'drift', lag = 24, selectlags = 'AIC'))
summary(ur.kpss(as.ts(ushp), type = 'mu'))
summary(ur.df(as.ts(ushp), type = 'none', lag = 24, selectlags = 'AIC'))


## ============================================================
## 8. First differencing and stationarity tests
## ============================================================

## Create the first difference of house prices.
## This is used to remove a possible stochastic trend.
ushp %>%
  mutate(d.hp = difference(hp)) -> ushp

## Test whether the first-differenced series is stationary.
## Missing values created by differencing are removed before the ADF test.
summary(ur.kpss(ushp %>% select(d.hp) %>% as.ts(), type = 'mu'))
summary(ur.df(ushp %>% select(d.hp) %>% filter(!is.na(d.hp)) %>% as.ts(), type = 'none', lag = 24, selectlags = 'AIC'))

## Series appears stationary at this stage, so the next step is to test for breaks.


## ============================================================
## 9. Structural break test on first-differenced series
## ============================================================

## Prepare data for the structural break test.
## Lag0 is the current first difference, and Lag1 is its lagged value.
ushp.ts <- cbind(
  Lag0 = ushp %>% select(d.hp) %>% filter(!is.na(d.hp)) %>% as.ts(),
  Lag1 = stats::lag(ushp %>% select(d.hp) %>% filter(!is.na(d.hp)) %>% as.ts())
)

## Compute recursive F statistics for structural change.
## The model tests whether the intercept and lag coefficient are stable over time.
## from = 0.10 trims 10% of observations at the beginning and end.
qlr <- Fstats(Lag0 ~ 1 + Lag1, data = ushp.ts, from = 0.10)   

## Save the F-statistics plot as a PDF file.
pdf("FStats.pdf")
plot(qlr, alpha = 0.1)
dev.off()

## Run the supF test for structural instability and print the result.
test <- sctest(qlr, type = "supF")      
test

## Estimate potential breakpoints using a 1% significance level.
breakpoints(qlr, alpha = 0.01)

## Plot the F statistics and add estimated breakpoints as vertical lines.
plot(qlr, alpha = 0.1, main = "F Statistics")
lines(breakpoints(qlr))  


## ============================================================
## 10. Structural break test on post-2008 subsample
## ============================================================

## Restrict the time-series object to observations from 2008 onward.
## This checks whether instability remains in the post-2008 period.
ushp.ts = window(ushp.ts, start = 2008)

## Re-run the structural break test on the restricted sample.
qlr <- Fstats(Lag0 ~ Lag1, data = ushp.ts, from = 0.15)   

## Save the restricted-sample F-statistics plot as a PDF file.
pdf("FStats2.pdf")
plot(qlr)
dev.off()

## Run the supF test and estimate breakpoints for the restricted sample.
test <- sctest(qlr, type = "supF")      
test
breakpoints(qlr, alpha = 0.1)

## Plot the restricted-sample F statistics and add estimated breakpoints.
plot(qlr, alpha = 0.1, main = "F Statistics")
lines(breakpoints(qlr))  


## ============================================================
## 11. Re-check stationarity in restricted sample
## ============================================================

## Restrict the tsibble to observations from January 2007 onward.
## Then re-check the unit-root evidence in the restricted sample.
ushp %>%
  filter_index("2007 M1" ~ .) -> ushp

summary(ur.kpss(ushp %>% select(d.hp) %>% as.ts(), type = 'mu'))
summary(ur.df(ushp %>% select(d.hp) %>% filter(!is.na(d.hp)) %>% as.ts(), type = 'none', lag = 24, selectlags = 'AIC'))

## Because the unit-root evidence is conflicting, difference the series once more.
ushp %>%
  mutate(d2.hp = difference(d.hp)) -> ushp

## Test stationarity of the second-differenced series.
summary(ur.kpss(ushp %>% select(d2.hp) %>% as.ts(), type = 'mu'))
summary(ur.df(ushp %>% select(d2.hp) %>% filter(!is.na(d2.hp)) %>% as.ts(), type = 'none', lag = 24, selectlags = 'AIC'))

## The second-differenced series appears stationary, so model estimation follows.


## ============================================================
## 12. ACF and PACF for model identification
## ============================================================

## Inspect the ACF and PACF of the second-differenced series.
## ggAcf() helps identify possible MA terms.
## ggPacf() helps identify possible AR terms.
plot.3 <- ggAcf(ushp %>% select(d2.hp)) + 
  ggtitle("ACF")
print(plot.3)

plot.4 <- ggPacf(ushp %>% select(d2.hp)) + 
  ggtitle("PACF")
print(plot.4)


## ============================================================
## 13. Estimate ARIMA and ETS models on original data
## ============================================================

## Estimate forecasting models using data up to December 2017.
## ARIMA(hp) lets the algorithm select an ARIMA specification.
## arma_guessed imposes a manually chosen seasonal ARIMA model.
## ETS(hp) estimates an exponential smoothing model.
models <- ushp %>%
  filter_index(. ~ "2017M12") %>%
  model(arma = ARIMA(hp),
        arma_guessed = ARIMA(hp ~ pdq(1, 2, 1) + PDQ(1, 0, 1)),
        ets = ETS(hp))


## ============================================================
## 14. Inspect model estimates and residual diagnostics
## ============================================================

## Print model summaries and information criteria.
models %>% select(arma) %>% report()
models %>% select(arma_guessed) %>% report()
models %>% select(ets) %>% report()

## Plot residual diagnostics for the manually specified ARIMA model.
## gg_tsresiduals() shows residual time plot, residual ACF, and distribution plot.
## type = "innovation" uses one-step-ahead forecast errors.
models %>% 
  select(arma_guessed) %>% 
  gg_tsresiduals(type = "innovation")

## Plot residual diagnostics for the automatically selected ARIMA model.
models %>% 
  select(arma) %>% 
  gg_tsresiduals(type = "innovation")

## Plot residual diagnostics for the ETS model.
models %>% 
  select(ets) %>% 
  gg_tsresiduals(type = "innovation")

## Residual diagnostics are not fully satisfactory, so try a Box-Cox transformation.


## ============================================================
## 15. Estimate ARIMA and ETS models on Box-Cox transformed data
## ============================================================

## Estimate the same model types on Box-Cox transformed data.
## This checks whether stabilising the variance improves residual behaviour.
modelsg <- ushp %>%
  filter_index(. ~ "2017M12") %>%
  model(arma = ARIMA(box_cox(hp, lambda)),
        arma_guessed = ARIMA(box_cox(hp, lambda) ~ pdq(1, 2, 1) + PDQ(1, 0, 1)),
        ets = ETS(box_cox(hp, lambda)))


## ============================================================
## 16. Inspect transformed-data models
## ============================================================

## Print summaries for the transformed-data models.
modelsg %>% select(arma) %>% report()
modelsg %>% select(arma_guessed) %>% report()
modelsg %>% select(ets) %>% report()

## Plot residual diagnostics for the transformed manually specified ARIMA model.
modelsg %>% 
  select(arma_guessed) %>% 
  gg_tsresiduals(type = "innovation")

## Plot residual diagnostics for the transformed automatic ARIMA model.
modelsg %>% 
  select(arma) %>% 
  gg_tsresiduals(type = "innovation")

## Plot residual diagnostics for the transformed ETS model.
modelsg %>% 
  select(ets) %>% 
  gg_tsresiduals(type = "innovation")

## The transformation does not improve the diagnostics enough,
## so continue with models estimated on the original data.


## ============================================================
## 17. Ljung-Box residual autocorrelation tests
## ============================================================

## Run Ljung-Box tests for residual autocorrelation.
## augment() extracts residuals from each fitted model.
## features(..., ljung_box) computes the test statistic and p-value.
## lag = 20 sets the number of autocorrelations tested.
## dof adjusts for the number of estimated model parameters.
augment(models %>% 
          select(arma_guessed))  %>%
  features(.resid, features = ljung_box, lag = 20, dof = 6)

augment(models %>% 
          select(arma))  %>%
  features(.resid, features = ljung_box, lag = 20, dof = 6)

augment(models %>% 
          select(ets))  %>%
  features(.resid, features = ljung_box, lag = 20, dof = 6) 

## Ljung-Box tests are acceptable, so produce forecasts.


## ============================================================
## 18. Forecast holdout period
## ============================================================

## Forecast the holdout period from January 2018 onward.
## The future dates are supplied through the observed holdout sample.
forc <- models %>% 
  forecast(ushp %>% 
             select(hp) %>% 
             filter_index("2018 M1" ~ .))


## ============================================================
## 19. Plot forecasts
## ============================================================

## Plot forecasts from the automatically selected ARIMA model
## against the full observed series.
## filter(.model == "arma") keeps only the ARIMA forecasts.
## autoplot() overlays forecasts and actual observations.
forc %>%
  filter(.model == "arma") %>%
  autoplot(ushp %>% 
             select(hp)) +
  labs(title = "Models")

## Plot forecasts from the ETS model against the full observed series.
forc %>%
  filter(.model == "ets") %>%
  autoplot(ushp %>% 
             select(hp)) +
  labs(title = "Models")


## ============================================================
## 20. Compare forecast accuracy
## ============================================================

## Compare forecast accuracy against the actual holdout observations.
## accuracy() reports measures such as RMSE, MAE, MAPE, and MASE.
accuracy(forc, ushp %>% 
           select(hp)) 

## The automatically selected ARIMA model is preferred because it is
## more parsimonious and has lower AIC/BIC.