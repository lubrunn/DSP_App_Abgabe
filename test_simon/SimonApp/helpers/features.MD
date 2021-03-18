###1. No feautures
The model predicts the dependent variables without the help of 
the additional variables specified in the second tab. 
The only information remaining are the time variables,
which include: ``day``,``week``,``month``,``quarter``,``season`` and ``year``.

<br>
###2. Forecasted feautures
The model predicts the dependent variable with the help of the specified
variables, which are forecasted for the selected time horizon. Each variable
is forecasted by using an ARIMA model (univariate). The package
``auto.arima`` selects the autoregressive and moving average automatically.
The final data frame consists of forecasted and time-specific variables-

