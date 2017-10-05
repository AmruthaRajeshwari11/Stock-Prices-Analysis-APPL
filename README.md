# Stock-Prices-Analysis-APPL
predicting stock prices of APPL using historical prices, market indicators, twitter indicators and stock prices of competing shares


Predicting the stock prices is challenging. This project, includes the following tasks

1. Downloading relevant data- AAPL historic stock prices, relevant market indicators of U.S(GDP,Unemploment Rate,Consumer Price,Producer Price -Technology Industry, Retail Sales Indices), Twitter News Feed Indicators, and Stock Prices of competitor companies( Google, HP and Microsoft in this case). AAPL Historic Revenue and Sales would have been key indicators, but could'nt find the data for free.

2.checking the predictability of stock prices of AAPL using twitter indicators. A regression model shows p<<0.01 indicating that the model is significant

3. Building an auto arima model for Stock price predictions and using this as Baseline model for comparisions

4. Building the autoarima model with twitter indicators as external regressors and testing the performance

5. Building the autoarima model with twitter and economic indicators as external regressors and testing the performance

6.Building a VAR model on transformed APPL, GOOG,HPQ and MSFT (transformed to make stationary) and predicting future values
