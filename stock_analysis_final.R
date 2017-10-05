###analysis of stock prices of APPL##

packages <- c("Quandl", "quantmod","TTR","xts","DMwR","data.table","forecast","plyr",
              "dplyr","zoo","reshape","vars")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}

library(Quandl)
library(quantmod)
library(TTR)
library(xts)
library(DMwR)
library(forecast)
library(plyr)
library(dplyr)
library(zoo)
library(reshape)
library(vars)
##########extracting the data for economic indicators
###period of interest being 01-04-2014 to 30-06-2017

economic_indicators = Quandl(c("FRED/GDP.1","FRED/UNRATE.1","FRED/FPCPITOTLZGUSA.1","FRED/PCU334111334111.1","FRED/RETAILSMSA.1"), start_date="2013-12-31", end_date="2017-06-30")
names_1=c("Date","GDP","Unemployment_Rate","Consumer_Price","Producer_Price_Industry","Retailer_Sales")
names(economic_indicators)=names_1
economic_indicators <- transform(economic_indicators, GDP = na.locf(GDP), Consumer_Price = na.locf(Consumer_Price))

#########extracting the stock volumes and prices of APPL and competitor comapanies
stock_prices=Quandl(c("WIKI/AAPL.11","WIKI/GOOG.11","WIKI/HPQ.11","WIKI/MSFT.11"), api_key="kwoAjFXJTuUxGyAZQD6Y",start_date="2014-01-01", end_date="2017-06-30")
stock_prices=stock_prices[complete.cases(stock_prices),]
#########extracting the twitter market indicators for APPL
news_feed=Quandl("NS1/AAPL_MM", api_key="kwoAjFXJTuUxGyAZQD6Y",start_date="2014-01-01", end_date="2017-06-30")


###################################TASK 1###########################################################
###Analysing the predictive power of twitter indicator on APPL stock price

stock_AAPL=Quandl("WIKI/AAPL",api_key="kwoAjFXJTuUxGyAZQD6Y",start_date="2014-01-01", end_date="2017-06-30")
stock_APPL_subset=stock_AAPL[,c(1,12)]
###merging twitter feed and stock_APPL so that only the overlapped Date points remain
analysis_data=merge(stock_APPL_subset,news_feed,by.x="Date",by.y="Date")
analysis_data_ts=read.zoo(analysis_data, format = "%Y-%m-%d")
#building the linear regression model
model1=lm(formula =`Adj. Close`~. ,data =analysis_data)
###looking at the summary and residuals
##summary with p-value: < 2.2e-16 makes the model significant
summary(model1)
resid(model1)
plot(density(resid(model1))) 
qqnorm(resid(model1))
qqline(resid(linear_reg))

#########################TASK2####################################
##1)building a arima time series model for predicting and testing on test dataset
Time_compare=stock_AAPL[,c("Date","Adj. Close")]
news_feed_1=news_feed[,-c(3,4)]

consolidated_data_1=merge(Time_compare,news_feed_1,by="Date")
covariate_matrix_1=as.matrix(consolidated_data_1[,c(3:5)])

#test and train split of independent and dependent variables
covariate_train_1=covariate_matrix_1[1:875,]
covariate_test_1=covariate_matrix_1[876:880,]
x_variable_1=ts(consolidated_data_1$`Adj. Close`,start=1)
x_train_1=x_variable_1[1:875]
x_test_1=x_variable_1[876:880]
#auto-arima with twitter indicators as external regressors
model_1=auto.arima(x_train_1,xreg=covariate_train_1)
y_test_1=forecast(model_1,xreg=covariate_test_1)
#forecasted variables
View(y_test_1)


##################################Task3############################
##building a model to predict stock prices using auto arima with market indicators and twitter
##indicators as external regressors

Time_compare=stock_AAPL[,c("Date","Adj. Close")]
#merging stock_AAPL with economic_idicators to bring them to one time scale
consolidated_data_2=merge(Time_compare,economic_indicators,by="Date",all.x = TRUE)
###removing the first 61 rows to simplify the process as they have NA's
consolidated_data_2=consolidated_data_2[-c(1:61),]
#imputing using na.locf so as to have market_indicators for each day to apply in the model
consolidated_data_2 <- transform(consolidated_data_2, GDP = na.locf(GDP),Unemployment_Rate=na.locf(Unemployment_Rate), Consumer_Price = na.locf(Consumer_Price),Producer_Price_Industry=na.locf(Producer_Price_Industry),Retailer_Sales=na.locf(Retailer_Sales))
###the market indicators have to be lagged by one time point because the indicators of the same day are not available for forecasting
consolidated_data_2=consolidated_data_2%>%mutate(lagged_GDP = lag(GDP, 1), lagged_Unemployment_Rate = lag(Unemployment_Rate, 1),
                                              lagged_Consumer_Price = lag(Consumer_Price, 1),lagged_Producer_Price_Industry=lag(Producer_Price_Industry,1),lagged_Retailer_Sales=lag(Retailer_Sales,1))
consolidated_data_2=consolidated_data_2[-1,-c(3:7)]

consolidated_data_2=merge(consolidated_data_2,news_feed,by="Date")
covariate_matrix_2=as.matrix(consolidated_data_2[,c(3:12)])
#test and train split of independent and dependent variables
covariate_train_2=covariate_matrix_2[1:813,]
covariate_test_2=covariate_matrix_2[814:818,]
x_variable_2=ts(consolidated_data_2$Adj..Close,start=1)
x_train_2=x_variable_2[1:813]
x_test_2=x_variable_2[814:818]
#auto-arima with external regressors(both market indices and twitter indices)
model_2=auto.arima(x_train,xreg=covariate_train_2)
y_test_2=forecast(model_2,xreg=covariate_test_2)
#forecasted variables
View(y_test_2)

#####################################task-4####################
#builiding a VAR model to explain the prices of APPL stocks using its competitors

##converting them to stationary by appling transfomations
stock_prices_diff=stock_prices[-1,]
stock_prices_diff[,2]=diff(log(stock_prices[,2]))
stock_prices_diff[,3]=diff(log(stock_prices[,3]))
stock_prices_diff[,4]=diff(log(stock_prices[,4]))
stock_prices_diff[,5]=diff(log(stock_prices[,5]))


##splitting into train and test 
train_var_data=stock_prices_diff[1:817,]
test_var_data=stock_prices_diff[818:822,]

### building VAR model on train

##selecting the optimum lag based on the results of VARselect
VARselect(train_var_data[,-1], lag.max = 10, type = "both")
##changing the p value accordingly to build the model
var_model=VAR(train_var_data[,-1], p = 1,type="both")
var_model
#Forecasting next 5 days using the VAR model built
prd_var<- predict(var_model, n.ahead = 5)
print(prd_var)
plot(prd_var, "single")
#will have to transform back to match the original scale


















