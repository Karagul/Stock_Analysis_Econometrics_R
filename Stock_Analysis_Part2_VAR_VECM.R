library(dplyr)
library(timeSeries)
library(tsDyn)
library(tseries)
library(forecast)
library(stats)
library(tseries)
library(readxl)
library(gtools)
library(vars)
library(tidyverse)
library(urca)
library(lubridate)
library(reshape2)
library(ggfortify)


#1 Set the directory to save files e.g images, excel etc

path = "C:/Users/Zvinodashe/Desktop/Stock_Analysis_Final"
setwd(path)

#2 Retrieve exel file to get PriceData and Return Data

priceData       <- read_excel("priceData.xlsx")
priceData$X__1  <- NULL
Date            <- as.Date(priceData$ref.date, format="%Y/%m/%d")
priceData         <- data.frame(Date, priceData)
priceData$ref.date<- NULL
head(priceData)
tail(priceData)


returnData      <- as.data.frame(sapply(priceData[,-1], function(x) diff(log(x))))
Date            <- priceData$Date[-1]
retData         <- data.frame(Date, returnData)
head(retData)
tail(retData)

# Data that excludes the last 3 months for forecasting purposes
# Data is only up to Sep-30 as opposed to Dec-31
retData_excl_3  <- subset(retData,as.Date(Date) <= "2018-09-30")
priceDate_excl_3<- subset(priceData,as.Date(Date) <= "2018-09-30")
tail(retData_excl_3)
tail(priceDate_excl_3)


#(a) AIC to determine appropriate lag length
# We restrict to 9 lags aswedont want to many parameter estimates
VAR_model      <- VAR(retData_excl_3[,-1], 
                      lag.max =  12, 
                      type    = "none",
                      ic      = "AIC")
summary(VAR_model)

# Diagnise the above model with p = 6
# We maytestor serial autocorrelation using the Portmanteau test and Boxt.test
#Rerun var model with other suggested lags if H0 can be rejected at 0.05

serial.test(VAR_model, lags.pt = 12, type = "PT.asymptotic")

#----------OPTIONAL not sure SECTION BELOW ??-------------- 

# If we want we can fit various maybe get a better model than AIC one 
VAR_model_1     <- VAR(retData_excl_3[,-1], p = 1, type="none")
VAR_model_2     <- VAR(retData_excl_3[,-1], p = 2, type="none")
VAR_model_3     <- VAR(retData_excl_3[,-1], p = 3, type="none")
VAR_model_4     <- VAR(retData_excl_3[,-1], p = 4, type="none")
VAR_model_5     <- VAR(retData_excl_3[,-1], p = 5, type="none")
# try p = 7,8,9 etc
summary(VAR_model_1)
summary(VAR_model_2)
summary(VAR_model_3)
summary(VAR_model_4)
summary(VAR_model_5)
# all models below still have high chi-squared value 6 still best
serial.test(VAR_model_1, lags.pt = 12, type = "PT.asymptotic")
res <-residuals(VAR_model_5)
acf(res)

# -------------Optional above Will rely on AIC selected p lag ----------------------
# still p = 6 from above analysis is better lag selection plus we will 

#(b) Interpret Results Impulse Response Function
VAR_irf        <- irf(VAR_model, 
                      boot    = TRUE, 
                      ci      = 0.95,
                      n.ahead = 13)
plot(VAR_irf)

#(c) Interpret Results Forecast Variance Decomposition function 

VAR_fvd        <- fevd(VAR_model,
                       n.ahed = 13)

plot(VAR_fvd)


#(d) Autocorrelations and Cross-correlations i.e autocorrelogram, cross-correlogram etc
res           <- residuals(VAR_model)
acf(res)
pacf(res)
# Ideal to use function to try many combination, but 
# here will just change the pairs manually (1,2)(1,3)(1,4)(1,5)..(2,3)etc
plot(ccf(res[,4],
    res[,5],
    type="correlation", 
    lag.max=13
    ),
    main = "MSFT vs T")

#2.  VEC MODEL 

# Check all data is stationary of same order
priceData_firstDiff <- as.data.frame(sapply(priceData[,-1], function(x) diff(x)))
head(priceData_firstDiff)
tail(priceData_firstDiff)

# ADF test for stationarity null hypothesis unit root, alternative-stationary
adf.test(priceData_firstDiff$BAC)
adf.test(priceData_firstDiff$F)
adf.test(priceData_firstDiff$MSFT)
adf.test(priceData_firstDiff$GE)
adf.test(priceData_firstDiff$T)

# Apply the Johansen Test(eigen and trace both used) using lags from previous VAR model
johansenTest_Eigen <- ca.jo(priceData[,-1],
                            type="eigen",
                            K = 6,
                            ecdet="none",
                            spec ="longrun")
summary(johansenTest_Eigen)

johansenTest_Trace <- ca.jo(priceData[,-1],
                            type="trace",
                            K = 6,
                            ecdet="none",
                            spec ="longrun")
summary(johansenTest_Trace)

# Fit the VECM model with r -> number cointergrations from above
# Estimate either a VECM by Engle-Granger or Johansen (MLE) method.
# Basically, a VAR with 2 lags corresponds here to a VECM with 1 lag
# So we use VECM one order less than the VAR
# It is best practise to work with log of price then any values, 
# ..forecasts of estimate use value exp  2.18 to change them back
# ..owing to sheer laziness just worked with price data as is

VECM_model  = VECM(priceData[,-1],
                   lag = 5,
                   r=1,
                   include = "const",
                   estim   = "ML",
                   LRinclude = "none"
                   ) 

summary(VECM_model)

# Forecast Returns using VAR and VEC model 
# Last 30 days actual data below
last_3monthsR <- subset(retData,as.Date(Date) >= "2018-09-30" )
last_3monthsP <- subset(priceData, as.Date(Date) >= "2018-09-30")
size         <- length(last_3monthsR$BAC) # 62 data points last 3 months
size
# Predictions price using VECM model 
# will change for each asset 1 to 5 to get plot
prd_VECM     <-  predict(VECM_model, n.ahead = 62)
prd_VECM
length(last_3monthsR$Date)

plot(last_3monthsR$Date, 
     last_3monthsP$T, 
     type="l", col="blue",
     xlab = "3 Months Forecast T",
     ylab = "Price"
     )
lines(last_3monthsR$Date,
      prd_VECM[,5], 
      lty=1, col="red")
legend("topright", 
       legend = c("actual", "pred"), 
       col = c("blue", "red"),
       pch = c(17,19))

# Predictions returns using VAR model 
prd_VAR  <- predict(VAR_model, n.ahead = 62)
prd_BAC  <- prd_VAR$fcst$BAC[,1]
prd_F    <- prd_VAR$fcst$F[,1]
prd_GE   <- prd_VAR$fcst$GE[,1]
prd_MSFT <- prd_VAR$fcst$MSFT[,1]
prd_T    <- prd_VAR$fcst$T[,1]

prd_VAR

plot(last_3monthsR$Date, 
     last_3monthsR$T, 
     type="l", col="blue",
     xlab = "3 Months Forecast T",
     ylab = "Returns"
)
lines(last_3monthsR$Date,
      prd_T, 
      lty=1, col="red")
legend("topright", 
       legend = c("actual", "pred"), 
       col = c("blue", "red"),
       pch = c(17,19))

