library(forecast)
library(stats)
library(tseries)
library(readxl)
library(tidyverse)
library(evir)
library(ismev)
library(copula)
library(VineCopula)

#1 Set the directory to save files e.g images, excel etc

path = "C:/Users/Zvinodashe/Desktop/Stock_Analysis_Final"
setwd(path)

#2 Retrieve exel file to get PriceData and Return Data

priceData       <- read_excel("priceData.xlsx")
priceData$X__1  <- NULL
Date            <- as.Date(priceData$ref.date, format="%Y/%m/%d")
priceData       <- data.frame(Date, priceData)
priceData$ref.date<- NULL
head(priceData)
tail(priceData)


returnData      <- as.data.frame(sapply(priceData[,-1], function(x) diff(log(x))))
Date            <- priceData$Date[-1]
retData         <- data.frame(Date, returnData)
head(retData)
tail(retData)


#1.  GEV Models for each of the 5 assets  "BAC", "F", "GE", "MSFT","T"

# Create negative return data by multiply by -1
negativeReturnData <- -returnData
head(negativeReturnData)
head(negativeReturnData$BAC)

# Fit GEV Models and Quantile Plots for 5 assets
BAC_GEV <- gev(negativeReturnData$BAC, block =21)  
BAC_GEV$par.ests
BAC_GEV$par.ses

F_GEV <- gev(negativeReturnData$F, block =21)
F_GEV$par.ests
F_GEV$par.ses

GE_GEV <- gev(negativeReturnData$GE, block =21)
GE_GEV$par.ests
GE_GEV$par.ses

MSFT_GEV <- gev(negativeReturnData$MSFT, block =21)
MSFT_GEV$par.ests
MSFT_GEV$par.ses

T_GEV <- gev(negativeReturnData$T, block =21)
T_GEV$par.ests
T_GEV$par.ses

# Quantile plots to compare fit of models-select which = 2
# plot(GEV_Object, which=2, optional params..)
plot(BAC_GEV, main = "BAC GEV Quantile Plot")
plot(F_GEV, main = "F GEV Quantile Plot")
plot(GE_GEV, main = "GE GEV Quantile Plot")
plot(MSFT_GEV, main = "MSFT GEV Quantile Plot")
plot(T_GEV, main = "T GEV Quantile Plot")


# Return level plots use ismev package as evirs 
# ..plots only Scatter and Quantile Plots
# NB// Ensure fit of gev.fit and gev produce same estimates
BAC_fit <- gev.fit(BAC_GEV$data)
gev.diag(BAC_fit)

F_fit <- gev.fit(F_GEV$data)
gev.diag(F_fit)

GE_fit <- gev.fit(GE_GEV$data)
gev.diag(GE_fit)

MSFT_fit <- gev.fit(MSFT_GEV$data)
gev.diag(MSFT_fit)

T_fit <- gev.fit(T_GEV$data)
gev.diag(T_fit)

head(returnData)
# Copulas for bivariate distributions of all pairs of assets
# Get an idea of which copula to fit usign VineCopula
# function can be used to avoid repetition below..!
# Also fit other copula to see what they give us
varBAC         <- pobs(returnData[,1])
varF           <- pobs(returnData[,2])
BAC_F_copula   <- BiCopSelect(varBAC, varF,family = NA)
print("BAC & F copula")
BAC_F_copula
BAC_F_copula$family
BAC_F_copula$par
BAC_F_copula$par2
# Also try Gumbel & clayton & Normal to see if above reasonable


varBAC         <- pobs(returnData[,1])
varGE          <- pobs(returnData[,3])
BAC_GE_copula  <- BiCopSelect(varBAC, varGE,family = NA)
print("BAC & GE copula")
BAC_GE_copula
BAC_GE_copula$family
BAC_GE_copula$par
BAC_GE_copula$par2

varBAC         <- pobs(returnData[,1])
varMSFT        <- pobs(returnData[,4])
BAC_MSFT_copula<- BiCopSelect(varBAC, varMSFT,family = NA)
print("BAC & MSFT copula")
BAC_MSFT_copula
BAC_MSFT_copula$family
BAC_MSFT_copula$par
BAC_MSFT_copula$par2

varBAC         <- pobs(returnData[,1])
varT           <- pobs(returnData[,5])
BAC_T_copula   <- BiCopSelect(varBAC, varT,family = NA)
print("BAC & T copula")
BAC_T_copula
BAC_T_copula$family
BAC_T_copula$par
BAC_T_copula$par2

varF           <- pobs(returnData[,2])
varGE          <- pobs(returnData[,3])
F_GE_copula    <- BiCopSelect(varF, varGE,family = NA)
print("F & GE copula")
F_GE_copula
F_GE_copula$family
F_GE_copula$par
F_GE_copula$par2

varF           <- pobs(returnData[,2])
varMSFT        <- pobs(returnData[,4])
F_MSFT_copula  <- BiCopSelect(varF, varMSFT,family = NA)
print("F & MSFT copula")
F_MSFT_copula
F_MSFT_copula$family
F_MSFT_copula$par
F_MSFT_copula$par2

varF           <- pobs(returnData[,2])
varT           <- pobs(returnData[,5])
F_T_copula   <- BiCopSelect(varF, varT,family = NA)
print("F & T copula")
F_T_copula
F_T_copula$family
F_T_copula$par
F_T_copula$par2

varGE          <- pobs(returnData[,3])
varMSFT        <- pobs(returnData[,4])
GE_MSFT_copula <- BiCopSelect(varGE, varMSFT,family = NA)
print("GE & MSFT copula")
GE_MSFT_copula
GE_MSFT_copula$family
GE_MSFT_copula$par
GE_MSFT_copula$par2

varGE          <- pobs(returnData[,3])
varT           <- pobs(returnData[,5])
GE_T_copula    <- BiCopSelect(varGE, varT,family = NA)
print("GE & T copula")
GE_T_copula
GE_T_copula$family
GE_T_copula$par
GE_T_copula$par2

varMSFT        <- pobs(returnData[,4])
varT           <- pobs(returnData[,5])
MSFT_T_copula  <- BiCopSelect(varMSFT, varT,family = NA)
print("F & GE copula")
MSFT_T_copula
MSFT_T_copula$family
MSFT_T_copula$par
MSFT_T_copula$par2


# fitting different copula to check the best based on AIC and ..
# ..to get parameters of the copula

gumbel_copula <- gumbelCopula(dim=2)
normal_copula <- normalCopula(dim=2)
t_copula      <- tCopula(dim=2)
frank_copula  <- frankCopula(dim=2)
joe_copula    <- joeCopula(dim=2)

#1. Fits for BAC & F 
data_BAC_F    <- as.matrix(cbind(varBAC,varF))
data_F_T      <- as.matrix(cbind(varF,varT))
data_BAC_GE   <- as.matrix(cbind(varBAC,varGE))
data_GE_MSFT  <- as.matrix(cbind(varGE,varMSFT))

t_fit         <- fitCopula(t_copula,
                           data_BAC_F,
                          method="ml")
gumbel_fit    <- fitCopula(gumbel_copula,
                           data_BAC_F,
                          method="ml")
normal_fit    <- fitCopula(normal_copula,
                           data_BAC_F,
                           method="ml")
frank_fit     <- fitCopula(frank_copula,
                           data_BAC_F,
                           method="ml")
joe_fit       <- fitCopula(joe_copula,
                         data_BAC_F,
                         method="ml")

AIC(t_fit)
AIC(gumbel_fit)
AIC(normal_fit)
AIC(frank_fit)
AIC(joe_fit)

coef(t_fit)
coef(gumbel_fit)
coef(normal_fit)
coef(frank_fit)
coef(joe_fit)

# Repeat above remaining 9 combinations of pairs of assets
gumbel_fit    <- fitCopula(gumbel_copula,
                           data_F_T,
                           method="ml")
coef(gumbel_fit)

gumbel_fit    <- fitCopula(gumbel_copula,
                           data_BAC_GE,
                           method="ml")
coef(gumbel_fit)

gumbel_fit    <- fitCopula(gumbel_copula,
                           data_GE_MSFT,
                           method="ml")
coef(gumbel_fit)
