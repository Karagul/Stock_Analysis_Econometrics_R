library(ggplot2)
library(tsDyn)
library(tseries)
library(forecast)
library(moments)
library(kdensity)
library(stats)
library(xlsx)
library(tseries)
library(BatchGetSymbols)
library(readxl)
library(reshape2)
library(gtools)


#1 Set the directory to save files e.g images, excel etc

path = "C:/Users/Zvinodashe/Desktop/Stock_Analysis_Final"
setwd(path)

#2 Define stocks and variables for analysis- can change here outputs autochange

stocks          <- c("BAC", "F", "GE", "MSFT","T")
numStocks       <- length(stocks)


#3 Define time period for analysis
startDate       <- "2000-01-01"
endDate         <- "2018-12-31"
frequency       <- "daily"

#4 Fetch data from Yahoo using BatchGetSymbols package into a dataframe

getData         <- function(stocks,
                            startDate,
                            endDate) {
  
  output        <- BatchGetSymbols(tickers   = stocks, 
                                 first.date   = startDate,
                                 last.date    = endDate, 
                                 freq.data    = frequency,
                                 cache.folder = file.path(tempdir(),
                                                          'BGS_Cache') ) 
  # output is long data format needs converting to wide format
  
  tickersLong   <- output$df.tickers
  tickersWide   <- reshape.wide(tickersLong)
  priceData     <- tickersWide$price.adjusted
  
  # write priceData to exel file 
  write.xlsx(priceData, 
             file      = "priceData.xlsx",
             sheetName = "priceData", 
             append    = FALSE)
  return(tickersWide)
  
}
tickersWide     <- getData(stocks, startDate, endDate)

#5 Retrieve exel file and create return data and squared returns data

priceData       <- read_excel("priceData.xlsx")
priceData$X__1  <- NULL
head(priceData)

returnData      <- as.data.frame(sapply(priceData[,-1], function(x) diff(log(x))))
Date            <- priceData$ref.date[2:length(priceData$ref.date)]
    # Below dataframe of returns I(1) differenced log returns
retData         <- data.frame(Date, returnData)
head(retData)

    # Below returns squared data based on ret*ret or ret^2
SquareData      <- as.data.frame(sapply(retData[,-1], function(x) x*x))
retSqData       <- data.frame(Date, SquareData)
head(retSqData)

#6 Time Series Plots for price data on single plot

priceDataPlots  <- function(data) {
  
    #transform dataframe so data can be grouped
    dataMelted  <- melt(data, id.var= 'ref.date')
    
    ggplot(dataMelted, 
           aes(x=ref.date, 
               y=value, 
               col=variable)
                         ) + 
    geom_line() +
    theme_minimal()
    ggsave("Time_Series_Plots_Price.png")
  
}
priceDataPlots(priceData)

#7 Time Series Plots for return data on seperate plots

retDataPlots    <- function(stocks, data, folder) {
  
  #create directory to save images
  dir.create(file.path(folder))
  
  loop          <- 2:(numStocks+1)
  
  for (i in loop) {
    
    plot        <- data[,i]
    y           <- paste(stocks[(i-1)],"-" ,"Returns", sep="")
    ggplot(data=data) +
    geom_line(mapping = aes(x=Date,y=plot), color = "lightblue") +
    labs(x="Date from 2000-01-01 to 2018-12-31", 
         y=y) +
    theme_minimal()
    
    filename    <- paste(folder,"/",stocks[i-1],"_Returns_Plot.jpeg", sep="")
    
    ggsave(filename)
  }
  
  
  
}
retDataPlots(stocks,retData,"retDataPlots")

#8 Statistics for the dataset

statsData       <- function(stocks, data) {
  
  stats         <- c("min", "max", "mean", "sd",
                     "kurt", "skew")
  statsDF       <- data.frame(stats)
  loop          <- 2:(numStocks+1)
  for (i in loop) {
    
    min         <- min(data[,i])
    max         <- max(data[,i])
    mean        <- mean(data[,i])
    sd          <- sd(data[,i])
    kurt        <- kurtosis(data[,i])
    skew        <- skewness(data[,i])
    vector      <- c(min, max, mean, sd, kurt, skew)
    statsDF[stocks[i-1]] <- vector
    
  }
  # Write results of stats for data to exel file 
  write.xlsx(statsDF, 
             file      = "statsDF.xlsx",
             sheetName = "stats", 
             append    = FALSE)

}
statsData(stocks, retData)
statsData       <- read_excel("statsDF.xlsx")
statsData


#9  Kernel density Plots comparison with nearest normal distribution

densityPlots    <- function(stocks, data, newFolder) {
  
  dir.create(file.path(newFolder))
  loop          <- 2:(numStocks+1)
  
  for(i in loop) {
    
    fileName    <- paste(newFolder,"/",stocks[i-1],"_Kernel_Density_Plot.jpeg", sep="")
    jpeg(fileName,width = 500, height = 500)
      # Histogram and Density Plots
      hist(data[,i],
           breaks=64,
           freq=FALSE,
           main=stocks[i-1],
           xlab=paste("Daily Log Returns ", stocks[i-1],sep=""),
           col = "pink")
      
      kDensity = kdensity(data[,i], start="normal")
      
      # add density distribution
      lines(kDensity,
            col="blue",
            lwd= 1)
      
      # add closest normal distribution
      lines(kDensity,
            plot_start = TRUE,
            col = "red",
            lwd = 1)
    dev.off()

  }
  
}
densityPlots(stocks, retData, "kDensityPlots")


#10  Normal Quantile_Quantile Plots analysis

qqPlots         <- function(newFolder) {  
  
  dir.create(file.path(newFolder))
  loop          <- 2:(numStocks+1)
  
  for (i in loop) {
    
    vector      <- unlist(retData[,i])
    meanVector  <- as.numeric(statsData[1,1+i])
    sdVector    <- as.numeric(statsData[4,1+i])
    standardised<- (vector-meanVector)/sdVector
    
    fileName    <- paste(newFolder,"/",stocks[i-1],"_Kernel_Density_Plot.jpeg", sep="")
    jpeg(fileName,width = 500, height = 500)
    
        qqnorm(standardised,
               main     = paste(stocks[i-1],"-","Normal QQ-Plot (i)",sep=""),
               pch      = 1
               )
        
        qqline(standardised,
               distribution = qnorm,
               col = "steelblue", 
               lwd = 2)
     dev.off() 
  }
  
}
qqPlots("QQ-Plots")


#11 Calculate correlations of assets 
numericData    <- retData[,2:6]
corrMatrix     <- cor(numericData)
write.xlsx(corrMatrix, 
           file      = "corrMatrix.xlsx",
           sheetName = "Corerelation", 
           append    = FALSE)


#12 Scatterplots for each pairs of assets
sizeDataPoints  <- length(retData$BAC)

scatterPlots    <- function(newFolder,percentData) {
  
  # Change percentData to get clearer plots using fewer datapoints
  combinations  <- combn(1:numStocks, 2)
  loop          <- 1:(length(combinations)/2) 
  dir.create(file.path(newFolder))
  numPoints     <- round((percentData/100)*sizeDataPoints)
  for(i in loop)  {
    
    pair        <- combinations[,i]
    one         <- pair[1]
    two         <- pair[2]
    fileName    <- paste(newFolder,"/",stocks[one],"-vs-",stocks[two],"_ScatterPlot.jpeg", sep="")
    jpeg(fileName,width = 500, height = 700)
    plot(retData[1:numPoints,one+1],
         retData[1:numPoints,two+1],
         xlab = paste(stocks[one]," Returns",sep=""),
         ylab = paste(stocks[two]," Returns",sep=""),
         col = "blue")
    dev.off()
    
  }
  
}
scatterPlots("scatterPlots", 25)

#13 Autocorrelation Plots + Stationarity Tests Returns and ReturnsSquaredData 

acfPacfPlots    <- function(dataIn,folderName) {
  
  dir.create(file.path(folderName))
  
  loop          <- 2:(numStocks+1)
  
  for (i in loop) {
    
    file_handle <- file(paste(folderName,"/",stocks[i-1],"-adf.txt",sep=""))
    data_ts     <- ts(data = dataIn[,i],
                      frequency=365.25,
                      start    = c(2000,01,01))
    data_ts     <- window(data_ts,start=2010)
    fileName    <- paste(folderName,"/",stocks[i-1],"_.jpeg", sep="")
    
    jpeg(fileName,width=550, height=250)
      par(mfrow=c(1,2))
      acf(data_ts)
      pacf(data_ts)
    dev.off()
    writeLines(paste(adf.test(data_ts),"\n",sep=""), 
               file_handle)
    close(file_handle)
    
  } 
  
  
}
acfPacfPlots(priceData,"autocorr_price")
acfPacfPlots(retData, "autocorr_ret")
acfPacfPlots(retSqData,"autcorr_ret_Sq")

#14 Fitting ARMA models to Returns and Returns Squared Data
#   From analysis our data has low autocorrelations so will only 
#  ..consider a lagset of values (p,q) from 0-3 and test all 
#  ..possible permuations of (p,q) from lagset fitting models on this
#  ..then find the best model from the statistics and test on each
#  ..best order will be in variable best_order and lowest AIC in min_aic
#  ..Box test done on the best model to check for residuals 

lagset  <- 0:3
options <- permutations(n=length(lagset), r=2, v=lagset,repeats.allowed =T)
size    <- length(options)/2

armaModel <- function(data, outputFolder, indexAsset) {
  
  file_handle <- file(paste(outputFolder,"/",stocks[indexAsset],".txt",sep=""))
      
  data_ts     <- ts(data = data[,indexAsset+1],
                        frequency=365.25,
                        start    = c(2000,01,01))
  data_ts     <- window(data_ts,start=2010)
      
  min_aic     <- Inf
  best_order  <- c(3,3,3)
      
  sink(file = file_handle, append = TRUE, type = "output",
           split = FALSE)
      
      for (i in 1:size) {
        
        p = as.numeric(as.character(options[i,][1]))
        q = as.numeric(as.character(options[i,][2]))
        order = c(p,0,q)
        fitted_ARMA <- arima(data_ts,
                             order =order,
                             method="ML")
        curr_aic    <- AIC(fitted_ARMA)
        if(curr_aic < min_aic) {
          min_aic   <- curr_aic
          best_order<- order
        }
        
        summary(fitted_ARMA)
        
        
      }
      
      print(paste("Best model is ", "(",toString(best_order),")" ," with AIC ", min_aic, sep=""))
      best_ARMA <- arima(data_ts, order=best_order, method="ML")
      summary(Box.test(best_ARMA$residuals,lag=12))
      close(file_handle)
      sink()

}

outputFolder1 ="ret_ARMA"
outputFolder2 = "retSq_ARMA"
dir.create(outputFolder1) 
dir.create(outputFolder2)

# Note! Struggling with for loop need to relook this code?????????

# returns model for all 5 assets
armaModel(retData, outputFolder1,1)
armaModel(retData, outputFolder1,2)
armaModel(retData, outputFolder1,3)
armaModel(retData, outputFolder1,4)
armaModel(retData, outputFolder1,5)

# returns squared model for all 5 assets
armaModel(retSqData, outputFolder2,1)
armaModel(retSqData, outputFolder2,2)
armaModel(retSqData, outputFolder2,3)
armaModel(retSqData, outputFolder2,4)
armaModel(retSqData, outputFolder2,5)
