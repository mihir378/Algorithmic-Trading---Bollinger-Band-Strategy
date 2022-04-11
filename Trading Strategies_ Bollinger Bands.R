###### STRATEGIES w BOLLINGER BANDS ######

#1. Buy When 2 green break the upper bound and sell when MA crossed from above 
#2. Sell when 2 red break MSFTer bound and buy when MA crossed from beMSFT

###### Strategy 1 - Buying Strategy ######
library(xts)
library(PerformanceAnalytics)
library(ggplot2)
library(quantmod)

################### BB Long Strategy ###################
getSymbols("MSFT")
price <- Cl(MSFT)
bb <- BBands(price,n=20,sd=2,maType = "SMA")

Close <- Cl(MSFT)
Open <- Op(MSFT)

qty <- 1
day <- 20 

signal <- c()    
signal[1:(day+2)] <- 0 

wealth <-c()
wealth[1:(day+2)] <- 100  

return<-c()                  
return[1:(day+2)] <- 0

profit <-c()
profit[1:(day+2)] <- 0

for (i in (day+2): length(price)){
  if (bb$up[i-1] < price[i-1] && bb$up[i] < price[i]){  
    signal[i] <- 1
  } else if (bb$up[i-1] > price[i-1] && bb$mavg[i] > price[i]){            #bb$up[i-1] > price[i-1] && bb$mavg[i] > price[i]
                                                                           #bb$up[i-1] > price[i-1] && (Open[i]/Close[i]) >= 0.8
    signal[i] <- 0
  } else {         
    signal[i] <- signal[i-1]
  }
}
signal<-reclass(signal,price)


trade <- Lag(signal)
for (i in (day+2):length(price)){
  profit[i] <- qty * trade[i] * (Close[i] - Open[i])  
  wealth[i] <- wealth[i-1] + profit[i]
  return[i] <- (wealth[i] / wealth[i-1]) -1  
}
ret.bb1<-reclass(return,price)

Return.cumulative(ret.bb1,geometric = TRUE)
Return.annualized(ret.bb1, geometric = TRUE)
SemiSD(ret.bb1)

###### Charting #####
chartSeries(MSFT,
            subset="2021-01::2022-04-09",
            theme=chartTheme('white'))
addBBands(n=20,sd=2,maType="SMA")
addTA(signal,type='S',col='red')

################### BB Short Strategy ###################
qty <- 1
day <- 20 

signal <- c()    
signal[1:(day+2)] <- 0 

wealth <-c()
wealth[1:(day+2)] <- 100  

return<-c()                  
return[1:(day+2)] <- 0

profit <-c()
profit[1:(day+2)] <- 0

for (i in (day+2): length(price)){
  if (bb$dn[i-1] > price[i-1] && bb$dn[i] > price[i]){  
    signal[i] <- 1
  } else if (bb$dn[i-1] < price[i-1] && bb$mavg[i] < price[i]){         #bb$dn[i-1] < price[i-1] && bb$mavg[i] < price[i]
                                                                        #bb$dn[i-1] < price[i-1] && (Open[i]/Close[i]) < 0.7
    signal[i] <- 0
  } else {         
    signal[i] <- signal[i-1]
  }
}
signal<-reclass(signal,price)

Close <- Cl(MSFT)
Open <- Op(MSFT)
trade <- Lag(signal)
for (i in (day+2):length(price)){
  profit[i] <- qty * trade[i] * (Open[i] - Close[i])  
  wealth[i] <- wealth[i-1] + profit[i]
  return[i] <- (wealth[i] / wealth[i-1]) -1  
}
ret.bb2<-reclass(return,price)

Return.cumulative(ret.bb2,geometric = TRUE)
Return.annualized(ret.bb2, geometric = TRUE)
SemiSD(ret.bb2)

##### Charting #####
chartSeries(MSFT,
            subset="2021-11::2022-04-09",
            theme=chartTheme('white'))
addBBands(n=20,sd=2,maType="SMA")
addTA(signal,type='S',col='red')


############## Combining results #################
ret_all <- cbind(ret.bb1,ret.bb2)
charts.PerformanceSummary(ret_all,
                          legend.loc = "topleft")


