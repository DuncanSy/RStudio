require("arrow");require("quantmod"); require("urca"); require("stats"); require("jsonlite"); require("httr"); require("digest");
require("forecast"); require("tseries");require("DEoptim"); require("PerformanceAnalytics")
setwd("~/Coding")

adausdt1 <- read_parquet("ADA-USDT.parquet", as_data_frame = TRUE)
adausdt <- subset(adausdt1, select = colnames(adausdt1[,c(4, 10)]))

bm <- getSymbols("^GSPC",from = "2010-01-01", auto.assign = FALSE )

adaCl<- data.frame(adausdt[,c(2,1)])
# adjusted <- time_ceiling(adaCl[,1], "minute")
coin <- xts(adaCl[,2], order.by = adaCl[,1], tzone = "UTC")
colnames(coin) <- "ADA.Close"
#as.POSIXct(adaCl$open_time, format = "%Y-%m-%d %H:%M:%S"))

toBT <- function(coin, momo1, momo2, nFast,nSlow,nSig, nRSI, rsiL, clNdays)
{
  #ROC
  coinRets <- ROC(coin$ADA.Close, type = "discrete")
  # bmRets <- ROC()
  
  #Momentum
  coinMOMO1 <- momentum(coinRets, n = momo1); colnames(coinMOMO1) <- "coinMOMO1"
  coinMOMO2 <- momentum(coinRets, n = momo2); colnames(coinMOMO2) <- "coinMOMO2"
  
  macd <- MACD(x = na.locf(coin$ADA.Close), nFast = nFast, nSlow = nSlow, nSig = nSig)
  
  rsi <- RSI(price = na.locf(coin$ADA.Close), n = nRSI)
  rsiLag <- Lag(rsi,rsiL); colnames(rsiLag) <- "rsiLag"
  
  #signal when conditions met
  toSig <- ifelse(coinMOMO1>coinMOMO2 &
                    macd$macd > macd$signal &
                    rsi > rsiLag
                  ,1, NA)
  #Row for signal
  sigLoc <- which(toSig==1)
  #Close after N days
  posCl = sigLoc + clNdays
  posCl[posCl>nrow(coin)] <- nrow(coin)
  #insert 0s to close position
  toSig[posCl,] <- 0
  toSig <- na.locf(toSig)
  #lagsignal to buy next close and hold
  toSig <- Lag(toSig); colnames(toSig) <- "sig"
  #check signals
  if (nrow(toSig[toSig==1]) !=0)
  {
    #combine 
    ALL <- na.omit(merge(coin, toSig))
    #ROC of coin
    ALL$buyNhold <- ROC(ALL[,1], type = 'discrete')
    # ROC of strat
    ALL$stratRets <- ALL$buyNhold * ALL$sig
    #na.omit
    ALL <- na.omit(ALL)
  }
  else {
    ALL<- coin
    ALL$sig <- 0
    ALL$buyNhold <- 0
    ALL$stratRets <- 0
    ALL <- na.omit(ALL)
  }
  ALL
}

toOptim <- function(n)
{
  RES <- try(toBT(coin = coin[5001:10000,], momo1 = n[1], momo2 = n[2], nFast = n[3], nSlow = n[4], 
                  nSig = n[5], nRSI = n[6], rsiL = n[7], clNdays = n[8]))
  if(!inherits(RES, 'try-error'))
  {
    RES <- colSums(RES[,c("buyNhold", "stratRets")])
    RES <- (RES[2]-RES[1])+(RES[2]-RES[3])
  } else {
    RES = -1000
  }
  return(-RES)
}

fnmap_f <- function(x) {c(round(x,0))}



# adaRets <- data.frame(log(adaCl[1:1000000, 2]+1/adaCl[1:1000000, 2]))

# LOWER = c(1,1,1,1,1,1,1,1)
# UPPER = c(50,50,50,50,20,100,50,100)

LOWER = rep(1,8)
UPPER = rep(50,8)

r<- DEoptim(toOptim, lower = LOWER, upper = UPPER, control = list(itermax = 1000))

op <- r$optim$bestmem

best <- toBT(coin = coin, momo1 = yuh[1], momo2 = yuh[2], nFast = yuh[3],nSlow = yuh[4],nSig = yuh[5], nRSI = yuh[6], rsiL = yuh[7], clNdays = yuh[8])

charts.PerformanceSummary(merge(best$stratRets, best$buyNhold), geometric = FALSE)

## ACF, PACF, Dickey-Fuller Test ##

acf(adaRets, lag.max = 50)
pacf(adaRets, lag.max = 50)

diffAda <- diff(adaRets[,1], 1)

adf.test(adaRets[,1])
adf.test(diffAda)


# Time Series and auto.arima #

priceArima <- ts(adaRets, start = 1, frequency = 12)
# fitAdaRets <- auto.arima(priceArima, trace = TRUE)


m1 <- lm()