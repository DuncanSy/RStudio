require("arrow");require("quantmod"); require("urca"); require("stats"); require("jsonlite"); require("httr"); require("digest")

setwd("~/Coding")

btcusdt1 <- read_parquet("BTC-USDT.parquet", as_data_frame = TRUE)
btcusdt <- subset(btcusdt1, select = colnames(btcusdt1[,1:4]), as.numeric(as.POSIXct(open_time)) >= as.numeric(as.POSIXct("2017-08-17 00:00:00")))
adausdt1 <- read_parquet("ADA-USDT.parquet", as_data_frame = TRUE)
adausdt <- subset(adausdt1, select = colnames(adausdt1[,1:4]))

btcOHLC <- try.xts(btcusdt)
adaOHLC <- data.frame(adausdt)

btcClose <- data.frame(btcusdt[,c(2,1)])
adaClose <- data.frame(adausdt[,c(2,1)])


testCoint = function(coint1, coint2){
  cointPrc1 <- coint1[,1]  
  cointPrc2 <- coint2[,1]
  a = cointPrc1
  b = cointPrc2
  comb = cbind(a,b)
  m1 = ar(comb)
  cointAB = ca.jo(comb, K = 2)
  results = summary(cointAB)
  return(results)
  # if((m1$order)>1){
  #   cointAB = ca.jo(comb, K=m1$order)
  #   results = summary(cointAB)
  # }
  # else{
  #   cointAB = ca.jo(comb, K = 2)
  #   results = summary(cointAB)
  # }
  # return(results)
}

crypto <- cbind(btcClose, adaClose[,2])
colnames(crypto) <- c('Date','BTC', 'ADA')
tsRainbow = rainbow(ncol(crypto))
plot(crypto[,2:3], type = 'l', ylab = "Price", main = "1m Prices of BTC and ADA", col=tsRainbow)
legend(x='top', lty = 1, legend = colnames(crypto), col = tsRainbow, cex=.8, ncol = 2)

btcadacoint <- testCoint(btcClose,adaClose)
