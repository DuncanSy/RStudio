require("quantmod"); require("urca")

setwd("C:\\Users\\....")

retrievePrice =  function(filename)
{
  initial <- as.xts(read.zoo(file = filename, header = TRUE, format = "%Y-%m-%d", sep=','))
  vprc <- initial[,5:6]
  return(vprc)
}
 
testCoint = function(coint1, coint2, date = "2016-10-03::" ){
  cointPrc1 <- coint1[,1]  
  cointPrc2 <- coint2[,1]
  a = cointPrc1[date]
  b = cointPrc2[date]
  comb = cbind(a,b)
  m1 = ar(comb)
  if((m1$order)>1){
    cointAB = ca.jo(comb, K=m1$order)
    results = summary(cointAB)
  }
  else{
    cointAB = ca.jo(comb, K = 2)
    results = summary(cointAB)
  }
  return(results)
}

jnugRets <- retrievePrice('JNUG.csv')
jdstRets <- retrievePrice('JDST.csv')
nugtRets <- retrievePrice('NUGT.csv')

test <- testCoint(jnugRets, jdstRets)

etfs <- cbind(jnugRets[,1], jdstRets[,1], nugtRets[,1])
colnames(etfs) <- c("JNUG", "JDST", "NUGT")
tsRainbow <- rainbow(ncol(etfs))
plot(etfs, ylab = "Price", main = "Daily Prices of Gold ETFs", col = tsRainbow)
legend(x = "top", lty = 1, legend = colnames(etfs), col = tsRainbow, cex = 8, ncol = 4)
