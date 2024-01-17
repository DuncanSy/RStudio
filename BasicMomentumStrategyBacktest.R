require("PerformanceAnalytics") ; require("quantmod") ; require("pbapply"); require("data.table")

e <- new.env()
tickers <- c('JDST','JNUG', 'SPY')
getSymbols(tickers, from = "2010-12-07", env = e)

# Take just the adjusted close of all tickers
PRC <- do.call(merge, eapply(e,Ad))

# 
if(last(index(PRC)) != Sys.Date())
{
  last <- pblapply(as.list(gsub(".Adjusted","", names(PRC))), getQuote)
  PRC <- rbind(PRC,xts(coredata(t(rbindlist(last)$Last)),order.by = Sys.Date()))
}

NOM <- colnames(PRC) <- gsub(".Adjusted","", names(PRC))
MOMO60 <- round(ROC(PRC, n = 60, type="discrete"),4)

MOMO60 <- MOMO60["20030331::"]
PRC <- PRC["20030331::"]

indx <- seq(as.Date("2003-03-31"), length.out = 300, by = '4 weeks')


#SELECT <- MOMO60[paste(indx)];dim(SELECT)

indx2 <- ifelse((indx %in% index(SELECT) == FALSE), paste(indx+1), paste(indx))


SELECT <- MOMO60[paste(indx2)];dim(SELECT)

PRC2 <- PRC[paste(indx2)];dim(SELECT)

ASSETS4 <- combn(NOM,4)


MOMO = function(x)
{
  y <- ASSETS4[,x]
  s <- SELECT[,y]
  SEQ <- as.numeric(apply(s, 1, which.max))
  PRC2 <- round(PRC2[,y],2)
  RETS <- CalculateReturns(PRC2,"discrete")
  ALL <- do.call(merge,lapply(as.list(1:ncol(RETS)), function(x){
    Lag(reclass(ifelse(SEQ == x, 1, 0), match.to = s)*RETS[,x])
  }))
  
  colnames(ALL) <- names(PRC2)
  ALL[is.na(ALL)] <- 0
  
  EQT <- reclass(rowSums(ALL), match.to = ALL); EQT[is.na(EQT)] <- 0
  colnames(EQT) <- paste(names(PRC2), collapse = "-")
}

STRAT <- pblapply(as.list(1:ncol(ASSETS4)), function(x) MOMO(x))


