require("quantmod"); require("urca"); require("stats"); require("jsonlite"); require("httr"); require("digest")
require("tidyverse"); require("PerformanceAnalytics"); require("PortfolioAnalytics"); require("ROI.plugin.quadprog")
require("ROI.plugin.glkp")

api_Key <- "7E5tWdigUAw0DnwB5kR1bxgZGkmJmKPEKCrghsZUFAUIPyY7YgPGiD2NMMijTSAK"
api_Secret <- "WP8NoDH2CVfas1Fxop2XmtQQsVcS63FpY1XXTAwqbpTwm5gU2twveSYXMjX6jBAg"
api_Url <- "https://api.binance.us"
path <- "/api/v3/klines"


timestamp <- round(as.numeric(Sys.time())*1000)
timeQueryString <- paste("timestamp=", timestamp, sep="")
# symbol <- "symbol=ADAUSDT"
# interval <- "interval=5m"
# orderbklmt <- "&limit=5"



coins <- c("ADAUSDT", "BTCUSDT", "ETHUSDT", "LTCUSDT")

## CALCULATES RATE OF CHANGE FOR CRYPTO IN COINS ##
coin_data <- lapply(
  coins,
  function(c) {
    scoop <- paste(api_Url, path, sep = "")
    params <- paste0(
      "?symbol=",
      c,
      "&interval=1d",
      "&limit=1000",
      sep = ""
    )    
    url <- paste0(scoop, params, sep = "")
    request <- GET(url = url, add_headers("X-MBX-APIKEY" = api_Key))
    response <- content(request, as = "text", encoding = "UTF-8")
    data <- as.data.frame(fromJSON(response), stringsAsFactors = F)
    
    ## For some reason this line doesn't work ##
    # data <- as.data.frame(fromJSON(content(GET(url=url, add_headers("X-MBX-APIKEY" = api_Key))), as = "text"), stringsAsFactors = F)
    
    names(data)<- c("date","open", "high", "low", "close", "volume", "close_time", "quote_asset_volume", 
                    "number_of_trades", "taker_buy_base_asset_volume", "taker_buy_asset_volume", "ignore")
    
    data <- data %>%
      mutate(
        date = as.Date(as.POSIXct(as.numeric(date) / 1000, origin = "1970-01-01", tz = "UTC")),
        close = as.numeric(close),
      )
    
    data <- data %>% 
      arrange(date) %>%
      mutate(
        pnl = replace_na(ROC(close, type = "discrete"),0)
      ) %>%
      select(date, pnl)
    
    names(data) <- c("date", c)
    data
  }
)

# Check Check #
# str(coin_data)

## REFORMAT TO data.frame of returns ##

coindf <- coin_data %>% 
  reduce(inner_join, by = "date")

row.names(coindf) <- coindf$date

coindf <- coindf %>% 
  select(-date)



chart.CumReturns(coindf, legend.loc = "topleft")

## Cumulation of all returns in one column ##
portRet <- Return.portfolio(coindf)
colSums(is.na(portRet))

## SPY BENCHMARK ##
benchmarkPrices<- getSymbols.yahoo('^GSPC', from = '2019-09-25', periodicity = 'daily', auto.assign = FALSE)[,4]
benchmarkRets <- na.omit(ROC(benchmarkPrices))
colSums(is.na(benchmarkRets))


# CAPM.beta(portRet, benchmarkRets, .035/252)

CAPM.jensenAlpha(portRet, benchmarkRets, .035/252)
SharpeRatio(portRet, .035/252)


table.AnnualizedReturns(portRet)
table.CalendarReturns(portRet)

portf <- portfolio.spec(colnames(coindf))

portf <- add.constraint(portf, type="weight_sum", min_sum=0.99, max_sum = 1.01)
portf <- add.constraint(portf, type="transaction_cost", ptc = 0.001)
portf <- add.constraint(portf, type="box", min=.10, max = .40)
portf <- add.objective(portf, type="return", name="mean")
portf <- add.objective(portf, type="risk", name="StdDev", target=0.005)
 
# optPort <- optimize.portfolio(portRet, portf, optimize_method = 'ROI')

rp <- random_portfolios(portf, 1000, "sample")

opt_rebal <- optimize.portfolio.rebalancing(coindf, portf,
                                            optimize_method = "random",
                                            search_size = 100,
                                            rp=rp,
                                            rebalance_on = "weeks",
                                            training_period = 1,
                                            rolling_window = 1)
