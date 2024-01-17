require("jsonlite"); require("data.table"); require("rvest")

## TDAmeritrade API (figure out how to house globally) ##

apiKey <- "ATM2T4A9VMUBWRHZYETGWKB2OSAPWA0B"

## Base URLs ##
baseURL <- "https://api.tdameritrade.com"
ticker <-"AMD"

## Yahoo screeners ##

dgain <- "https://finance.yahoo.com/screener/predefined/day_gainers?offset=0&count=100"
active <- "https://finance.yahoo.com/screener/predefined/most_actives?offset=0&count=100"

html_table_fix <- function(x, header = NA, trim = TRUE,
                           fill = FALSE, dec = ".") {
  
  stopifnot(html_name(x) == "table")
  
  # Throw error if any rowspan/colspan present
  rows <- html_nodes(x, "tr")
  n <- length(rows)
  cells <- lapply(rows, "html_nodes", xpath = ".//td|.//th")
  
  ncols <- lapply(cells, html_attr, "colspan", default = "1")
  # Replace empty values of colspan with "1"
  ncols <- lapply(ncols, function(x) {x[x==""] <- "1"; x})
  ncols <- lapply(ncols, as.integer)
  nrows <- lapply(cells, html_attr, "rowspan", default = "1")
  nrows <- lapply(nrows, as.integer)
  
  p <- unique(vapply(ncols, sum, integer(1)))
  maxp <- max(p)
  
  if (length(p) > 1 & maxp * n != sum(unlist(nrows)) &
      maxp * n != sum(unlist(ncols))) {
    # then malformed table is not parsable by smart filling solution
    if (!fill) { # fill must then be specified to allow filling with NAs
      stop("Table has inconsistent number of columns. ",
           "Do you want fill = TRUE?", call. = FALSE)
    }
  }
  
  values <- lapply(cells, html_text, trim = trim)
  out <- matrix(NA_character_, nrow = n, ncol = maxp)
  
  # fill colspans right with repetition
  for (i in seq_len(n)) {
    row <- values[[i]]
    ncol <- ncols[[i]]
    col <- 1
    for (j in seq_len(length(ncol))) {
      out[i, col:(col+ncol[j]-1)] <- row[[j]]
      col <- col + ncol[j]
    }
  }
  
  # fill rowspans down with repetition
  for (i in seq_len(maxp)) {
    for (j in seq_len(n)) {
      rowspan <- nrows[[j]][i]; colspan <- ncols[[j]][i]
      if (!is.na(rowspan) & (rowspan > 1)) {
        if (!is.na(colspan) & (colspan > 1)) {
          # special case of colspan and rowspan in same cell
          nrows[[j]] <- c(utils::head(nrows[[j]], i),
                          rep(rowspan, colspan-1),
                          utils::tail(nrows[[j]], length(rowspan)-(i+1)))
          rowspan <- nrows[[j]][i]
        }
        for (k in seq_len(rowspan - 1)) {
          l <- utils::head(out[j+k, ], i-1)
          r <- utils::tail(out[j+k, ], maxp-i+1)
          out[j + k, ] <- utils::head(c(l, out[j, i], r), maxp)
        }
      }
    }
  }
  
  if (is.na(header)) {
    header <- all(html_name(cells[[1]]) == "th")
  }
  if (header) {
    col_names <- out[1, , drop = FALSE]
    out <- out[-1, , drop = FALSE]
  } else {
    col_names <- paste0("X", seq_len(ncol(out)))
  }
  
  # Convert matrix to list to data frame
  df <- lapply(seq_len(maxp), function(i) {
    utils::type.convert(out[, i], as.is = TRUE, dec = dec)
  })
  names(df) <- col_names
  class(df) <- "data.frame"
  attr(df, "row.names") <- .set_row_names(length(df[[1]]))
  
  if (length(unique(col_names)) < length(col_names)) {
    warning('At least two columns have the same name')
  }
  
  df
}

s1 <- dgain %>% read_html() %>% html_nodes("table") %>% html_table_fix()
s2 <- active %>% read_html() %>% html_nodes("table") %>% html_table_fix()

try <-

## Historical Stock Data (1 ticker per request) ##
AHdata <- "true"
freq <- "1"
fType <- "minute"
pType <- "day"
hPriceParams <- paste0("&periodType=", pType, "&frequencyType=", fType, "&frequency=", freq, "&needExtendedHoursData=", AHdata)
getPriceHistTD <- function(ticker)
{
  url <- paste0(baseURL, "/v1/marketdata/", ticker, "/pricehistory?apikey=",apiKey, hPriceParams)
  tmp <- read_json(url, simplifyVector=TRUE)
  tmp <- tmp$candles
  tmp <- xts(tmp[,c("open", "high", "low", "close", "volume")], order.by = as.POSIXct(tmp$datetime/1000, origin = "1970-01-01"))
  colnames(tmp) <- paste0(ticker, ".", c("Open", "High", "Low", "Close", "Volume"))
  tmp
}

hist <- getPriceHistTD(ticker = ticker)
test <- subset(hist, format(index(hist), '%H') %in% c('09'))
n <- length(test[,4])
tprice <- test[,4]
tvol <- test[,5]
cl <- diff(log(tprice), lag=1)


# jdstHist <- getPriceHistTD("JDST")
# jnugHist <- getPriceHistTD("JNUG")

## Real Time Quotes ##



## OPTIONS CHAIN DATA ##

# optionPath <- "/v1/marketdata/chains?apikey="
# optionParam <- "&contractType=ALL&includeQuotes=TRUE&strategy=SINGLE&range=ALL&optionType=ALL"
# 
# getOptionChainTD <- function(ticker)
# {
#   url <- paste0(baseURL, optionPath, apiKey,"&symbol=",ticker, optionParam)
#   tmp <- read_json(url, simplifyVector=TRUE)
# }
# 
# OC <- getOptionChainTD(ticker = ticker)
# 
# oc2df <- function(OC)
# {
#   c.exp <- names(OC$callExpDateMap)
#   p.exp <- names(OC$putExpDateMap)
#   x <- c.exp[1]
#   
#   # CALLS #
#   calls <- lapply(as.list(c.exp), function(x){
#     oc <- OC$callExpDateMap[paste(x)]
#     strks <- names(oc[[1]])
#     
#     dat <- lapply(as.list(strks), function(y) oc[[x]][[y]])
#     rbindlist(dat, use.names = TRUE, fill = TRUE)
#   })
#   # PUTS #
#   puts <- lapply(as.list(p.exp), function(x){
#     oc <- OC$putExpDateMap[paste(x)]
#     strks <- names(oc[[1]])
#     
#     dat <- lapply(as.list(strks), function(y) oc[[x]][[y]])
#     rbindlist(dat, use.names = TRUE, fill = TRUE)
#     
#   })
#   puts <- rbindlist(puts, use.names = TRUE, fill = TRUE)
#   calls <- rbindlist(calls, use.names = TRUE, fill = TRUE)
#   
#   all <- rbind(calls, puts)
#   
#   # Convert Time Format *** FIX *** #
#   
#   all$tradeTimeInLong <- as.POSIXct(all$tradeTimeInLong/1000, origin="1970-01-01")
#   all$quoteTimeInLong <- as.POSIXct(all$quoteTimeInLong/1000, origin="1970-01-01")
#   all$expirationDate <- as.POSIXct(all$expirationDate/1000, origin="1970-01-01")
#   all$lastTradingDay <- as.POSIXct(all$lastTradingDay/1000, origin="1970-01-01")
#   all
# }
# 
# results <- oc2df(OC)


