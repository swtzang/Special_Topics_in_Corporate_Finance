# https://fderyckel.github.io/financialanalysiswithR/02-getting_data.html
library(tidyverse)
library(lubridate)        #to deal with dates
library(rvest)            # to webscrap financial data 

library(gridExtra)        # to stack the charts
library(scales)           # again to deal with dates but on the x-axis
library(bdscale)          # to remove weekends and holidays
library(reshape2)


# setting up the path within the project for easier portability of code
thePath <- "/home/swtzang/git/special_topics_corporate_finance"

# we make it a function that take as input a vector of tickers
tickers <- c("GDX", "MAG")
i=1
get_eod_data <- function(ticker_vector){
  tickers <- ticker_vector
  for (i in 1:length(tickers)){
    print(tickers[i])
    data = quantmod::getSymbols(Symbols = tickers[i],
                                src = "av",
                                api.key = "U9M001MPWDVDNQHH", 
                                output.size = "full", 
                                adjusted = TRUE, 
                                auto.assign = FALSE,
                                index.class="POSIXct")
    colnames(data) = c("Open", "High", "Low", "Close", "Volume", "Adjusted")
    zoo::write.zoo(data, paste0(thePath, "/stockdata_av/", tickers[i], ".csv"), 
                   sep = ",", row.names = FALSE)
    # I am adding this because, we are limited to 5 download per minutes with the free key.
    Sys.sleep(14)
  }}

## Just get data using this function from now on ;-)
get_eod_data(c("GDX", "MAG"))
#
tickers <- c("XOM", "SLB", "CANE", "NIB", "GDXJ")
#tickers <- c("SLB", "CANE", "NIB", "GDXJ")
get_eod_data(tickers)

# compute returns
df <- list()
for (i in tickers){
df[[i]] <- read_csv(paste0(thePath, "/stockdata_av/", i, ".csv")) %>% 
           select(Index, Adjusted) %>% rename(!!i := Adjusted)
}
# merge all stock prices
df_all <- df %>% map_df(gather, key=key, value=value, -Index) %>% 
          spread(key, value)
#
na.omit(df_all)


x <- tibble(i = c("a","b","c"), j = 1:3)
y <- tibble(i = c("b","c","d"), k = 4:6)
z <- tibble(i = c("c","d","a"), l = 7:9)

list(x, y, z) %>% 
  map_df(gather, key=key, value=value, -i) %>% 
  spread(key, value)

