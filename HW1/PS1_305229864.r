require(foreign)
require(data.table)
require(ggplot2)
require(lubridate)
require(moments)

rm(list=ls())
# Download data and set as data.table
crsp_monthly <- as.data.table(read.csv("crsp_monthly.csv"))

######################## PS1 Qn 1 ################################################ 
# Input: crsp_monthly
# Output: Monthly_CRSP_Stocks
PS1_Q1 <- function(crsp_monthly){
  # convert date to date datatype
  crsp_monthly[, date:= ymd(date)]
  
  # set keys, sort by date
  setkey(crsp_monthly, date)
  
  # set the year and month as integer
  crsp_monthly[,Year:= year(date)]
  crsp_monthly[,Month:= month(date)]
  
  # Filter by sharecode 10 and 11  and Filter by EXCHCD 1 2 3
  crsp_monthly <- crsp_monthly[SHRCD %in% c(10,11)]
  crsp_monthly <- crsp_monthly[EXCHCD %in% c(1,2,3)]
  
  # Filter out missing ret and dlret data
  crsp_monthly <- crsp_monthly[!RET %in% c(-99,-88,-77,-66,-55,-44)]
  crsp_monthly <- crsp_monthly[!DLRET %in% c(-99,-88,-77,-66,-55,-44)]
  
  # convert the ret and delisting return into numeric data type for easy calculation
  crsp_monthly[, RET := as.numeric(as.character(RET))]
  crsp_monthly[, DLRET := as.numeric(as.character(DLRET))]
  crsp_monthly[, PRC := as.numeric(as.character(PRC))]
  crsp_monthly[, SHROUT := as.numeric(as.character(SHROUT))]
  
  # calculates the cum-Dividend returns
  crsp_monthly[, `:=`(cumDivRet, ifelse(is.na(DLRET),RET,ifelse(is.na(RET), DLRET, (1+DLRET)*(1+RET)-1 )))]
  
  # calculate the marketcap
  crsp_monthly[, `:=`(ME, abs(PRC) * abs(SHROUT)*1000)]
  
  #lag the market cap of each firm
  crsp_monthly[, mktCapLagged := shift(ME), by=c("PERMNO")]
  
  # remove NAs
  crsp_monthly <- crsp_monthly[PRC != "NA"]
  crsp_monthly <- crsp_monthly[RET != "NA"]
  crsp_monthly <- crsp_monthly[cumDivRet != "NA"]
  crsp_monthly <- crsp_monthly[mktCapLagged != "NA"]

  # value weight portfolio, this might be wrong, let's see
  valueweight <- crsp_monthly[,list(Stock_Vw_Ret = weighted.mean(cumDivRet, mktCapLagged, na.rm = TRUE)),
                                    by=list(Year, Month)]
  
  # equal weight portfolio
  equalweight <- crsp_monthly[,list(Stock_Ew_Ret = mean(cumDivRet)),by=list(Year, Month)]
  
  # Total market value of the previous month
  mktcap <- crsp_monthly[,list(Stock_lag_MV = sum(mktCapLagged)/1000000), by=list(Year, Month)]
  crspMktEachPeriod <- cbind(valueweight, equalweight[,3],mktcap[,3])
  
  # remove the first date column
  return(crspMktEachPeriod)
}

PS1_ans <- PS1_Q1(crsp_monthly)
Monthly_CRSP_Stocks <- PS1_ans
Monthly_CRSP_Stocks
write.table(Monthly_CRSP_Stocks, file = "Monthly_CRSP_Stocks.csv", row.names=FALSE, sep=",")
################################################################################################ 


######################## PS1 Qn 2 ################################################
rm(list=ls())
# load data from FF website
FF_mkt <- as.data.table(read.csv("F-F_Research_Data_Factors.CSV"))

# load data from Qn 1 output
Monthly_CRSP_Stocks <- as.data.table(read.csv("Monthly_CRSP_Stocks.csv"))

PS1_Q2 <- function(FF_mkt, Monthly_CRSP_Stocks){
  # set column names to preferred column names
  colnames(FF_mkt) <- c("date", "Mkt_RF","SMB","HML","RF")
  
  # subtset Monthly_CRSP_Stocks to contain only data from 1926 July
  Monthly_CRSP_Stocks <- Monthly_CRSP_Stocks[7:dim(Monthly_CRSP_Stocks)[1],]
  
  # set the Risk free rate to decimal numbers
  FF_mkt[, RF := RF  /100]
  FF_mkt[, Mkt_RF := Mkt_RF/100]
  
  # put risk free rate in Monthly_CRSP_Stocks
  Monthly_CRSP_Stocks <- cbind(Monthly_CRSP_Stocks, RFAunnual=FF_mkt$RF)
  
  # calculate the estimated return
  Monthly_CRSP_Stocks[, EstimatedExMktRet := Stock_Vw_Ret - RFAunnual]
  
  # put estimated return in Monthly_CRSP_Stocks
  Monthly_CRSP_Stocks <- cbind(Monthly_CRSP_Stocks,ActualExMktRet=FF_mkt$Mkt_RF)
  
  # get Estimated and actual 
  ret <- Monthly_CRSP_Stocks[,.(EstimatedExMktRet,ActualExMktRet)] 
  
  AnnualizedMean <- c(mean(ret$EstimatedExMktRet) * 12 , mean(ret$ActualExMktRet) * 12)
  AnnualizedSD <- c(sd(ret$EstimatedExMktRet) * sqrt(12) , sd(ret$ActualExMktRet) * sqrt(12))
  AnnualizeSR <- c(AnnualizedMean[1]/AnnualizedSD[1], AnnualizedMean[2]/AnnualizedSD[2])
  Skewness <- c(skewness(ret$EstimatedExMktRet),skewness(ret$ActualExMktRet))
  ExcessKurtosis <-  c(kurtosis(ret$EstimatedExMktRet),kurtosis(ret$ActualExMktRet))-3
  # find the annualized return, annualized volatility, sharpe ratio, skewness and excess kurtosis
  summary <- data.table( Stats= c("Annualized Mean", "Annualized SD", "Sharpe Ratio","Skewness","Excess Kurtosis"),
      EstimatedExMktRet = c(AnnualizedMean[1], AnnualizedSD[1], AnnualizeSR[1],Skewness[1],ExcessKurtosis[1]),
                        ActualExMktRet = c(AnnualizedMean[2], AnnualizedSD[2], AnnualizeSR[2],Skewness[2],ExcessKurtosis[2]))
  
  return(summary)
}

PS1_Q2_ans <- PS1_Q2(FF_mkt, Monthly_CRSP_Stocks)
PS1_Q2_ans

################################################################################################ 


######################## PS1 Qn 3 ################################################
rm(list=ls())
# load data from FF website
FF_mkt <- as.data.table(read.csv("F-F_Research_Data_Factors.CSV"))

# load data from Qn 1 output
Monthly_CRSP_Stocks <- as.data.table(read.csv("Monthly_CRSP_Stocks.csv"))

PS1_Q3 <- function(FF_mkt,Monthly_CRSP_Stocks){
  colnames(FF_mkt) <- c("date", "Mkt_RF","SMB","HML","RF")
  
  # subtset Monthly_CRSP_Stocks to contain only data from 1926 July
  Monthly_CRSP_Stocks <- Monthly_CRSP_Stocks[7:dim(Monthly_CRSP_Stocks)[1],]
  
  # set the Risk free rate to decimal numbers
  FF_mkt[, RF := RF  /100]
  FF_mkt[, Mkt_RF := Mkt_RF/100]
  
  # put risk free rate in Monthly_CRSP_Stocks
  Monthly_CRSP_Stocks <- cbind(Monthly_CRSP_Stocks, RFAunnual=FF_mkt$RF)
  
  # calculate the estimated return
  Monthly_CRSP_Stocks[, EstimatedExMktRet := Stock_Vw_Ret - RFAunnual]
  
  # put estimated return in Monthly_CRSP_Stocks
  Monthly_CRSP_Stocks <- cbind(Monthly_CRSP_Stocks,ActualExMktRet=FF_mkt$Mkt_RF)
  
  # get Estimated and actual 
  ret <- Monthly_CRSP_Stocks[,.(EstimatedExMktRet,ActualExMktRet)] 
  
  summary <- c(cor(ret$EstimatedExMktRet,ret$ActualExMktRet), max(abs(ret$EstimatedExMktRet - ret$ActualExMktRet)))
  names(summary) <- c("Correlation","Max difference")
  return(summary)
}

PS1_Q3_ans <- PS1_Q3(FF_mkt,Monthly_CRSP_Stocks)
PS1_Q3_ans

################################################################################################ 

