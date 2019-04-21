suppressMessages(require(foreign))
suppressMessages(require(data.table))
suppressMessages(require(ggplot2))
suppressMessages(require(lubridate))
suppressMessages(require(zoo))
suppressMessages(require(moments))

rm(list=ls())
######################## PS1 Qn 1 ################################################
# Import Bond data and set as data.table
bonddata <- as.data.table(read.csv("bonddata.csv"))

# change MCALDT to date type 
bonddata[, MCALDT := mdy(MCALDT) ]

PS2_Q1 <- function(bonddata){
  # sort data by date
  setkey(bonddata, MCALDT)
  
  # Filter out missing Monthly Unadjusted Return
  bonddata[,`:=`(TMRETNUA, ifelse(TMRETNUA == -99.0, NA, TMRETNUA))]
  
  # set the year and month as integer
  bonddata[,Year:= year(MCALDT)]
  bonddata[,Month:= month(MCALDT)]

  # lag the market by KYCRSPID
  bonddata[,mktCapLagged := shift(TMTOTOUT), by=KYCRSPID]
  
  valueweight <- bonddata[, list(Bond_Vw_Ret = sum(TMRETNUA*mktCapLagged, na.rm = TRUE)/sum(mktCapLagged,na.rm = TRUE)), 
                          by=list(Year, Month)]
  
  equalweight <- bonddata[, list(Bond_Ew_Ret = sum(TMRETNUA,na.rm = TRUE)/length(!is.na(TMRETNUA))), 
                          by=list(Year, Month)]
  
  mktcap <- bonddata[,list(Bond_lag_MV = sum(mktCapLagged, na.rm = TRUE)), by=list(Year, Month)]
  
  ans <- merge(merge(valueweight, equalweight, by=c("Year", "Month")), mktcap,  by=c("Year", "Month"))
  return(ans)
}

Monthly_CRSP_Bonds <- PS2_Q1(bonddata)
write.table(Monthly_CRSP_Bonds, file = "Monthly_CRSP_Bonds.csv", row.names=FALSE, sep=",")
################################################################################################ 

rm(list=ls())
######################## PS1 Qn 2 ################################################
# load data Monthly CRSP Stocks from Qn 1 output
Monthly_CRSP_Stocks <- as.data.table(read.csv("Monthly_CRSP_Stocks.csv"))
Monthly_CRSP_Riskless  <- as.data.table(read.csv("Monthly_CRSP_Riskless.csv"))
Monthly_CRSP_Bonds <- as.data.table(read.csv("Monthly_CRSP_Bonds.csv"))

# change MCALDT to date type 
Monthly_CRSP_Riskless[, caldt := ymd(caldt) ]

# sort data by date
setkey(Monthly_CRSP_Riskless, caldt)

# start of the function
PS2_Q2 <- function(Monthly_CRSP_Stocks, Monthly_CRSP_Riskless, Monthly_CRSP_Bonds){
  # set the year and month as integer
  Monthly_CRSP_Riskless[,Year:= year(caldt)]
  Monthly_CRSP_Riskless[,Month:= month(caldt)]
  Monthly_CRSP_Riskless[,caldt:=NULL]
  
  
  stocks <- Monthly_CRSP_Stocks[, .(Year, Month, Stock_Vw_Ret, Stock_lag_MV)]
  bonds <- Monthly_CRSP_Bonds[,.(Year,Month, Bond_Vw_Ret, Bond_lag_MV)]
  
  # merge stock and bonds data
  ans <- merge(stocks, bonds,  by=c("Year", "Month"))
  ans <- merge(ans, Monthly_CRSP_Riskless,  by=c("Year", "Month"))
  ans[,`:=`(Stock_Excess_Vw_Ret = (Stock_Vw_Ret - t30ret), Bond_Excess_Vw_Ret = (Bond_Vw_Ret - t30ret))]
  
  # get the needed column out 
  summary <- ans[,.(Year, Month, Stock_lag_MV, Stock_Excess_Vw_Ret, Bond_lag_MV,Bond_Excess_Vw_Ret)]
  
  return(summary)
}

summary <- PS2_Q2(Monthly_CRSP_Stocks, Monthly_CRSP_Riskless, Monthly_CRSP_Bonds)
write.table(summary, file = "Monthly_CRSP_Universe.csv", row.names=FALSE, sep=",")
################################################################################################ 


rm(list=ls())
######################## PS1 Qn 3 ################################################
# load data Monthly CRSP Stocks from Qn 2 output
Monthly_CRSP_Universe<- as.data.table(read.csv("Monthly_CRSP_Universe.csv"))

# start of the function 
setorder(Monthly_CRSP_Universe,Year,Month)

PS2_Q3 <- function(Monthly_CRSP_Universe){
  # value weighted portfolio return
  Monthly_CRSP_Universe[,stock_Vw_Weight := Stock_lag_MV/(Stock_lag_MV + Bond_lag_MV)]
  Monthly_CRSP_Universe[,Excess_Vw_Ret := stock_Vw_Weight * Stock_Excess_Vw_Ret + (1 - stock_Vw_Weight) * Bond_Excess_Vw_Ret]
  
  # 60-40 portfolio return above riskless rate
  Monthly_CRSP_Universe[,Excess_60_40_Ret := 0.6 * Stock_Excess_Vw_Ret + 0.4 * Bond_Excess_Vw_Ret]
  
  # find the inverse sigma hat of the stocks
  # at the end of each calendar month,we estimated volatilities of all the available asset
  # classes (using data up to month t – 1), hence we need to shift the data down by 1 row 
  Monthly_CRSP_Universe[,Stock_inverse_sigma_hat := shift(1/rollapply(Stock_Excess_Vw_Ret,36, sd, fill=NA, align="right"))]
  
  # similar approach to get Bond_inverse_sigma_hat
  Monthly_CRSP_Universe[,Bond_inverse_sigma_hat := shift(1/rollapply(Bond_Excess_Vw_Ret,36, sd, fill=NA, align="right"))]
  
  # unlevered position, k = 1 / (sum of inverse sigma hat of the both assets)
  Monthly_CRSP_Universe[,Unlevered_k := 1/(Stock_inverse_sigma_hat + Bond_inverse_sigma_hat)]
  # find unlevered risk parity portfolio return 
  Monthly_CRSP_Universe[,Excess_Unlevered_RP_Ret := shift(Unlevered_k * Stock_inverse_sigma_hat) * Stock_Excess_Vw_Ret 
                                                     + shift(Unlevered_k * Bond_inverse_sigma_hat) * Bond_Excess_Vw_Ret]
  
  
  # set the portfolio weight in each asset class equal to the inverse of its volatility
  #these weights are multiplied by a constant to match the ex-post realized volatility of the Value-Weighted benchmark.
  # For comparison purposes,we set k such that the annualized volatility of this portfolio matches the ex post realized volatility of
  # the benchmark (the value-weighted market portfolio or the 60/40 portfolio).
  # Let's choose value-weighted as our benchmark
  vol_levered_port <- sd(shift(Monthly_CRSP_Universe$Stock_inverse_sigma_hat) * Monthly_CRSP_Universe$Stock_Excess_Vw_Ret 
                         + shift(Monthly_CRSP_Universe$Bond_inverse_sigma_hat) * Monthly_CRSP_Universe$Bond_Excess_Vw_Ret, na.rm = TRUE)
  
  # Formula: Realized_Vol = K * Annualized Volatility of the portfolio
  # Annualized Volatility of the portfolio = sd (inversse sigma hat * Excess value weighted returns)
  k <- sd(Monthly_CRSP_Universe$Excess_Vw_Ret)/vol_levered_port
  
  Monthly_CRSP_Universe[,Levered_k := k]
  
  Monthly_CRSP_Universe[,Excess_Levered_RP_Ret := shift(Levered_k * Stock_inverse_sigma_hat) * Stock_Excess_Vw_Ret 
                        + shift(Levered_k * Bond_inverse_sigma_hat) * Bond_Excess_Vw_Ret]
  Port_Rets <- Monthly_CRSP_Universe[,.(Year, Month,Stock_Excess_Vw_Ret, 
                                        Bond_Excess_Vw_Ret, Excess_Vw_Ret, Excess_60_40_Ret, 
                                        Stock_inverse_sigma_hat,Bond_inverse_sigma_hat,Unlevered_k,Excess_Unlevered_RP_Ret, Levered_k, Excess_Levered_RP_Ret)]
  return(Port_Rets)
}

Port_Rets <- PS2_Q3(Monthly_CRSP_Universe)
write.table(Port_Rets, file = "Port_Rets.csv", row.names=FALSE, sep=",")
################################################################################################ 

rm(list=ls())
######################## PS1 Qn 4 ################################################
# load data Port_Rets from Qn 3output
Port_Rets <- as.data.table(read.csv("Port_Rets.csv"))

# start of the function
PS2_Q4 <- function(Port_Rets){
  # subset data by date 
  Port_Rets[, date:= ymd(paste0(Year,'/',Month,'/01'))]
  Port_Rets <- Port_Rets[date %between% c("1930-01-01", "2010-06-01")]
  Port_Rets[, date:=NULL]
  
  # CRSP stocks
  crsp_stock <- c()
  crsp_stock[1] <-mean(Port_Rets$Stock_Excess_Vw_Ret, na.rm = TRUE) * 12 * 100
  crsp_stock[2] <- t.test(Port_Rets$Stock_Excess_Vw_Ret,na.rm = TRUE)$statistic
  crsp_stock[3] <- sd(Port_Rets$Stock_Excess_Vw_Ret, na.rm = TRUE) * sqrt(12) * 100
  crsp_stock[4] <- as.double(crsp_stock[[1]])/as.double(crsp_stock[[3]])
  crsp_stock[5] <- skewness(Port_Rets$Stock_Excess_Vw_Ret,na.rm = TRUE)
  crsp_stock[6] <- kurtosis(Port_Rets$Stock_Excess_Vw_Ret,na.rm = TRUE) -3
  
  result <- as.data.frame(round(crsp_stock,2), col=1, row.names = 
                            c("Excess Return","t-Stat. of Excess Return",
                              "Volatility","Sharpe Ratio","Skewness","Excess Kurtosis"))
  
  # CRSP bonds
  crsp_bonds <- c()
  crsp_bonds[1] <-mean(Port_Rets$Bond_Excess_Vw_Ret, na.rm = TRUE) * 12 * 100
  crsp_bonds[2] <- t.test(Port_Rets$Bond_Excess_Vw_Ret,na.rm = TRUE)$statistic
  crsp_bonds[3] <- sd(Port_Rets$Bond_Excess_Vw_Ret, na.rm = TRUE) * sqrt(12) * 100
  crsp_bonds[4] <- as.double(crsp_bonds[[1]])/as.double(crsp_bonds[[3]])
  crsp_bonds[5] <- skewness(Port_Rets$Bond_Excess_Vw_Ret,na.rm = TRUE)
  crsp_bonds[6] <- kurtosis(Port_Rets$Bond_Excess_Vw_Ret,na.rm = TRUE) -3
  result <- cbind(result,round(crsp_bonds,2))
  
  # Value weight return
  VW_Portfolio <- c()
  VW_Portfolio[1] <-mean(Port_Rets$Excess_Vw_Ret, na.rm = TRUE) * 12 * 100
  VW_Portfolio[2] <- t.test(Port_Rets$Excess_Vw_Ret,na.rm = TRUE)$statistic
  VW_Portfolio[3] <- sd(Port_Rets$Excess_Vw_Ret, na.rm = TRUE) * sqrt(12) * 100
  VW_Portfolio[4] <- as.double(VW_Portfolio[[1]])/as.double(VW_Portfolio[[3]])
  VW_Portfolio[5] <- skewness(Port_Rets$Excess_Vw_Ret,na.rm = TRUE)
  VW_Portfolio[6] <- kurtosis(Port_Rets$Excess_Vw_Ret,na.rm = TRUE) -3
  result <- cbind(result,round(VW_Portfolio,2))
  
  # 60/40 portfolio
  Excess_60_40_Portfolio <- c()
  Excess_60_40_Portfolio[1] <-mean(Port_Rets$Excess_60_40_Ret, na.rm = TRUE) * 12 * 100
  Excess_60_40_Portfolio[2] <- t.test(Port_Rets$Excess_60_40_Ret,na.rm = TRUE)$statistic
  Excess_60_40_Portfolio[3] <- sd(Port_Rets$Excess_60_40_Ret, na.rm = TRUE) * sqrt(12) * 100
  Excess_60_40_Portfolio[4] <- as.double(Excess_60_40_Portfolio[[1]])/as.double(Excess_60_40_Portfolio[[3]])
  Excess_60_40_Portfolio[5] <- skewness(Port_Rets$Excess_60_40_Ret,na.rm = TRUE)
  Excess_60_40_Portfolio[6] <- kurtosis(Port_Rets$Excess_60_40_Ret,na.rm = TRUE) -3
  result <- cbind(result,round(Excess_60_40_Portfolio,2))
  
  # unlevered RP
  Unlevered_RP_Portfolio <- c()
  Unlevered_RP_Portfolio[1] <-mean(Port_Rets$Excess_Unlevered_RP_Ret, na.rm = TRUE) * 12 * 100
  Unlevered_RP_Portfolio[2] <- t.test(Port_Rets$Excess_Unlevered_RP_Ret,na.rm = TRUE)$statistic
  Unlevered_RP_Portfolio[3] <- sd(Port_Rets$Excess_Unlevered_RP_Ret, na.rm = TRUE) * sqrt(12) * 100
  Unlevered_RP_Portfolio[4] <- as.double(Unlevered_RP_Portfolio[[1]])/as.double(Unlevered_RP_Portfolio[[3]])
  Unlevered_RP_Portfolio[5] <- skewness(Port_Rets$Excess_Unlevered_RP_Ret,na.rm = TRUE)
  Unlevered_RP_Portfolio[6] <- kurtosis(Port_Rets$Excess_Unlevered_RP_Ret,na.rm = TRUE) -3
  result <- cbind(result,round(Unlevered_RP_Portfolio,2))
  
  # levered RP
  Levered_RP_Portfolio <- c()
  Levered_RP_Portfolio[1] <-mean(Port_Rets$Excess_Levered_RP_Ret, na.rm = TRUE) * 12 * 100
  Levered_RP_Portfolio[2] <- t.test(Port_Rets$Excess_Levered_RP_Ret,na.rm = TRUE)$statistic
  Levered_RP_Portfolio[3] <- sd(Port_Rets$Excess_Levered_RP_Ret, na.rm = TRUE) * sqrt(12) * 100
  Levered_RP_Portfolio[4] <- as.double(Levered_RP_Portfolio[[1]])/as.double(Levered_RP_Portfolio[[3]])
  Levered_RP_Portfolio[5] <- skewness(Port_Rets$Excess_Levered_RP_Ret,na.rm = TRUE)
  Levered_RP_Portfolio[6] <- kurtosis(Port_Rets$Excess_Levered_RP_Ret,na.rm = TRUE) -3
  result <- cbind(result, round(Levered_RP_Portfolio,2))
  return(t(result))
}

Q4_summary <- PS2_Q4(Port_Rets)
write.table(Q4_summary, file = "Q4_summary.csv", row.names=FALSE, sep=",")



