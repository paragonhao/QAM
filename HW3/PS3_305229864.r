suppressMessages(require(data.table))
suppressMessages(require(lubridate))
suppressMessages(require(zoo))
suppressMessages(require(moments))

rm(list=ls())
# import CRPS data 1927 to 2018
crsp_monthly <- as.data.table(read.csv("crsp_monthly.csv"))

######################## PS3 Qn 1 ################################################ 
PS3_Q1 <- function(crsp_monthly){
  
  # set keys, sort by date
  setkey(crsp_monthly, date)
  
  # convert date to date datatype
  crsp_monthly[, date:= ymd(date)]
  
  # Filter by sharecode 10 and 11  and Filter by EXCHCD 1 2 3
  crsp_monthly <- crsp_monthly[SHRCD %in% c(10,11)]
  crsp_monthly <- crsp_monthly[EXCHCD %in% c(1,2,3)]
  
  # set the year and month as integer
  crsp_monthly[,Year:= year(date)]
  crsp_monthly[,Month:= month(date)]
  crsp_monthly[, YrMo := Year * 12 + Month]
  
  # Filter out missing ret and dlret data
  for(i in c('RET','DLRET')){
    crsp_monthly[,paste0(i) := as.character(get(i))]
    crsp_monthly[get(i) %in% c('', ' ','A','C','P','S','T','B','C'), paste0(i) := NA]
    crsp_monthly[, paste0(i) := as.numeric(get(i))]
    crsp_monthly[get(i) %in% c(-66,-77,-88,-99), paste0(i) := NA]
  }
  
  # convert the ret and delisting return into numeric data type for easy calculation
  crsp_monthly[, PRC := abs(as.numeric(as.character(PRC)))]
  crsp_monthly[, SHROUT := as.numeric(as.character(SHROUT))]
  
  # calculates the cum-Dividend returns
  crsp_monthly[, `:=`(Ret, ifelse(is.na(DLRET),RET,ifelse(is.na(RET), DLRET, (1+DLRET)*(1+RET)-1 )))]
  
  # log the ret, shift 2 period
  crsp_monthly[, shifted_log_ret := log(shift(1 + Ret, n = 2, type = "lag")), by= PERMNO]
  
  # Market Cap 
  crsp_monthly[, Mkt_cap := abs(PRC) * SHROUT]
  setorder(crsp_monthly, PERMNO, date)
  crsp_monthly[, lag_Mkt_Cap := shift(Mkt_cap), by = PERMNO]
  
  # to get rid of the stocks that has less than 13 months of return (current month + past 12 months = 13 months)
  crsp_monthly[, one := 1]
  permno_grp_by <- aggregate(crsp_monthly$one, by=list(PERMNO = crsp_monthly$PERMNO), FUN=sum)
  permnos_to_remove <- permno_grp_by[permno_grp_by$x<13,]$PERMNO
  crsp_monthly <- crsp_monthly[!PERMNO %in% permnos_to_remove]
  
  #Find if the price at t-13 is missing , and if ret at t-2 is missing 
  rollingWin <- 11
  crsp_monthly[,c("isAvailT_minus_13","isAvailT_minus_2") := .(!is.na(shift(PRC,13)), !is.na(shifted_log_ret)), by = PERMNO]
  
  # calculate ranking return based on log return of the stocks
  crsp_monthly[,Ranking_Ret := rollapply(shifted_log_ret, rollingWin, function(x){
    # must have at least 8 returns in the 11 window
    if(sum(is.na(x)) >4){
      return(NA)
    }else{
      return(sum(x, na.rm = TRUE))
    }
  }, fill=NA, align="right", partial = TRUE), by = PERMNO]
  
  # assign NAs to Ranking return based on data restriction 
  crsp_monthly[!isAvailT_minus_13 | !isAvailT_minus_2 | is.na(lag_Mkt_Cap), Ranking_Ret := NA]
  
  crsp_monthly <- crsp_monthly[Ranking_Ret != "NA"]
  return(crsp_monthly[,.(Year, Month, PERMNO, EXCHCD,lag_Mkt_Cap, Ret, Ranking_Ret)])
}

CRSP_Stocks_Momentum <- PS3_Q1(crsp_monthly)
# write cumulative return
write.table(CRSP_Stocks_Momentum, file = "CRSP_Stocks_Momentum.csv", row.names=FALSE, sep=",")
################################################################################################

######################## PS3 Qn 2 ################################################ 
rm(list=ls())

# import CRPS momentum data 1927 to 2018
CRSP_Stocks_Momentum <- as.data.table(read.csv("CRSP_Stocks_Momentum.csv"))

PS3_Q2 <- function(CRSP_Stocks_Momentum){
  # DM decile
  CRSP_Stocks_Momentum[,DM_decile := cut(Ranking_Ret, breaks=quantile(Ranking_Ret,probs=c(0:10)/10), 
                                         labels=FALSE, include.lowest = TRUE), .(Year,Month)]
  
  # Fama decile
  CRSP_Stocks_Momentum[,KF_decile := cut(Ranking_Ret, breaks=quantile(.SD[EXCHCD==1,Ranking_Ret],probs=c(0:10)/10), 
                                         labels=FALSE, include.lowest = T), .(Year,Month)]
  
  # Extreme cumulative loss and gain is not captured by the Fama Decile,manually sort them into 1 and 10 decile
  CRSP_Stocks_Momentum[is.na(KF_decile) ,KF_decile := DM_decile]
  
  return(CRSP_Stocks_Momentum[,.(PERMNO, Year, Month, Ret, lag_Mkt_Cap, DM_decile,KF_decile)])
}
CRSP_Stocks_Momentum_decile <- PS3_Q2(CRSP_Stocks_Momentum)
write.table(CRSP_Stocks_Momentum_decile, file = "CRSP_Stocks_Momentum_decile.csv", row.names=FALSE, sep=",")
########################################################################################################################     

######################## PS3 Qn 3 ################################################ 
rm(list=ls())

# import CRPS momentum to 2018
CRSP_Stocks_Momentum_decile <- as.data.table(read.csv("CRSP_Stocks_Momentum_decile.csv"))

# import Fama French
FF_mkt <- as.data.table(read.csv("F-F_Research_Data_Factors.csv"))

# convert date to date datatype
FF_mkt[, X:= ymd(X, truncated = 1)]

# change from percentage to decimal
FF_mkt[, RF := RF/100]

# add year month column
FF_mkt[,Year:= year(X)]
FF_mkt[,Month:= month(X)]

# 1927 to 2018
FF_mkt <- FF_mkt[Year>=1927 & Year<=2018]

PS3_Q3 <- function(CRSP_Stocks_Momentum_decile, FF_mkt){
  
  # get only year month and rf
  FF_mkt_rf <- FF_mkt[, .(Year, Month, RF)]
  
  # separete dm and fama french deciles
  DM_decile_data <- CRSP_Stocks_Momentum_decile[,.(PERMNO, Year, Month, Ret, lag_Mkt_Cap, DM_decile)]
  KRF_decile_data <- CRSP_Stocks_Momentum_decile[,.(PERMNO, Year, Month, Ret, lag_Mkt_Cap, KF_decile)]
  
  # DM portfolio
  DM_portfolio <- DM_decile_data[,.(DM_Ret = sum(lag_Mkt_Cap * Ret, na.rm = T)/(sum(lag_Mkt_Cap,na.rm=T))), .(Year, Month, DM_decile)]
  setkey(DM_portfolio, Year, Month, DM_decile)
  
  # KF decile
  KRF_portfolio <- KRF_decile_data[,.(KRF_Ret = sum(lag_Mkt_Cap * Ret, na.rm = T)/(sum(lag_Mkt_Cap,na.rm=T))), .(Year, Month, KF_decile)]
  setkey(KRF_portfolio, Year, Month, KF_decile)
  
  # Merge the two portfolios together
  merge_port <- merge(DM_portfolio, KRF_portfolio, by.x = c("Year", "Month","DM_decile"), by.y = c("Year", "Month","KF_decile"), all.x = T)
  merge_port <- merge(merge_port,FF_mkt_rf, by=c("Year", "Month"))
  colnames(merge_port) <- c("Year","Month","decile","DM_Ret","KRF_Ret","Rf")
  return(merge_port)
}
CRSP_Stocks_Momentum_returns <- PS3_Q3(CRSP_Stocks_Momentum_decile, FF_mkt)
write.table(CRSP_Stocks_Momentum_returns, file = "CRSP_Stocks_Momentum_returns.csv", row.names=FALSE, sep=",")
################################################################################################ 

######################## PS3 Qn 4 ################################################ 
rm(list=ls())

# import CRPS momentum returns
CRSP_Stocks_Momentum_returns <- as.data.table(read.csv("CRSP_Stocks_Momentum_returns.csv"))

PS3_Q4 <- function(merge_port){
  # create an empty matrix
  summary <- matrix(0, nrow = 4, ncol =11)
  
  result <- merge_port[, list(MeanExRet = mean(DM_Ret - Rf), StdDev = sd(DM_Ret - Rf)),  by = decile]
  summary[1, 1:10] <- result$MeanExRet * 12
  summary[2, 1:10] <- result$StdDev * sqrt(12)
  summary[3, 1:10] <- summary[1, 1:10]/summary[2, 1:10]
  summary[4, 1:10]  <- merge_port[,  list(sk_m = skewness(log(1+DM_Ret))), by = decile]$sk_m
  colnames(summary) <- c("Decile 1","Decile 2","Decile 3","Decile 4","Decile 5"
                         ,"Decile 6","Decile 7","Decile 8","Decile 9","Decile 10","WML")
  rownames(summary) <- c("Excess Return","Volatility", "SR", "SK(m)")
  summary[1, 1:10] <- summary[1, 1:10] * 100
  summary[2, 1:10] <- summary[2, 1:10] * 100
  
  # winner minus losers
  winner <- merge_port[decile ==10,]
  loser <- merge_port[decile ==1,]
  
  wml <-  winner$DM_Ret - loser$DM_Ret
  
  summary[1, 11] <- mean(wml)*12 * 100
  summary[2, 11] <- sd(wml) * sqrt(12) * 100
  summary[3, 11] <- summary[1, 11]/summary[2, 11]
  summary[4, 11] <- skewness(log(1 + wml + merge_port[decile ==1, Rf ]))
  return(summary)
}

summary <- PS3_Q4(CRSP_Stocks_Momentum_returns)
write.table(summary, file = "Qn4_summary.csv", row.names=FALSE, sep=",")
################################################################################################ 

####################################################### PS3 Qn 5 ################################################
rm(list=ls())

# import CRPS momentum returns
CRSP_Stocks_Momentum_returns <- as.data.table(read.csv("CRSP_Stocks_Momentum_returns.csv"))

############################## Parsing FF 10 portfolios momentum data 
# select 10 portflio momentum
KRF_returns <- as.data.table(read.csv("10_Portfolios_Prior_12_2.csv", header = T))
colnames(KRF_returns) <- c("date", 1:10)
KRF_returns[, date := ymd(date, truncated = 1)]

# reshape the portfolio
KRF_returns <- melt(KRF_returns, id=c("date"))
setkey(KRF_returns, date)

# set the year and month as integer
KRF_returns[, Year:= year(date)]
KRF_returns[, Month:= month(date)]

# select year to end at dec 2018 12
KRF_returns <- KRF_returns[Year<2019 & Year >1926]

# change returnt o decimal
KRF_returns[, value := value/100]
KRF_returns[, date := NULL]
colnames(KRF_returns) <- c("decile", "KRF_Ret_actual","Year", "Month")
############################## End of parsing

# initialize empty result matrix
Qn5_result <- matrix(0, nrow=2,ncol=11)

# select m_m_pt_tot.txt monthly all firms total return 
DM_returns <- as.data.table(read.csv("m_m_pt_tot.csv", header = FALSE))[,1:3]
colnames(DM_returns) <- c("date", "decile","DM_Ret")

# set the year and month as integer
DM_returns[, Year:= year(ymd(date))]
DM_returns[, Month:= month(ymd(date))]
DM_returns[, date := NULL]

################### correlation between DM estimate and actual 1927 to 2016
DM_2016_estimate <- CRSP_Stocks_Momentum_returns[Year < 2017]
for(i in 1:10){
  DM_Ret_estimate <- DM_2016_estimate[decile==i, DM_Ret]
  DM_Actual <- DM_returns[decile==i, DM_Ret]
  Qn5_result[1,i] <- cor(DM_Ret_estimate, DM_Actual)
}

################### winner minus losers DM 
winner_est <- DM_2016_estimate[decile ==10]
loser_est <- DM_2016_estimate[decile ==1]
wml_est <-  winner_est$DM_Ret - loser_est$DM_Ret

winner_actual <- DM_returns[decile ==10]
loser_actual <- DM_returns[decile ==1]
wml_actual <- winner_actual$DM_Ret - loser_actual$DM_Ret

Qn5_result[1, 11] <- cor(wml_est, wml_actual)


################### correlation between KRF estimate and actual 1927 to 2018
setkey(CRSP_Stocks_Momentum_returns, Year, Month, decile)
setkey(KRF_returns, Year, Month, decile)

# start of the function
# correlation between KRF estimate and actual 
for(i in 1:10){
  KRF_Ret_estimate <- CRSP_Stocks_Momentum_returns[decile==i, KRF_Ret]
  KRF_Actual <- KRF_returns[decile==i, KRF_Ret_actual]
  Qn5_result[2,i] <-cor(KRF_Ret_estimate, KRF_Actual)
}

################### winner minus losers KRF 
winner_est <- CRSP_Stocks_Momentum_returns[decile ==10]
loser_est <- CRSP_Stocks_Momentum_returns[decile ==1]
wml_est <-  winner_est$KRF_Ret - loser_est$KRF_Ret

winner_actual <- KRF_returns[decile ==10]
loser_actual <- KRF_returns[decile ==1]
wml_actual <- winner_actual$KRF_Ret - loser_actual$KRF_Ret

Qn5_result[2, 11] <- cor(wml_est, wml_actual)



