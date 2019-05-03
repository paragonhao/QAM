suppressMessages(require(data.table))
suppressMessages(require(lubridate))
suppressMessages(require(zoo))
rm(list=ls())
# import CRPS data 1927 to 2018
crsp_monthly <- as.data.table(read.csv("crsp_monthly.csv"))

######################## PS3 Qn 1 ################################################ 
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

crsp_monthly[,Ranking_Ret := rollapplyr(shifted_log_ret, rollingWin, function(x){
  if(sum(is.na(x)) >4){
    return(NA)
  }else{
    return(sum(x, na.rm = TRUE))
  }
}, fill=NA, align="right", partial = TRUE), by = PERMNO]

crsp_monthly[!isAvailT_minus_13 | !isAvailT_minus_2 | is.na(lag_Mkt_Cap), Ranking_Ret := NA]

crsp_monthly <- crsp_monthly[Ranking_Ret != "NA"]
crsp_final <- crsp_monthly[,.(PERMNO, date, SHRCD, EXCHCD, DLRET, PRC, RET, SHROUT, Year, Month, Ret ,lag_Mkt_Cap, Ranking_Ret)]

# write cumulative return
#write.table(crsp_final, file = "CRSP_Stocks_Momentum.csv", row.names=FALSE, sep=",")


######################## PS3 Qn 2 ################################################ 
#rm(list=ls())
# import CRPS momentum data 1927 to 2018
#CRSP_Stocks_Momentum <- as.data.table(read.csv("CRSP_Stocks_Momentum.csv"))

CRSP_Stocks_Momentum <- crsp_final

# DM decile
CRSP_Stocks_Momentum[,DM_decile := cut(Ranking_Ret, breaks=quantile(Ranking_Ret,probs=c(0:10)/10), 
                                       labels=FALSE, include.lowest = TRUE), .(Year,Month)]

# Fama decile
FFbreakpoints <- quantile(CRSP_Stocks_Momentum[EXCHCD==1,Ranking_Ret],probs=c(0:10)/10)
CRSP_Stocks_Momentum[,KF_decile := cut(Ranking_Ret, breaks=FFbreakpoints, labels=FALSE, include.lowest = T),.(Year,Month)]

# Extreme cumulative loss and gain is not captured by the Fama Decile,manually sort them into 1 and 10 decile
CRSP_Stocks_Momentum[is.na(KF_decile) ,KF_decile := DM_decile]

CRSP_Stocks_Momentum_final <- CRSP_Stocks_Momentum[,.(PERMNO, Year, Month, Ret, lag_Mkt_Cap,DM_decile,KF_decile)]
#write.table(CRSP_Stocks_Momentum_final, file = "CRSP_Stocks_Momentum_decile.csv", row.names=FALSE, sep=",")

######################## PS3 Qn 3 ################################################ 
#rm(list=ls())
# import CRPS momentum to 2018
#CRSP_Stocks_Momentum_decile <- as.data.table(read.csv("CRSP_Stocks_Momentum_decile.csv"))

CRSP_Stocks_Momentum_decile <- CRSP_Stocks_Momentum_final

# import Fama French
FF_mkt <- as.data.table(read.csv("F-F_Research_Data_Factors.csv"))

# convert date to date datatype
FF_mkt[, X:= ymd(X, truncated = 1)]

# change from percentage to decimal
FF_mkt[, RF := RF/100]

# add year month column
FF_mkt[,Year:= year(X)]
FF_mkt[,Month:= month(X)]

# get only year month and rf
FF_mkt_rf <- FF_mkt[, .(Year, Month, RF)]

# separete dm and fama french deciles
DM_decile_data <- CRSP_Stocks_Momentum_decile[,.(PERMNO, Year, Month, Ret, lag_Mkt_Cap, DM_decile)]
FF_decile_data <- CRSP_Stocks_Momentum_decile[,.(PERMNO, Year, Month, Ret, lag_Mkt_Cap, KF_decile)]

# DM portfolio
DM_portfolio <- DM_decile_data[,.(DM_Ret = weighted.mean(Ret, lag_Mkt_Cap, na.rm = TRUE)), .(Year, Month, DM_decile)]
setkey(DM_portfolio, Year, Month, DM_decile)

# KF decile
FF_portfolio <- FF_decile_data[,.(KRF_Ret = weighted.mean(Ret, lag_Mkt_Cap,na.rm = TRUE)), .(Year, Month, KF_decile)]
setkey(FF_portfolio, Year, Month, KF_decile)

# Merge the two portfolios together
merge_port <- merge(DM_portfolio, FF_portfolio, by.x = c("Year", "Month","DM_decile"), by.y = c("Year", "Month","KF_decile"))
merge_port <- merge(merge_port,FF_mkt_rf, by=c("Year", "Month"))

######################## PS3 Qn 4 ################################################ 

result <- merge_port[, list(MeanExRet = mean(DM_Ret - RF), StdDev = sd(DM_Ret - RF)),  by = DM_decile]
result$MeanExRet * 12
result$StdDev * sqrt(12)
