suppressMessages(require(data.table))
suppressMessages(require(lubridate))
suppressMessages(require(zoo))
suppressWarnings(require(dplyr))
suppressWarnings(require(moments))
rm(list=ls())

# Qn 1 
########################## Cleaning up CRSP data ########################## 
# load crsp data from 1972 Dec to 2018 Dec
crsp_monthly<- as.data.table(read.csv("crsp_monthly.csv"))

# convert date to date datatype
crsp_monthly[, date:= ymd(date)]

setorder(crsp_monthly, PERMCO)

# Filter by sharecode 10 and 11  and Filter by EXCHCD 1 2 3
crsp_monthly <- crsp_monthly[SHRCD %in% c(10,11)]
crsp_monthly <- crsp_monthly[EXCHCD %in% c(1,2,3)]

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

# Market Cap and find the MktCap aggregate by PERMCO, which is the same firm
crsp_monthly[, Mkt_cap := abs(PRC) * SHROUT/1000]
setorder(crsp_monthly, PERMNO, date)

# set the year and month as integer
crsp_monthly[, Year:= year(date)]
crsp_monthly[, Month:= month(date)]
######################################################################################## 

##################################### Cleaning up compustat data #####################################
# load computat data from 1970 Dec to 2017 Dec, data only available until 2017 Dec
# compustat_annual$indfmt has FS and INDL , remove FS in this case.
compustat_annual<- as.data.table(read.csv("compustat.csv"))
compustat_PRBA<- as.data.table(read.csv("PRBA.csv"))

# convert date to date datatype
compustat_PRBA[, datadate:= ymd(datadate)]

# extract year from PRBA
compustat_PRBA[, year := year(datadate)]

# merge compustat with PRBA
compustat_PRBA <- compustat_PRBA[,.(gvkey, prba,year)]

setkey(compustat_annual,gvkey,datadate)
setkey(compustat_PRBA,gvkey,year)

compustat_annual <- compustat_annual[datafmt=="STD" & indfmt=="INDL"]

compustat_merged <- merge(compustat_annual, compustat_PRBA, by.x = c("gvkey","fyear"), by.y = c("gvkey","year"), all.x = T)
####################################################################

########################### Calculate fundamental factors on compustat data ###########################
# calculates the shareholders equity (SHE)
compustat_merged[,SHE:= coalesce(seq, ceq + pstk, at - lt - mib, at - lt)]

# calculates DT
compustat_merged[,DT:= coalesce(txditc, itcb + txdb, itcb, txdb)]

# calculates book value of preferred stock (PS)
compustat_merged[, PS:= coalesce(pstkrv, pstkl, pstk)]

# calculates book equity
compustat_merged[,c("MinusPS","MinusPrba"):=.(-PS, -prba)]
compustat_merged[,BE:=rowSums(.SD, na.rm = T),.SDcols=c("SHE","MinusPS","DT","MinusPrba")]
compustat_merged[is.na(SHE), BE:=NA]
#####################################################################################


########################## Merge Linktable and Compustat data ##########################
linktable <- as.data.table(read.csv("linktable.csv"))

#### code from TA session
merged <- merge(crsp_monthly, linktable, by.x='PERMCO', by.y = 'LPERMCO', allow.cartesian = T)
setkey(merged)

merged[,LINKDT := ymd(LINKDT)]
merged[LINKENDDT == 'E', LINKENDDT := NA]
merged[,LINKENDDT := ymd(LINKENDDT)]
merged <- merged[(is.na(LINKDT) | date >= LINKDT) & (is.na(LINKENDDT) | date <= LINKENDDT)]
setorder(merged, gvkey, date)

# Multiple GVKEYs per PERMCO

### First, if LC not LC linktype, only keep LC
# identify Same PERMCO but different PERMNO
merged[, prob := .N > 1, by = .(PERMCO, date)]
merged[, Good_match := sum(LINKTYPE == 'LC'), by =.(PERMCO, date)]
merged <- merged[!(prob == T & Good_match == T & LINKTYPE != 'LC')]

### Second, if P and not P linkprim, only keep p
merged[, prob := .N > 1, by= .(PERMCO, date)]
merged[, Good_match := sum(LINKPRIM == 'P'), by =.(PERMCO, date)]
merged <- merged[!(prob == T & Good_match == T & LINKPRIM != 'P')]

### Third, if 1 and not liid, only keep 1 
merged[, prob := .N > 1, by = .(PERMCO, date)]
merged[, Good_match := sum(LIID == 1), by =.(PERMCO,date)]
merged <- merged[!(prob == T & Good_match == T & LIID != 1)]

### Fourth, use the link that's current
merged[, prob := .N > 1, by = .(PERMCO, date)]
merged[, Good_match := sum(is.na(LINKENDDT)), by = .(PERMCO, date)]
merged <- merged[!(prob==T & Good_match == T & !is.na(LINKENDDT))]

### Fifth, use the link that's been around the longest
merged[, prob := .N > 1, by = .(PERMCO, date)]
merged[, Good_match := NULL]
merged[is.na(LINKENDDT), LINKENDDT := as.Date('2019-12-31', '%Y-%m-%d')]
merged[, Date_diff := as.integer(LINKENDDT - LINKDT)]
setorder(merged, PERMCO, date, Date_diff)
merged[prob == T, Good_match := Date_diff == Date_diff[.N], by = .(PERMCO, date)]
merged <- merged[!(prob==T & Good_match != T)]

### Sixth, use the gvkey that has been around the longest
merged[, prob := .N > 1, by = .(PERMCO, date)]
merged[, Good_match :=NULL]
setorder(merged, gvkey, LINKDT)
merged[prob == T, start_Date := LINKDT[1], by = .(gvkey)]
setorder(merged, gvkey, LINKENDDT)
merged[prob == T, end_Date := LINKENDDT[.N], by = .(gvkey)]
merged[, Date_diff := as.integer(end_Date - start_Date)]
setorder(merged, PERMCO, date, Date_diff)
merged[prob == T, Good_match := Date_diff == Date_diff[.N], by = .(PERMCO, date)]
merged <- merged[!(prob == T & Good_match != T)]

### Seventh, use the smaller gvkey
setorder(merged, PERMCO, date, gvkey)
merged <- unique(merged, by = c('PERMCO', 'date'))

### Clean up extra variables and final check of match
merged <- merged[, .(gvkey, date, EXCHCD, Mkt_cap, PERMCO, PERMNO, Ret , Year, Month)]

#####################################################################################

################################ Merge CRSP and Compustat together ############################################### 
compustat_merged_data <- compustat_merged[, .(gvkey, fyear, BE, at)]

finaldata <- merged %>%
  left_join(compustat_merged_data,by =c("gvkey"="gvkey","Year"="fyear"))%>%
  select(gvkey, date, PERMNO, PERMCO, EXCHCD, Ret, Mkt_cap, BE, at)%>% as.data.table

#####################################################################################

########################## Find the size portfolio return #####################################

# set the year and month as integer
finaldata[, Year:= year(date)]
finaldata[, Month:= month(date)]
setorder(finaldata, Year, Month)


crsp_monthly_cleaned<- finaldata[, MktCap := sum(Mkt_cap,na.rm = T), .(PERMCO, Year, Month)]

# PERMCO is a unique permanent identifier assigned by CRSP to all companies with issues on a CRSP file. 
# This number is permanent for all securities issued by this company regardless of name changes.
# The PERMNO identifies a firm's security through all its history, and companies may have several stocks at one time.
# In short: A PERMCO can have multiple PERMNOs

# Adds up stocks from the same company based on PERMCO, 
crsp_mergedBy_JUNE <- crsp_monthly_cleaned[Month == 6, .(PERMCO, Year, MktCap, EXCHCD)]

# sorting into deciles
# NYSE stocks are used as breakpoints for all the stocks in NYSE AMEX and NASDAQ
crsp_mergedBy_JUNE[,size_decile := findInterval(MktCap, quantile(.SD[EXCHCD==1,MktCap], seq(0.1,0.9,0.1)), left.open = T) + 1,by = .(Year)]

crsp_mergedBy_JUNE <- crsp_mergedBy_JUNE[,.(PERMCO, Year, size_decile)]

setorder(crsp_monthly_cleaned, Year, Month)
setorder(crsp_mergedBy_JUNE, Year)

crsp_size_merged <- merge(crsp_monthly_cleaned, crsp_mergedBy_JUNE, by = c("PERMCO","Year"), all.x = T)

setkey(crsp_size_merged, Year, Month, size_decile)

# shift by 6 months
crsp_size_merged[,c("size_Rank","lagged_MktCap") := .(shift(size_decile, 6), shift(MktCap,1)), .(PERMNO)]

# remove the NAs and 0s from the lagged market
crsp_size_merged <- crsp_size_merged[lagged_MktCap != 0]
crsp_size_merged <- crsp_size_merged[!is.na(size_Rank) & !is.na(lagged_MktCap)]

# size portfolio
size_portfolio <- crsp_size_merged[,.(Size_Ret = weighted.mean(Ret, lagged_MktCap, na.rm = TRUE)), .(Year, Month, size_Rank)]
setkey(size_portfolio, Year, Month, size_Rank)

# cut data from 1973 jan
size_portfolio <- size_portfolio[Year>1972]

write.table(size_portfolio, file = "size_portfolio.csv", row.names=FALSE, sep=",")
write.table(crsp_size_merged, file ="size_10Deciles.csv", row.names=FALSE, sep=",")
#####################################################################################

#################################### Find the BE/MEB portfolio ####################################
historical_BE_raw_data <- as.data.table(read.csv("DFF_BE_With_Nonindust.csv", header = F)) # from Fama French website

BE_data <- finaldata[Month == 12,]

# handle historical BE 
historical_BE <- historical_BE_raw_data[,c(-2,-3)]
colnames(historical_BE) <- c("PERMNO", 1926:2001)
historical_BE <- as.data.table(melt(historical_BE, id.vars = c("PERMNO")))
historical_BE[, Year := as.numeric(as.character(variable))]
historical_BE[, variable :=NULL]
historical_BE <- historical_BE[value != -99.99 & Year >= 1970]

# merge with crsp and compustat
crsp_compustat_BE_merged <- merge(BE_data, historical_BE,by.x = c("PERMNO","Year"),by.y=c("PERMNO","Year"),all.x=T)
crsp_compustat_BE_merged[is.na(BE) & !is.na(value), BE := value]

# clean up crps and compustat merged data
# if total asset is NA, assign BE to be NA as well, is BE is 0, assign BE to be NA
crsp_compustat_BE_merged[BE == 0 | (is.na(at)), BE:=NA]
crsp_compustat_BE_merged[,value := NULL]
crsp_compustat_BE_merged[,BEME := BE/MktCap]

# get rid of NAs for sorting later
crsp_compustat_BE_merged <- crsp_compustat_BE_merged[!is.na(BEME) & BEME>=0]

# sorting into deciles
# NYSE stocks are used as breakpoints for all the stocks in NYSE AMEX and NASDAQ
crsp_compustat_BE_merged[,BEME_decile := findInterval(BEME, quantile(.SD[EXCHCD==1,BEME], seq(0.1,0.9,0.1)), left.open = T) + 1,by = .(Year, Month)]

# shift ranking forward by a year
crsp_compustat_BE_merged[,lagged_BEME_decile := shift(BEME_decile), by=PERMNO]

setorder(finaldata, Year, Month)
setorder(crsp_compustat_BE_merged, Year)

# MERGE the BEME decile information back to crsp cleaned up data.
crsp_compustat_BE_final <- crsp_compustat_BE_merged[, .(PERMNO,PERMCO, Year, BEME_decile, lagged_BEME_decile)]
crsp_BE_decile_merged <- merge(finaldata, crsp_compustat_BE_final, by = c("PERMNO","PERMCO","Year"), all.x = T)

# shift from t to june of t+1
crsp_BE_decile_merged[,c("BEME_Rank","lagged_MktCap") := .(shift(lagged_BEME_decile, 6), shift(MktCap, 1)), .(PERMNO)]

# clean up data 
crsp_BEME_final <- crsp_BE_decile_merged[!is.na(BEME_Rank) & !is.na(lagged_MktCap) & lagged_MktCap!=0.0]

# BE/ME portfolio
BEME_portfolio <- crsp_BEME_final[,.(BtM_Ret = weighted.mean(Ret, lagged_MktCap, na.rm = TRUE)), .(Year, Month, BEME_Rank)]
setkey(BEME_portfolio, Year, Month, BEME_Rank)

# cut data from 1973 jan
BEME_portfolio <- BEME_portfolio[Year>1972]

write.table(crsp_BEME_final, file ="BEME_10Deciles.csv", row.names=FALSE, sep=",")
write.table(BEME_portfolio, file = "BEME_portfolio.csv", row.names=FALSE, sep=",")
#####################################################################################

#################################### Find the SMB portfolio ####################################
rm(list=ls())
#Note: The B/M breakpoints for a region are the 30th and 70th percentiles of B/M for the big stocks of the region.
BEME_10Deciles<- as.data.table(read.csv("BEME_10Deciles.csv"))

# Take the median as the break point
size_10Deciles<- as.data.table(read.csv("size_10Deciles.csv"))

# construct SMB and HML
size_10Deciles[,SB:=ifelse(size_Rank<6,"S","B")]
BEME_10Deciles[,HML:=ifelse(BEME_Rank<4,"L",ifelse(BEME_Rank<8,"M","H"))]

BEME_Rank <- BEME_10Deciles[,.(PERMNO, PERMCO, Year, Month, Ret, HML,lagged_MktCap)]
Size_Rank <- size_10Deciles[,.(PERMNO, PERMCO, Year, Month, Ret, SB, lagged_MktCap)]

setkey(BEME_Rank,PERMCO,PERMNO,Year,Month)
setkey(Size_Rank,PERMCO,PERMNO,Year,Month)

# Merge the two 
sizeBEME_portfolios <- merge(Size_Rank,BEME_Rank)

# SMB and HML are all equal weighted 
SMB_HML<- sizeBEME_portfolios[,.(Ret = weighted.mean(Ret.x, lagged_MktCap.x, na.rm = T)),.(Year,Month, HML,SB)]
SMB_HML <- SMB_HML[Year>1972]
setkey(SMB_HML, Year,Month)

# SMB is defined as (SL + SM + SH)/3 - (BL + BM + BH)/3
# HMLis defined as (SH + BH)/2 - (SL + BH)/2
SMB <- SMB_HML[,.(SMB_Ret = (.SD[SB=="S" & HML=="L",Ret] + .SD[SB=="S" & HML=="M",Ret]+ .SD[SB=="S" & HML=="H",Ret] - .SD[SB=="B" & HML=="L",Ret] - .SD[SB=="B" & HML=="M",Ret] - .SD[SB=="B" & HML=="H",Ret])/3), by =.(Year, Month)]
HML <- SMB_HML[, .(HML_Ret =(.SD[SB=="S" & HML=="H",Ret] + .SD[SB=="B" & HML=="H",Ret] -.SD[SB=="S" & HML=="L",Ret] - .SD[SB=="B" & HML=="L",Ret])/2), by =.(Year, Month)]

################################################################################################

#################################### Summarize the result  ###########################################
size_portfolio <-  as.data.table(read.csv("size_portfolio.csv"))
BEME_portfolio <-  as.data.table(read.csv("BEME_portfolio.csv"))
Q1_summary <- merge(size_portfolio, BEME_portfolio, by.x = c("Year", "Month", "size_Rank"), by.y = c("Year", "Month", "BEME_Rank"))
Q1_summary<- merge(Q1_summary,HML)
Q1_summary<- merge(Q1_summary,SMB)
setnames(Q1_summary, "size_Rank","port")
write.table(Q1_summary, file = "Q1_summary.csv", row.names=FALSE, sep=",")
#####################################################################################

# Qn 2

####################################### output stats ME & Long short portfolio #############################################
FF_Factors<- as.data.table(read.csv("F-F_Research_Data_Factors.csv"))
size_FF_portfolio<- as.data.table(read.csv("Portfolios_Formed_on_ME.csv"))

size_FF_portfolio[,date:= ymd(X, truncated = 1)]
FF_Factors[, date:= ymd(X, truncated = 1)]

FF_Factors<- FF_Factors[date>"1972-12-01", .(Year = year(date), Month = month(date), SMB,HML,RF)]
setorder(FF_Factors,Year,Month)

size_FF_portfolio <- size_FF_portfolio[date>"1972-12-01" & date<"2019-01-01",]
size_FF_portfolio <- size_FF_portfolio[, c(11:21)]
colnames(size_FF_portfolio) <- c(1:10,"date")
size_FF_portfolio <- melt(size_FF_portfolio, id.vars = "date")

size_FF_portfolio[, Year := year(date)]
size_FF_portfolio[, Month := month(date)]

# change to decimal 
size_FF_portfolios <- size_FF_portfolio[, .(Year, Month, Ret= value/100, Size_rank = variable)]
FF_Factors <- FF_Factors[, .(Year, Month, SMB = SMB/100, HML= HML/100, RF= RF/100)]

size_FF_portfolios_merged <- merge(size_FF_portfolios, FF_Factors[, c(1,2,5)], by =c("Year","Month"))

# compute stats
ME_mat <- matrix(nrow=5, ncol=11,dimnames = list(c("Excess Return","Standard Deviation",
                                                     "Sharpe Ratio","Skewness","Correlation"), c(1:10,"LongShort")))

for(i in 1:10){
  ME_mat[1,i] <- mean(size_portfolio[size_Rank==i,Size_Ret] - size_FF_portfolios_merged[Size_rank==i,RF])*12
  ME_mat[2,i] <- sd(size_portfolio[size_Rank==i,Size_Ret])*sqrt(12)
  ME_mat[3,i] <- ME_mat[1,i]/ME_mat[2,i]
  ME_mat[4,i] <- skewness(size_portfolio[size_Rank==i,Size_Ret])
  ME_mat[5,i] <- cor(size_portfolio[size_Rank==i,Size_Ret], size_FF_portfolios_merged[Size_rank==i,Ret])
}

WML_ME <- size_portfolio[size_Rank==1,Size_Ret] - size_FF_portfolios_merged[Size_rank==10,Ret]
ME_mat[1,11] <- mean(WML_ME)*12
ME_mat[2,11] <- sd(WML_ME)*sqrt(12)
ME_mat[3,11] <- ME_mat[1,11]/ME_mat[2,11]
ME_mat[4,11] <- skewness(WML_ME)
ME_mat[5,11] <- cor(WML_ME, (size_FF_portfolios_merged[Size_rank==1,Ret] - size_FF_portfolios_merged[Size_rank==10,Ret]))

write.table(ME_mat, file = "ME_Matrix.csv", row.names=FALSE, sep=",")
####################################### output stats BEME & Long short portfolio #############################################
BEME_FF_Portfolio <- as.data.table(read.csv("Portfolios_Formed_on_BE-ME.csv"))
BEME_FF_Portfolio[,date:= ymd(X, truncated = 1)]

BEME_FF_Portfolio <- BEME_FF_Portfolio[date>"1972-12-01" & date<"2019-01-01",]
BEME_FF_Portfolio <- BEME_FF_Portfolio[, c(11:21)]
colnames(BEME_FF_Portfolio) <- c(1:10,"date")
BEME_FF_Portfolio <- melt(BEME_FF_Portfolio, id.vars = "date")

BEME_FF_Portfolio[, Year := year(date)]
BEME_FF_Portfolio[, Month := month(date)]

# change to decimal 
BEME_FF_Portfolio <- BEME_FF_Portfolio[, .(Year, Month, Ret= value/100, BEME_rank = variable)]

BEME_FF_Portfolio_merged <- merge(BEME_FF_Portfolio, FF_Factors[, c(1,2,5)], by =c("Year","Month"))

# compute stats
BEME_mat <- matrix(nrow=5, ncol=11,dimnames = list(c("Excess Return","Standard Deviation",
                                                   "Sharpe Ratio","Skewness","Correlation"), c(1:10,"LongShort")))

for(i in 1:10){
  BEME_mat[1,i] <- mean(BEME_portfolio[BEME_Rank==i,BtM_Ret] - BEME_FF_Portfolio_merged[BEME_rank==i,RF])*12
  BEME_mat[2,i] <- sd(BEME_portfolio[BEME_Rank==i,BtM_Ret])*sqrt(12)
  BEME_mat[3,i] <- BEME_mat[1,i]/BEME_mat[2,i]
  BEME_mat[4,i] <- skewness(BEME_portfolio[BEME_Rank==i,BtM_Ret])
  BEME_mat[5,i] <- cor(BEME_portfolio[BEME_Rank==i,BtM_Ret], BEME_FF_Portfolio_merged[BEME_rank == i,Ret])
}

WML_BEME <- BEME_portfolio[BEME_Rank==1,BtM_Ret] - BEME_FF_Portfolio_merged[BEME_rank==10,Ret]
BEME_mat[1,11] <- mean(WML_BEME)*12
BEME_mat[2,11] <- sd(WML_BEME)*sqrt(12)
BEME_mat[3,11] <- BEME_mat[1,11]/BEME_mat[2,11]
BEME_mat[4,11] <- skewness(WML_BEME)
BEME_mat[5,11] <- cor(WML_BEME, (BEME_FF_Portfolio_merged[BEME_rank==1,Ret] - BEME_FF_Portfolio_merged[BEME_rank==10,Ret]))

write.table(BEME_mat, file = "BEME_Matrix.csv", row.names=FALSE, sep=",")
####################################### output stats SMB portfolio #############################################

setkey(SMB,Year,Month)
setkey(FF_Factors,Year,Month)
# compute stats
SMB_mat <- matrix(nrow=5, ncol=1, dimnames = list(c("Excess Return","Standard Deviation",
                                                     "Sharpe Ratio","Skewness","Correlation")))

SMB_mat[1,1] <- mean(SMB$SMB_Ret - FF_Factors$RF)*12
SMB_mat[2,1] <- sd(SMB$SMB_Ret)*sqrt(12)
SMB_mat[3,1] <- SMB_mat[1,1]/SMB_mat[2,1]
SMB_mat[4,1] <- skewness(SMB$SMB_Ret)
SMB_mat[5,1] <- cor(SMB$SMB_Ret, FF_Factors$SMB)

write.table(SMB_mat, file = "SMB_Matrix.csv", row.names=FALSE, sep=",")
####################################### output stats HML portfolio #############################################

setkey(HML,Year,Month)
setkey(FF_Factors,Year,Month)
# compute stats
HML_mat <- matrix(nrow=5, ncol=1, dimnames = list(c("Excess Return","Standard Deviation",
                                                    "Sharpe Ratio","Skewness","Correlation")))

HML_mat[1,1] <- mean(HML$HML_Ret - FF_Factors$RF)*12
HML_mat[2,1] <- sd(HML$HML_Ret)*sqrt(12)
HML_mat[3,1] <- HML_mat[1,1]/HML_mat[2,1]
HML_mat[4,1] <- skewness(HML$HML_Ret)
HML_mat[5,1] <- cor(HML$HML_Ret, FF_Factors$HML)

write.table(HML_mat, file = "HML_Matrix.csv", row.names=FALSE, sep=",")
