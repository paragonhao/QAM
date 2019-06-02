suppressMessages(require(data.table))
suppressMessages(require(lubridate))
suppressWarnings(require(zoo))
suppressWarnings(require(dplyr))
rm(list=ls())

########################## Cleaning up CRSP data ########################## 
# load crsp data from 1960  to 2018 Dec
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
# load computat data from 1960 to 2017 Dec, data only available until 2017 Dec
compustat_annual<- as.data.table(read.csv("compustat_1960_2018.csv"))
compustat_PRBA<- as.data.table(read.csv("PRBA.csv"))

# convert date to date datatype
compustat_PRBA[, datadate:= ymd(datadate)]

# extract year from PRBA
compustat_PRBA[, year := year(datadate)]

# merge compustat with PRBA
compustat_PRBA <- compustat_PRBA[,.(gvkey, prba,year)]

setkey(compustat_annual,gvkey,datadate)
setkey(compustat_PRBA,gvkey,year)

compustat_merged <- merge(compustat_annual, compustat_PRBA, by.x = c("gvkey","fyear"), by.y = c("gvkey","year"), all.x = T)
#####################################################################################

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

# calculates cashflow = investing net + financing net + operating net (fincf, oancf, ivncf)
compustat_merged[, CF_NET := coalesce(fincf + oancf + ivncf, 
                                      fincf + oancf, 
                                      fincf + ivncf, 
                                      oancf + ivncf, fincf, oancf, ivncf)]

#####################################################################################

########################## Merge Linktable and CRSP data ##########################
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
compustat_merged_data <- compustat_merged[, .(gvkey, fyear, at, BE, CF_NET, dvt, revt, emp, sale)]

finaldata <- merged %>%
  left_join(compustat_merged_data,by =c("gvkey"="gvkey","Year"="fyear"))%>%
  select(gvkey, date, PERMNO, PERMCO, EXCHCD, Ret, Mkt_cap, at, BE, CF_NET, dvt, revt, emp, sale) %>% as.data.table

setorder(finaldata, date)

# Computstat only has data until 2017 dec, remove all data in 2018 which only have NAs on the fundamentals
finaldata[, Year:= year(date)]
finaldata[, Month:= month(date)]
finaldata <- finaldata[Year < 2018,]
write.table(finaldata, file ="final_data.csv", row.names=FALSE, sep=",")
#####################################################################################

################################## find the trailing data ###################################
# PERMCO is a unique permanent identifier assigned by CRSP to all companies with issues on a CRSP file. 
# This number is permanent for all securities issued by this company regardless of name changes.
# The PERMNO identifies a firm's security through all its history, and companies may have several stocks at one time.
# In short: A PERMCO can have multiple PERMNOs
rm(list=ls())
finaldata<- as.data.table(read.csv("final_data.csv"))
setorder(finaldata, Year, Month)
finaldata_dec <- finaldata[Month==12,]

finaldata_dec[, T5yrAvgCF := rollapply(CF_NET, 5, 
                                     function(x){ mean(x, na.rm = T)}, fill=NA, 
                                     align="right", partial = T), by = c("PERMCO")]

finaldata_dec[, T5yrAvgRev := rollapply(revt, 5, 
                                     function(x){ mean(x, na.rm = T)}, fill=NA, 
                                     align="right", partial = T), by = c("PERMCO")]

finaldata_dec[, T5yrAvgSale := rollapply(sale, 5, 
                                        function(x){ mean(x, na.rm = T)}, fill=NA, 
                                        align="right", partial = T), by = c("PERMCO")]

finaldata_dec[, T5yrAvgDvt := rollapply(dvt, 5, 
                                         function(x){ mean(x, na.rm = T)}, fill=NA, 
                                         align="right", partial = T), by = c("PERMCO")]
#####################################################################################


################################ sorting the 1000 largest stocks based on each metrics ####################################
finaldata_dec[, BE_index := 1]
finaldata_dec[, T5yrAvgCF_index := 1]
finaldata_dec[, T5yrAvgRev_index := 1]
finaldata_dec[, T5yrAvgSale_index := 1]
finaldata_dec[, T5yrAvgDvt_index := 1]
finaldata_dec[, emp_index := 1]

# sort BE and label it from largest to smallest (1 to 1000)
setorderv(finaldata_dec, cols = c("Year", "Month", "BE"), order =-1, na.last = T)
finaldata_dec[, BE_index := cumsum(BE_index), .(Year, Month)]

# sort trailing 5 year average Cash flow and label it from largest to smallest (1 to 1000)
setorderv(finaldata_dec, cols = c("Year", "Month", "T5yrAvgCF"), order =-1, na.last = T)
finaldata_dec[, T5yrAvgCF_index := cumsum(T5yrAvgCF_index), .(Year, Month)]

# sort revenue and label it from largest to smallest 
setorderv(finaldata_dec, cols = c("Year",  "Month", "T5yrAvgRev"), order = -1, na.last = T)
finaldata_dec[, T5yrAvgRev_index := cumsum(T5yrAvgRev_index), .(Year, Month)]

# sort revenue and label it from largest to smallest
setorderv(finaldata_dec, cols = c("Year","Month","T5yrAvgSale"), order = -1, na.last = T)
finaldata_dec[, T5yrAvgSale_index := cumsum(T5yrAvgSale_index), .(Year, Month)]

# sort sales and label it from largest to smallest
setorderv(finaldata_dec, cols = c("Year","Month","T5yrAvgDvt"), order = -1, na.last = T)
finaldata_dec[, T5yrAvgDvt_index := cumsum(T5yrAvgDvt_index), .(Year, Month)]

# sort employment and label it from largest to smallest 
setorderv(finaldata_dec, cols = c("Year","Month","emp"), order = -1, na.last = T)
finaldata_dec[, emp_index := cumsum(emp_index), .(Year, Month)]
#####################################################################################

################################## find the fundamental weights ###################################
# select top 1000 stock and make sure the BE value is higher than 0
setorder(finaldata, Year, Month)
setorder(finaldata_dec, Year)
finaldata_TTMs <- finaldata_dec[,.(PERMCO, Year, BE_index, T5yrAvgCF_index, T5yrAvgRev_index, 
                                  T5yrAvgSale_index, T5yrAvgDvt_index, T5yrAvgCF, T5yrAvgRev, 
                                  T5yrAvgSale, T5yrAvgDvt, emp_index)]
merged_fundamentals <- merge(finaldata, finaldata_TTMs, by = c("PERMCO","Year"), all.x = T, allow.cartesian = T)
merged_fundamentals <- merged_fundamentals[Year>1961,]
  
bvTop1000 <- merged_fundamentals[ BE_index <= 1000 & BE > 0]
bvTop1000[, c("BE_rank","lagged_BE") := .(shift(BE_index, 12), shift(BE, 12)), .(PERMNO)] 

bvTop1000 <- bvTop1000[lagged_BE != 0]
bvTop1000 <- bvTop1000[!is.na(BE_rank) & !is.na(lagged_BE)]

bvTop1000[, BV_Weight := lagged_BE/sum(lagged_BE), .(Year, Month)]
#bvTop1000_portfolio <- bvTop1000[,.(BV_Ret = weighted.mean(Ret, lagged_BE, na.rm = TRUE)), .(Year, Month)]
bvTop1000_portfolio <- bvTop1000[,.(BV_Ret = sum(BV_Weight * Ret, na.rm = T)), .(Year, Month)] 

setkey(bvTop1000_portfolio, Year, Month)

prod(bvTop1000_portfolio$BV_Ret + 1) ^ (1/(length(bvTop1000_portfolio$BV_Ret)/12)) -1
sd(bvTop1000_portfolio$BV_Ret) * sqrt(12)

#####
# 2 income
cfTop1000 <- merged_fundamentals[T5yrAvgCF_index <= 1000 & CF_NET >0]
cfTop1000[, c("CF_rank","lagged_T5yrAvgCF") := .(shift(T5yrAvgCF_index,12), shift(T5yrAvgCF,12)), .(PERMNO)] 

cfTop1000 <- cfTop1000[lagged_T5yrAvgCF != 0]
cfTop1000 <- cfTop1000[!is.na(CF_rank) & !is.na(lagged_T5yrAvgCF)]

cfTop1000[, CF_Weight := lagged_T5yrAvgCF/sum(lagged_T5yrAvgCF), .(Year, Month)]
#cfTop1000_portfolio <- cfTop1000[,.(CF_Ret = weighted.mean(Ret, lagged_T5yrAvgCF, na.rm = TRUE)), .(Year, Month)]
cfTop1000_portfolio <- cfTop1000[,.(CF_Ret = sum(CF_Weight * Ret, na.rm = T)), .(Year, Month)] 

setkey(cfTop1000_portfolio, Year, Month)

prod(cfTop1000_portfolio$CF_Ret + 1) ^ (1/(length(cfTop1000_portfolio$CF_Ret)/12)) -1
sd(cfTop1000_portfolio$CF_Ret) * sqrt(12)

#####
# 3 Revenue
RevTop1000 <- merged_fundamentals[ T5yrAvgRev_index <= 1000 ]
RevTop1000[, c("Rev_rank","lagged_T5yrAvgRev") := .(shift(T5yrAvgRev_index,12), shift(T5yrAvgRev, 12)), .(PERMNO)]

RevTop1000 <- RevTop1000[lagged_T5yrAvgRev != 0]
RevTop1000 <- RevTop1000[!is.na(Rev_rank) & !is.na(lagged_T5yrAvgRev)]

RevTop1000[, Rev_Weight := lagged_T5yrAvgRev/sum(lagged_T5yrAvgRev), .(Year, Month)]
#RevTop1000_portfolio <- RevTop1000[,.(Rev_Ret = weighted.mean(Ret, lagged_T5yrAvgRev, na.rm = TRUE)), .(Year, Month)]
RevTop1000_portfolio <- RevTop1000[,.(Rev_Ret = sum(Rev_Weight * Ret, na.rm = T)), .(Year, Month)]

setkey(RevTop1000_portfolio, Year, Month)

prod(RevTop1000_portfolio$Rev_Ret + 1) ^ (1/(length(RevTop1000_portfolio$Rev_Ret)/12)) -1
sd(RevTop1000_portfolio$Rev_Ret) * sqrt(12)
####

# 4 Dividend
DvtTop1000 <- merged_fundamentals[T5yrAvgDvt_index <= 1000 ]
DvtTop1000[, c("Dvt_rank","lagged_T5yrAvgDvt") := .(shift(T5yrAvgDvt_index,12), shift(T5yrAvgDvt,12)), .(PERMNO)]

DvtTop1000 <- DvtTop1000[lagged_T5yrAvgDvt != 0]
DvtTop1000 <- DvtTop1000[!is.na(Dvt_rank) & !is.na(lagged_T5yrAvgDvt)]

DvtTop1000[, Dvt_Weight := lagged_T5yrAvgDvt/sum(lagged_T5yrAvgDvt), .(Year, Month)]
#DvtTop1000_portfolio <- DvtTop1000[,.(Dvt_Ret = weighted.mean(Ret, lagged_T5yrAvgDvt, na.rm = TRUE)), .(Year, Month)]
DvtTop1000_portfolio <- DvtTop1000[,.(Dvt_Ret = sum(Dvt_Weight * Ret, na.rm = T)), .(Year, Month)]

setkey(DvtTop1000_portfolio, Year, Month)

prod(DvtTop1000_portfolio$Dvt_Ret + 1) ^ (1/(length(DvtTop1000_portfolio$Dvt_Ret)/12)) -1
sd(DvtTop1000_portfolio$Dvt_Ret) * sqrt(12)
####

# 5 sale
SaleTop1000 <- merged_fundamentals[ T5yrAvgSale_index <= 1000 ]
SaleTop1000[, c("Sale_rank","lagged_T5yrAvgSale") := .(shift(T5yrAvgSale_index, 12), shift(T5yrAvgSale, 12)), .(PERMNO)]

SaleTop1000 <- SaleTop1000[lagged_T5yrAvgSale != 0]
SaleTop1000 <- SaleTop1000[!is.na(Sale_rank) & !is.na(lagged_T5yrAvgSale)]
SaleTop1000_portfolio <- SaleTop1000[,.(Sale_Ret = weighted.mean(Ret, lagged_T5yrAvgSale, na.rm = TRUE)), .(Year, Month)]

setkey(SaleTop1000_portfolio, Year, Month)

prod(SaleTop1000_portfolio$Sale_Ret + 1) ^ (1/(length(SaleTop1000_portfolio$Sale_Ret)/12)) -1
sd(SaleTop1000_portfolio$Sale_Ret) * sqrt(12)
####

# 6 Employment
EmpTop1000 <- merged_fundamentals[ emp_index <= 1000 ]
EmpTop1000[, c("Emp_rank","lagged_emp") := .(shift(emp_index, 12), shift(emp, 12)), .(PERMNO)]

EmpTop1000 <- EmpTop1000[lagged_emp != 0]
EmpTop1000 <- EmpTop1000[!is.na(Emp_rank) & !is.na(lagged_emp)]
EmpTop1000_portfolio <- EmpTop1000[,.(Emp_Ret = weighted.mean(Ret, lagged_emp, na.rm = TRUE)), .(Year, Month)]

setkey(EmpTop1000_portfolio, Year, Month)

prod(EmpTop1000_portfolio$Emp_Ret + 1) ^ (1/(length(EmpTop1000_portfolio$Emp_Ret)/12)) -1
sd(EmpTop1000_portfolio$Emp_Ret) * sqrt(12)
####

# 2 Composite index, do it tmr
bvTop1000_premerge <- bvTop1000[,.(PERMCO, Year, Month, Ret, BV_Weight)]
cfTop1000_premerge <- cfTop1000[,.(PERMCO, Year, Month, Ret, CF_Weight)]
DvtTop1000_premerge <- DvtTop1000[,.(PERMCO, Year, Month, Ret, Dvt_Weight)]
RevTop1000_premerge <- RevTop1000[,.(PERMCO, Year, Month, Ret, Rev_Weight)]

setorder(bvTop1000_premerge, Year, Month)
setorder(cfTop1000_premerge, Year, Month)
setorder(DvtTop1000_premerge, Year, Month)
setorder(RevTop1000_premerge, Year, Month)

bv_cf <- merge(bvTop1000_premerge, cfTop1000_premerge, by = c("PERMCO", "Year", "Month"),  allow.cartesian = T)
bv_cf_Dvt <- merge(bv_cf, DvtTop1000_premerge, by = c("PERMCO", "Year", "Month"),  allow.cartesian = T)
bv_cf_Dvt_rev <-  merge(bv_cf_Dvt, RevTop1000_premerge, by = c("PERMCO", "Year", "Month","Ret"),  allow.cartesian = T)
setorder(bv_cf_Dvt_rev, Year, Month)
bv_cf_Dvt_rev[,composite_weight := mean(CF_Weight + Dvt_Weight + Rev_Weight + BV_Weight)]
composite_portfolio <- bv_cf_Dvt_rev[,.(Composite_Ret = weighted.mean(Ret, composite_weight, na.rm = TRUE)), .(Year, Month)]

prod(composite_portfolio$Composite_Ret + 1) ^ (1/(length(composite_portfolio$Composite_Ret)/12)) -1

#####


# cap weighted after sorting example , the return is not necessarily higher, in fact its about the same
# # 3 Revenue 
# RevTop1000 <- merged_fundamentals[ T5yrAvgRev_index <= 1000 ]
# RevTop1000[, c("Rev_rank","lagged_Mktcap") := .(shift(T5yrAvgRev_index,1), shift(Mkt_cap,1)), .(PERMNO)] 
# 
# RevTop1000 <- RevTop1000[lagged_Mktcap != 0]
# RevTop1000 <- RevTop1000[!is.na(Rev_rank) & !is.na(lagged_Mktcap)]
# RevTop1000_portfolio <- RevTop1000[,.(Rev_Ret = weighted.mean(Ret, lagged_Mktcap, na.rm = TRUE)), .(Year, Month)]
# 
# setkey(RevTop1000_portfolio, Year, Month)
# 
# prod(RevTop1000_portfolio$Rev_Ret + 1) ^ (1/(length(RevTop1000_portfolio$Rev_Ret)/12)) -1
# sd(RevTop1000_portfolio$Rev_Ret) * sqrt(12)
# ####
# 
# # 4 Dividend
# DvtTop1000 <- merged_fundamentals[ T5yrAvgDvt_index <= 1000 ]
# DvtTop1000[, c("Dvt_rank","lagged_Mktcap") := .(shift(T5yrAvgDvt_index,1), shift(Mkt_cap,1)), .(PERMNO)] 
# 
# DvtTop1000 <- DvtTop1000[lagged_Mktcap != 0]
# DvtTop1000 <- DvtTop1000[!is.na(Dvt_rank) & !is.na(lagged_Mktcap)]
# DvtTop1000_portfolio <- DvtTop1000[,.(Dvt_Ret = weighted.mean(Ret, lagged_Mktcap, na.rm = TRUE)), .(Year, Month)]
# 
# setkey(DvtTop1000_portfolio, Year, Month)
# 
# prod(DvtTop1000_portfolio$Dvt_Ret + 1) ^ (1/(length(DvtTop1000_portfolio$Dvt_Ret)/12)) -1
# sd(DvtTop1000_portfolio$Dvt_Ret) * sqrt(12)
# ####
# 
# # 5 sale
# SaleTop1000 <- merged_fundamentals[ T5yrAvgSale_index <= 1000 ]
# SaleTop1000[, c("Sale_rank","lagged_Mktcap") := .(shift(T5yrAvgSale_index,1), shift(Mkt_cap,1)), .(PERMNO)] 
# 
# SaleTop1000 <- SaleTop1000[lagged_Mktcap != 0]
# SaleTop1000 <- SaleTop1000[!is.na(Sale_rank) & !is.na(lagged_Mktcap)]
# SaleTop1000_portfolio <- SaleTop1000[,.(Sale_Ret = weighted.mean(Ret, lagged_Mktcap, na.rm = TRUE)), .(Year, Month)]
# 
# setkey(SaleTop1000_portfolio, Year, Month)
# 
# prod(SaleTop1000_portfolio$Sale_Ret + 1) ^ (1/(length(SaleTop1000_portfolio$Sale_Ret)/12)) -1
# sd(SaleTop1000_portfolio$Sale_Ret) * sqrt(12)
# ####
# 
# # 6 Employment 
# EmpTop1000 <- merged_fundamentals[ emp_index <= 1000 ]
# EmpTop1000[, c("Emp_rank","lagged_Mktcap") := .(shift(emp_index,1), shift(Mkt_cap,1)), .(PERMNO)] 
# 
# EmpTop1000 <- EmpTop1000[lagged_Mktcap != 0]
# EmpTop1000 <- EmpTop1000[!is.na(Emp_rank) & !is.na(lagged_Mktcap)]
# EmpTop1000_portfolio <- EmpTop1000[,.(Emp_Ret = weighted.mean(Ret, lagged_Mktcap, na.rm = TRUE)), .(Year, Month)]
# 
# setkey(EmpTop1000_portfolio, Year, Month)
# 
# prod(EmpTop1000_portfolio$Emp_Ret + 1) ^ (1/(length(EmpTop1000_portfolio$Emp_Ret)/12)) -1
# sd(EmpTop1000_portfolio$Emp_Ret) * sqrt(12)
# ####
# 
# 




#####################################################################################

