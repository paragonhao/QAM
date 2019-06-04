suppressMessages(require(data.table))
suppressMessages(require(lubridate))
suppressWarnings(require(zoo))
suppressWarnings(require(dplyr))
suppressWarnings(require(moments))
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
                                      oancf + ivncf, ib + dp)]

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
setorder(finaldata, PERMCO, Year, Month)
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

setorder(merged_fundamentals, PERMNO, Year, Month)

bvTop1000 <- merged_fundamentals[BE_index <= 1000 & BE > 0]
bvTop1000[, c("BE_rank","lagged_BE") := .(shift(BE_index, 12), shift(BE, 12)), .(PERMNO)] 

bvTop1000 <- bvTop1000[lagged_BE != 0]
bvTop1000 <- bvTop1000[!is.na(BE_rank) & !is.na(lagged_BE)]

bvTop1000[, BV_Weight := lagged_BE/sum(lagged_BE), .(Year, Month)]
#bvTop1000_portfolio <- bvTop1000[,.(BV_Ret = weighted.mean(Ret, lagged_BE, na.rm = TRUE)), .(Year, Month)]
bvTop1000_portfolio <- bvTop1000[,.(BV_Ret = sum(BV_Weight * Ret, na.rm = T)), .(Year, Month)] 
bvTop1000_portfolio <- bvTop1000_portfolio[Year >1962,]

#####
# 2 income
setorder(merged_fundamentals, PERMNO, Year, Month)

cfTop1000 <- merged_fundamentals[T5yrAvgCF_index <= 1000 & CF_NET >0]
cfTop1000[, c("CF_rank","lagged_T5yrAvgCF") := .(shift(T5yrAvgCF_index, 12), shift(T5yrAvgCF,12)), .(PERMNO)] 

cfTop1000 <- cfTop1000[lagged_T5yrAvgCF != 0]
cfTop1000 <- cfTop1000[!is.na(CF_rank) & !is.na(lagged_T5yrAvgCF)]

cfTop1000[, CF_Weight := lagged_T5yrAvgCF/sum(lagged_T5yrAvgCF), .(Year, Month)]
#cfTop1000_portfolio <- cfTop1000[,.(CF_Ret = weighted.mean(Ret, lagged_T5yrAvgCF, na.rm = TRUE)), .(Year, Month)]
cfTop1000_portfolio <- cfTop1000[,.(CF_Ret = sum(CF_Weight * Ret, na.rm = T)), .(Year, Month)] 
cfTop1000_portfolio <- cfTop1000_portfolio[Year >1962,]
#####
# 3 Revenue
RevTop1000 <- merged_fundamentals[ T5yrAvgRev_index <= 1000 ]
RevTop1000[, c("Rev_rank","lagged_T5yrAvgRev") := .(shift(T5yrAvgRev_index,4), shift(T5yrAvgRev, 4)), .(PERMNO)]

RevTop1000 <- RevTop1000[lagged_T5yrAvgRev != 0]
RevTop1000 <- RevTop1000[!is.na(Rev_rank) & !is.na(lagged_T5yrAvgRev)]

RevTop1000[, Rev_Weight := lagged_T5yrAvgRev/sum(lagged_T5yrAvgRev), .(Year, Month)]
#RevTop1000_portfolio <- RevTop1000[,.(Rev_Ret = weighted.mean(Ret, lagged_T5yrAvgRev, na.rm = TRUE)), .(Year, Month)]
RevTop1000_portfolio <- RevTop1000[,.(Rev_Ret = sum(Rev_Weight * Ret, na.rm = T)), .(Year, Month)]
RevTop1000_portfolio <- RevTop1000_portfolio[Year >1962,]
####

# 4 Dividend
DvtTop1000 <- merged_fundamentals[T5yrAvgDvt_index <= 1000 ]
DvtTop1000[, c("Dvt_rank","lagged_T5yrAvgDvt") := .(shift(T5yrAvgDvt_index,4), shift(T5yrAvgDvt,4)), .(PERMNO)]

DvtTop1000 <- DvtTop1000[lagged_T5yrAvgDvt != 0]
DvtTop1000 <- DvtTop1000[!is.na(Dvt_rank) & !is.na(lagged_T5yrAvgDvt)]

DvtTop1000[, Dvt_Weight := lagged_T5yrAvgDvt/sum(lagged_T5yrAvgDvt), .(Year, Month)]
#DvtTop1000_portfolio <- DvtTop1000[,.(Dvt_Ret = weighted.mean(Ret, lagged_T5yrAvgDvt, na.rm = TRUE)), .(Year, Month)]
DvtTop1000_portfolio <- DvtTop1000[,.(Dvt_Ret = sum(Dvt_Weight * Ret, na.rm = T)), .(Year, Month)]
DvtTop1000_portfolio <- DvtTop1000_portfolio[Year >1962,]
####

# 5 sale
SaleTop1000 <- merged_fundamentals[ T5yrAvgSale_index <= 1000 ]
SaleTop1000[, c("Sale_rank","lagged_T5yrAvgSale") := .(shift(T5yrAvgSale_index, 4), shift(T5yrAvgSale, 4)), .(PERMNO)]

SaleTop1000 <- SaleTop1000[lagged_T5yrAvgSale != 0]
SaleTop1000 <- SaleTop1000[!is.na(Sale_rank) & !is.na(lagged_T5yrAvgSale)]
SaleTop1000_portfolio <- SaleTop1000[,.(Sale_Ret = weighted.mean(Ret, lagged_T5yrAvgSale, na.rm = TRUE)), .(Year, Month)]
SaleTop1000_portfolio <- SaleTop1000_portfolio[Year >1962,]

####

# 6 Employment
EmpTop1000 <- merged_fundamentals[ emp_index <= 1000 ]
EmpTop1000[, c("Emp_rank","lagged_emp") := .(shift(emp_index, 4), shift(emp, 4)), .(PERMNO)]

EmpTop1000 <- EmpTop1000[lagged_emp != 0]
EmpTop1000 <- EmpTop1000[!is.na(Emp_rank) & !is.na(lagged_emp)]
EmpTop1000_portfolio <- EmpTop1000[,.(Emp_Ret = weighted.mean(Ret, lagged_emp, na.rm = TRUE)), .(Year, Month)]
EmpTop1000_portfolio <- EmpTop1000_portfolio[Year >1962,]
####

# 2 Composite index
bvTop1000_premerge <- bvTop1000[,.(PERMCO, Year, Month, Ret, BV_Weight)]
cfTop1000_premerge <- cfTop1000[,.(PERMCO, Year, Month, Ret, CF_Weight)]
DvtTop1000_premerge <- DvtTop1000[,.(PERMCO, Year, Month, Ret, Dvt_Weight)]
RevTop1000_premerge <- RevTop1000[,.(PERMCO, Year, Month, Ret, Rev_Weight)]

setorder(bvTop1000_premerge, Year, Month)
setorder(cfTop1000_premerge, Year, Month)
setorder(DvtTop1000_premerge, Year, Month)
setorder(RevTop1000_premerge, Year, Month)

bv_cf <- merge(bvTop1000_premerge, cfTop1000_premerge, by = c("PERMCO", "Year", "Month"), all=T, allow.cartesian = T)
bv_cf_Dvt <- merge(bv_cf, DvtTop1000_premerge, by = c("PERMCO", "Year", "Month"),  all=T,  allow.cartesian = T)
bv_cf_Dvt_rev <-  merge(bv_cf_Dvt, RevTop1000_premerge,  all=T, by = c("PERMCO", "Year", "Month","Ret"),  allow.cartesian = T)

setorder(bv_cf_Dvt_rev, Year, Month)
bv_cf_Dvt_rev[,composite_weight := mean(CF_Weight + Dvt_Weight + Rev_Weight + BV_Weight, na.rm = T)]
composite_portfolio <- bv_cf_Dvt_rev[,.(Composite_Ret = weighted.mean(Ret, composite_weight, na.rm = TRUE)), .(Year, Month)]
composite_portfolio <- composite_portfolio[Year > 1962,]

#####
# Reference 
# select the top 1000 stocks based on cap and value weight them to form the portfolio

# sort mkt cap and label it from largest to smallest (1 to 1000)
setorder(finaldata, Year, Month)
reference_dec <- finaldata[Month==12,]

reference_dec <- reference_dec[, mktCap_index := 1]

# sort Mkt cap and label it from largest to smallest (1 to 1000)
setorderv(reference_dec, cols = c("Year", "Mkt_cap"), order =-1, na.last = T)
reference_dec[, mktCap_index := cumsum(mktCap_index), .(Year, Month)]

setorder(finaldata, Year, Month)
setorder(reference_dec, Year)

reference_dec <- reference_dec[, .(PERMCO, Year, mktCap_index)]
reference_trim <- finaldata[, .(PERMNO, PERMCO, Ret,Year, Month, Mkt_cap)]

merged_reference <- merge(reference_trim, reference_dec, by = c("PERMCO","Year"), all.x = T, allow.cartesian = T)
merged_reference  <- merged_reference[Year>1961,]

setorder(merged_reference, PERMNO, Year, Month)

capTop1000 <- merged_reference[ mktCap_index <= 1000 & Mkt_cap > 0]
capTop1000[, c("mktCap_rank","lagged_MktCap") := .(shift(mktCap_index, 12), shift(Mkt_cap, 12)), .(PERMNO)]

capTop1000 <- capTop1000[lagged_MktCap != 0]
capTop1000 <- capTop1000[!is.na(mktCap_rank) & !is.na(lagged_MktCap)]

capTop1000_portfolio <- capTop1000[,.(vw_Ret = weighted.mean(Ret, lagged_MktCap, na.rm = TRUE)), .(Year, Month)]
capTop1000_portfolio <- capTop1000_portfolio[Year >1962]
#########
# S&P 500
vwretd <- as.data.table(read.csv("vwretd.csv"))
vwretd[, date := ymd(caldt)]
vwretd[, Year:= year(date)]
vwretd[, Month:= month(date)]
vwretd <- vwretd[Year >1962]


########################################### collecting data for the summary matrix
summary <- matrix(nrow=10, ncol=8,dimnames = list(c("SP500","Reference","Book","Income","Revenue",
                                                    "Sales","Dividends","Employment","Composite", "Average (ex Composite)"), 
                                                  c("Ending Value of 1","Geometric Return","Volatility",
                                                    "Sharpe Ratio","Excess Return vs. Reference", "t-statistic for Excess Return","Skewness","Excess Kurtosis")))

# sp 500
summary[1, 2] <- (prod(vwretd$vwretd + 1) ^ (1/(length(vwretd$vwretd)/12)) -1) * 100
summary[1, 3] <- sd(vwretd$vwretd ) * sqrt(12) * 100
summary[1, 7] <- skewness(vwretd$vwretd )
summary[1, 8] <- kurtosis(vwretd$vwretd ) - 3

# reference
summary[2, 2] <- (prod(capTop1000_portfolio$vw_Ret + 1) ^ (1/(length(capTop1000_portfolio$vw_Ret)/12)) -1) * 100
summary[2, 3] <- sd(capTop1000_portfolio$vw_Ret) * sqrt(12) *100
summary[2, 7] <- skewness(capTop1000_portfolio$vw_Ret)
summary[2, 8] <- kurtosis(capTop1000_portfolio$vw_Ret) - 3

# book 
summary[3, 2] <- (prod(bvTop1000_portfolio$BV_Ret + 1) ^ (1/(length(bvTop1000_portfolio$BV_Ret)/12)) -1) * 100
summary[3, 3] <- sd(bvTop1000_portfolio$BV_Ret) * sqrt(12) *100
summary[3, 7] <- skewness(bvTop1000_portfolio$BV_Ret)
summary[3, 8] <- kurtosis(bvTop1000_portfolio$BV_Ret) - 3


# cash flow/income
summary[4, 2] <- (prod(cfTop1000_portfolio$CF_Ret + 1) ^ (1/(length(cfTop1000_portfolio$CF_Ret)/12)) -1) * 100
summary[4, 3] <- sd(cfTop1000_portfolio$CF_Ret) * sqrt(12) *100
summary[4, 7] <- skewness(cfTop1000_portfolio$CF_Ret)
summary[4, 8] <- kurtosis(cfTop1000_portfolio$CF_Ret) - 3


# revenue
summary[5, 2] <- (prod(RevTop1000_portfolio$Rev_Ret + 1) ^ (1/(length(RevTop1000_portfolio$Rev_Ret)/12)) -1) * 100
summary[5, 3] <- sd(RevTop1000_portfolio$Rev_Ret) * sqrt(12) *100
summary[5, 7] <- skewness(RevTop1000_portfolio$Rev_Ret)
summary[5, 8] <- kurtosis(RevTop1000_portfolio$Rev_Ret) - 3


# Sales
summary[6, 2] <- (prod(SaleTop1000_portfolio$Sale_Ret + 1) ^ (1/(length(SaleTop1000_portfolio$Sale_Ret )/12)) -1) * 100
summary[6, 3] <- sd(SaleTop1000_portfolio$Sale_Ret) * sqrt(12) *100
summary[6, 7] <- skewness(SaleTop1000_portfolio$Sale_Ret)
summary[6, 8] <- kurtosis(SaleTop1000_portfolio$Sale_Ret) - 3


# dividends
summary[7, 2] <- (prod(DvtTop1000_portfolio$Dvt_Ret + 1) ^ (1/(length(DvtTop1000_portfolio$Dvt_Ret)/12)) -1) * 100
summary[7, 3] <- sd(DvtTop1000_portfolio$Dvt_Ret) * sqrt(12) *100
summary[7, 7] <- skewness(DvtTop1000_portfolio$Dvt_Ret)
summary[7, 8] <- kurtosis(DvtTop1000_portfolio$Dvt_Ret) - 3


# Employment
summary[8, 2] <- (prod(EmpTop1000_portfolio$Emp_Ret + 1) ^ (1/(length(EmpTop1000_portfolio$Emp_Ret)/12)) -1) * 100
summary[8, 3] <- sd(EmpTop1000_portfolio$Emp_Ret ) * sqrt(12) *100
summary[8, 7] <- skewness(EmpTop1000_portfolio$Emp_Ret )
summary[8, 8] <- kurtosis(EmpTop1000_portfolio$Emp_Ret ) - 3


# Composite
summary[9, 2] <- (prod(composite_portfolio$Composite_Ret + 1) ^ (1/(length(composite_portfolio$Composite_Ret)/12)) -1) * 100
summary[9, 3] <- sd(composite_portfolio$Composite_Ret) * sqrt(12) *100
summary[9, 7] <- skewness(composite_portfolio$Composite_Ret )
summary[9, 8] <- kurtosis(composite_portfolio$Composite_Ret ) - 3


# Average 
summary[10, 2] <- mean(summary[(3:8), 2])
summary[10, 3] <- mean(summary[(3:8), 3])
summary[10, 7] <- mean(summary[(3:8), 7])
summary[10, 8] <- mean(summary[(3:8), 8]) 


# import ff factors
FF_Factors <- as.data.table(read.csv("F-F_Research_Data_Factors.csv"))
FF_Factors[, date := ymd(X, truncated = 1)]
FF_Factors[, Year:= year(date)]
FF_Factors[, Month:= month(date)]
FF_Factors <- FF_Factors[Year >1962 & Year < 2018]
FF_Factors <- FF_Factors[, list(RF = RF/100, Year, Month)]

# get sharp ratio
setorder(FF_Factors, Year, Month)
setorder(vwretd, Year, Month)
setorder(capTop1000_portfolio, Year, Month)
setorder(bvTop1000_portfolio, Year, Month)
length_yr <- length(vwretd$vwretd)

ex1 <- vwretd$vwretd - FF_Factors$RF
summary[1, 4] <- (prod(ex1 + 1) ^ (1/(length_yr/12)) -1) * 100 / summary[1, 3]
summary[1, 1] <- prod(1 + vwretd$vwretd)

ex2 <- capTop1000_portfolio$vw_Ret - FF_Factors$RF
summary[2, 4]  <- (prod(ex2 + 1) ^ (1/(length_yr/12)) -1) * 100 / summary[2, 3]
summary[2, 1] <- prod(1 + capTop1000_portfolio$vw_Ret)

ex3 <- bvTop1000_portfolio$BV_Ret - FF_Factors$RF
summary[3, 4]  <- (prod(ex3 + 1) ^ (1/(length_yr/12)) -1) * 100 / summary[3, 3]
summary[3, 1] <- prod(1 + bvTop1000_portfolio$BV_Ret)

ex4 <- cfTop1000_portfolio$CF_Ret - FF_Factors$RF
summary[4, 4]  <-  (prod(ex4 + 1) ^ (1/(length_yr/12)) -1) * 100 / summary[4, 3]
summary[4, 1] <- prod(1 + cfTop1000_portfolio$CF_Ret)

ex5 <- RevTop1000_portfolio$Rev_Ret - FF_Factors$RF
summary[5, 4] <- (prod(ex5 + 1) ^ (1/(length_yr/12)) -1) * 100 / summary[5, 3]
summary[5, 1] <- prod(1 + RevTop1000_portfolio$Rev_Ret)

ex6 <- SaleTop1000_portfolio$Sale_Ret - FF_Factors$RF
summary[6, 4] <- (prod(ex6 + 1) ^ (1/(length_yr/12)) -1) * 100 / summary[6, 3]
summary[6, 1] <- prod(1 + SaleTop1000_portfolio$Sale_Ret)

ex7 <- DvtTop1000_portfolio$Dvt_Ret - FF_Factors$RF
summary[7, 4] <- (prod(ex7 + 1) ^ (1/(length_yr/12)) -1) * 100 / summary[7, 3]
summary[7, 1] <- prod(1 + DvtTop1000_portfolio$Dvt_Ret)

ex8 <- EmpTop1000_portfolio$Emp_Ret - FF_Factors$RF
summary[8, 4] <- (prod(ex8 + 1) ^ (1/(length_yr/12)) -1) * 100 / summary[8, 3]
summary[8, 1] <- prod(1 + EmpTop1000_portfolio$Emp_Ret)

ex9 <-composite_portfolio$Composite_Ret - FF_Factors$RF
summary[9, 4] <- (prod(ex9 + 1) ^ (1/(length_yr/12)) -1) * 100 / summary[9, 3]
summary[9, 1] <- prod(1 + composite_portfolio$Composite_Ret)

ex10 <- (bvTop1000_portfolio$BV_Ret + cfTop1000_portfolio$CF_Ret + RevTop1000_portfolio$Rev_Ret + DvtTop1000_portfolio$Dvt_Ret)/4 - FF_Factors$RF 
summary[10, 4] <-  (prod(ex10 + 1) ^ (1/(length_yr/12)) -1) * 100/ summary[10, 3]
summary[10, 1] <- mean(summary[(3:8), 1]) 
# Excess return vs reference
for(i in 1:10){
  summary[i, 5] <- summary[i,2] - summary[2,2]
}

# T-stats for excess return
n <- length(vwretd$vwretd)
for(i in 1:10){
  summary[i, 6] <-  sqrt(n) * summary[i,5]/ summary[i,3]
}

summary <- apply(summary, 2, round, digits = 2)
summary[,3] <- round(summary[,3], 1)

write.table(summary, file ="summary.csv", row.names=FALSE, sep=",")
write.table(composite_portfolio, file="composite_portfolio.csv", row.names=FALSE, sep=",")
write.table(capTop1000_portfolio, file="reference.csv", row.names=FALSE, sep=",")
write.table(vwretd, file="vwretd.csv", row.names = FALSE, sep=",")

#####################################################################################

FF_Factors <- as.data.table(read.csv("F-F_Research_Data_Factors.csv"))
FF_Factors[, date := ymd(X, truncated = 1)]
FF_Factors[, Year:= year(date)]
FF_Factors[, Month:= month(date)]
FF_Factors <- FF_Factors[Year >1962 & Year < 2018]
FF_Factors <- FF_Factors[, list(Mkt.RF = Mkt.RF/100 ,SMB = SMB/100, HML = HML/100, RF = RF/100, Year, Month)]

# regression on excess return 
compo_FF <- merge(composite_portfolio, FF_Factors, by = c("Year","Month"))
excompor_lm <- lm((Composite_Ret - RF) ~ Mkt.RF + SMB + HML, data= compo_FF)
summary(excompor_lm)

vw_FF <- merge(vwretd, FF_Factors, by = c("Year","Month"))
vw_FF[, vwretd := as.double(vwretd)]
exvw_lm <-lm((vwretd - RF) ~ Mkt.RF + SMB + HML, data = vw_FF)
summary(exvw_lm)


######################## find alpha and betas 

summary_table2 <- matrix(nrow=10, ncol=6,dimnames = list(c("SP500","Reference","Book","Income","Revenue",
                                                    "Sales","Dividends","Employment","Composite", "Average (ex Composite)"), 
                                                  c("Ending Value of 1","Geometric Return",
                                                    "CAPM Beta Vs Reference","Excess Return vs. Reference", "CAPM Alpha Vs Reference","t-statistic for CAPM Alpha")))

summary_table2[,1] <- summary[,1]
summary_table2[,2] <- summary[,2]
summary_table2[,4] <- summary[,5]

ex1_lm <- lm((vwretd$vwretd - FF_Factors$RF) ~ (capTop1000_portfolio$vw_Ret- FF_Factors$RF))
ex2_lm <- lm((capTop1000_portfolio$vw_Ret - FF_Factors$RF) ~ (capTop1000_portfolio$vw_Ret- FF_Factors$RF))
ex3_lm <- lm((capTop1000_portfolio$vw_Ret - FF_Factors$RF) ~ (capTop1000_portfolio$vw_Rett- FF_Factors$RF))

## go back to the average cmposite 

