suppressMessages(require(data.table))
suppressMessages(require(lubridate))
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
# #####################################################################################
# 
# ################################ sorting the 1000 largest stocks based on each metrics ####################################
# finaldata[, BE_index := 1]
# finaldata[, CF_NET_index := 1]
# finaldata[, dvt_index := 1]
# finaldata[, revt_index := 1]
# finaldata[, emp_index := 1]
# finaldata[, sale_index := 1]
# 
# # sort BE and label it from largest to smallest (1 to 1000)
# setorderv(finaldata, cols = c("Year", "Month", "BE"), order =-1, na.last = T)
# finaldata[, BE_index := cumsum(BE_index), .(Year, Month)]
# 
# # sort Cash flow and label it from largest to smallest (1 to 1000)
# setorderv(finaldata, cols = c("Year", "Month", "CF_NET"), order =-1, na.last = T)
# finaldata[, CF_NET_index := cumsum(CF_NET_index), .(Year, Month)]
# 
# # sort dividend and label it from largest to smallest 
# setorderv(finaldata, cols = c("Year",  "Month", "dvt"), order = -1, na.last = T)
# finaldata[, dvt_index := cumsum(dvt_index), .(Year, Month)]
# 
# # sort revenue and label it from largest to smallest
# setorderv(finaldata, cols = c("Year","Month","revt"), order = -1, na.last = T)
# finaldata[, revt_index := cumsum(revt_index), .(Year, Month)]
# 
# # sort employment and label it from largest to smallest 
# setorderv(finaldata, cols = c("Year","Month","emp"), order = -1, na.last = T)
# finaldata[, emp_index := cumsum(emp_index), .(Year, Month)]
# 
# # sort sales and label it from largest to smallest
# setorderv(finaldata, cols = c("Year","Month","sale"), order = -1, na.last = T)
# finaldata[, sale_index := cumsum(sale_index), .(Year, Month)]
# #####################################################################################
