suppressMessages(require(data.table))
suppressMessages(require(lubridate))

rm(list=ls())

# Qn 1 

########################## Cleaning up CRSP data ########################## 
# load crsp data from 1972 Dec to 2018 Dec
crsp_monthly<- as.data.table(read.csv("crsp_monthly.csv"))

# convert date to date datatype
crsp_monthly[, date:= ymd(date)]

setkey(crsp_monthly, PERMCO)

# Filter by sharecode 10 and 11  and Filter by EXCHCD 1 2 3
crsp_monthly <- crsp_monthly[SHRCD %in% c(10,11)]
crsp_monthly <- crsp_monthly[EXCHCD %in% c(1,2,3)]

# set the year and month as integer
crsp_monthly[,Year:= year(date)]
crsp_monthly[,Month:= month(date)]

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

# Market Cap 
crsp_monthly[, Mkt_cap := abs(PRC) * SHROUT]
setorder(crsp_monthly, PERMNO, date)
crsp_monthly[, lag_Mkt_Cap := shift(Mkt_cap), by = PERMNO]

# TODO: Understand whether to filter by the finance industry at this point
# identify non-financial firms 
# The North American Industry Classification System (NAICS) is replacing the U.S. 
# Standard Industrial Classification (SIC) system. 
# NAICS was developed jointly by the U.S., 
# Canada, and Mexico to provide new comparability in statistics about business activity 
# across North America. 
# need to consider both SICCD and NAICS as NAICS was not available in the early days.

write.table(crsp_monthly, file = "crsp_monthly.csv", row.names=FALSE, sep=",")
######################################################################################## 

##################################### Cleaning up compustat data #####################################
# load computat data from 1972 Dec to 2018 Dec
# compustat_annual$indfmt has FS and INDL , remove FS in this case.
compustat_annual<- as.data.table(read.csv("compustat.csv"))
compustat_PRBA<- as.data.table(read.csv("PRBA.csv"))
linktable <- as.data.table(read.csv("linktable.csv"))

# convert date to date datatype
compustat_annual[, date:= ymd(datadate)]
compustat_PRBA[, date:= ymd(datadate)]

# separate Financial service firms with non financial service firms
# FS_firms <- compustat_annual[indfmt != "INDL"]
# compustat_annual_indl_only <- compustat_annual[indfmt == "INDL"]

# merge compustat with PRBA
compustat_PRBA <- compustat_PRBA[,.(gvkey, prba, date)]
compustat_merged <- merge(compustat_annual, compustat_PRBA, by.x = c("gvkey","date"), by.y = c("gvkey","date"), all.x = T)
write.table(compustat_merged, file = "compustat_merged.csv", row.names=FALSE, sep=",")
####################################################################

########################### Calculate fundamental factors on compustat data ###########################
# calculates the shareholders equity (SHE)
compustat_merged[, `:=`(SHE, ifelse(!is.na(seq), seq, 
                                    ifelse(!is.na(ceq + pstk), (ceq + pstk), 
                                           ifelse(!is.na(at-lt-mib), at-lt-mib, at-lt))))]

# convert NAs in itcb to 0 
compustat_merged[, `:=`(itcb, ifelse(!is.na(itcb), itcb, 0))]

# convert NAs in txditc to 0 
compustat_merged[, `:=`(txdb, ifelse(!is.na(txdb), txdb, 0))]

# calculates the deferred taxes and investment tax credit (DT)
compustat_merged[, `:=`(DT, ifelse(!is.na(txditc), txditc, itcb + txdb))]

# calculates book value of preferred stock (PS)
compustat_merged[, `:=`(PS, ifelse(!is.na(pstkrv), pstkrv, 
                                   ifelse(!is.na(pstkl), pstkl, pstk)))]

# convert NAs in PS to 0 
compustat_merged[, `:=`(PS, ifelse(!is.na(PS), PS, 0))]

# convert NAs in DT to 0 
compustat_merged[, `:=`(DT, ifelse(!is.na(DT), DT, 0))]

# convert NAs in PRBA to 0 
compustat_merged[, `:=`(prba, ifelse(!is.na(prba), prba, 0))]

# calculates book equity
compustat_merged[, `:=`(BE, ifelse(!is.na(SHE), SHE - PS + DT - prba, NA))]

####################################################################



