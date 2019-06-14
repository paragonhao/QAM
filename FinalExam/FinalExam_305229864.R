suppressMessages(require(data.table))
suppressMessages(require(lubridate))
suppressMessages(require(zoo))
suppressWarnings(require(dplyr))
suppressWarnings(require(moments))
rm(list=ls())

# Qn 1 
########################## Replicate table 1 ########################## 
# load crsp data from 1967 Dec to 2018 Dec
q_factors <- as.data.table(read.csv("q-Factors_monthly_1967_to_2018.csv"))

momentum <- as.data.table(read.csv("F-F_Momentum_Factor.CSV"))

FF_3Factor <- as.data.table(read.csv("F-F_Research_Data_Factors.CSV"))

q_colnames <-colnames(q_factors)[-1][-1]

runQn1 <- function(){
# divide all the returns by 100 to decimal number
for(i in q_colnames){
  q_factors[,  `:=`(paste0(i), get(i)/100)]
}

FF_colnames <- colnames(FF_3Factor)[-1]
for(i in FF_colnames){
  FF_3Factor[,  `:=`(paste0(i), get(i)/100)]
}

# convert date to date datatype
FF_3Factor[, date:= ymd(X, truncated = 1)]
FF_3Factor[, X:=NULL]
FF_3Factor[, Year := year(date)]
FF_3Factor[, Month := month(date)]

# convert date to date datatype
momentum[, date:= ymd(X, truncated = 1)]
momentum[, X:=NULL]
momentum[, Year := year(date)]
momentum[, Month := month(date)]
momentum[, Mom := Mom/100]

# cut data to start from 1972
q_factors_1972 <- q_factors[Year > 1971,]
FF_3Factor_1972 <- FF_3Factor[Year >1971 & Year <2019,]
momentum_1972 <- momentum[Year >1971 & Year <2019,]

setkey(q_factors_1972, Year, Month)
setkey(FF_3Factor_1972, Year, Month)
setkey(momentum, Year, Month)

q1_cols <- c("0", "Mean","alpha",
             "beta(MKT)", "beta(SMB)", "beta(HML)", "beta(UMD)","R2")
Q1_result_mat <- matrix(nrow=18, ncol=8)
colnames(Q1_result_mat) <- q1_cols

# For Panel A, you should use the market excess return from Ken French's website.
# rm on market 
rm_mkt <- lm(q_factors_1972$ME ~ FF_3Factor_1972$Mkt.RF)
Q1_result_mat[1,1] <- "ME"
Q1_result_mat[1,2] <- mean(q_factors_1972$ME) * 100
Q1_result_mat[1,3] <- rm_mkt$coefficients[1] * 100
Q1_result_mat[1,4] <- rm_mkt$coefficients[2] 
Q1_result_mat[1,8] <- summary(rm_mkt)[[8]]
Q1_result_mat[2,2] <- (mean(q_factors_1972$ME)/sd(q_factors_1972$ME)) * sqrt(length(q_factors_1972$ME))
Q1_result_mat[2,3] <- summary(rm_mkt)[["coefficients"]][, "t value"][1]
Q1_result_mat[2,4] <- summary(rm_mkt)[["coefficients"]][, "t value"][2]

# rm on mkt and smb and hml 
rm_mkt_smb_hml <- summary(lm(q_factors_1972$ME ~ FF_3Factor_1972$Mkt.RF + FF_3Factor_1972$SMB + FF_3Factor_1972$HML))
Q1_result_mat[3,3] <- rm_mkt_smb_hml[["coefficients"]][1] * 100
Q1_result_mat[3,4] <- rm_mkt_smb_hml[["coefficients"]][2]
Q1_result_mat[3,5] <- rm_mkt_smb_hml[["coefficients"]][3]
Q1_result_mat[3,6] <- rm_mkt_smb_hml[["coefficients"]][4]
Q1_result_mat[3,8] <- rm_mkt_smb_hml[[8]]
Q1_result_mat[4,3] <- rm_mkt_smb_hml[["coefficients"]][, "t value"][1]
Q1_result_mat[4,4] <- rm_mkt_smb_hml[["coefficients"]][, "t value"][2]
Q1_result_mat[4,5] <- rm_mkt_smb_hml[["coefficients"]][, "t value"][3]
Q1_result_mat[4,6] <- rm_mkt_smb_hml[["coefficients"]][, "t value"][4]


# rm on mkt  smb hml and umd
rm_mkt_smb_hml_umd <- summary(lm(q_factors_1972$ME ~ FF_3Factor_1972$Mkt.RF + FF_3Factor_1972$SMB + FF_3Factor_1972$HML + momentum_1972$Mom))
Q1_result_mat[5,3] <- rm_mkt_smb_hml_umd[["coefficients"]][1] * 100
Q1_result_mat[5,4] <- rm_mkt_smb_hml_umd[["coefficients"]][2]
Q1_result_mat[5,5] <- rm_mkt_smb_hml_umd[["coefficients"]][3]
Q1_result_mat[5,6] <- rm_mkt_smb_hml_umd[["coefficients"]][4]
Q1_result_mat[5,7] <- rm_mkt_smb_hml_umd[["coefficients"]][5]
Q1_result_mat[5,8] <- rm_mkt_smb_hml_umd[[8]]
Q1_result_mat[6,3] <- rm_mkt_smb_hml_umd[["coefficients"]][, "t value"][1]
Q1_result_mat[6,4] <- rm_mkt_smb_hml_umd[["coefficients"]][, "t value"][2]
Q1_result_mat[6,5] <- rm_mkt_smb_hml_umd[["coefficients"]][, "t value"][3]
Q1_result_mat[6,6] <- rm_mkt_smb_hml_umd[["coefficients"]][, "t value"][4]
Q1_result_mat[6,7] <- rm_mkt_smb_hml_umd[["coefficients"]][, "t value"][5]


# IA 
# ia on market 
ia_mkt <- lm(q_factors_1972$IA ~ FF_3Factor_1972$Mkt.RF)
Q1_result_mat[7,1] <- "IA"
Q1_result_mat[7,2] <- mean(q_factors_1972$IA) * 100
Q1_result_mat[7,3] <- ia_mkt$coefficients[1] * 100
Q1_result_mat[7,4] <- ia_mkt$coefficients[2] 
Q1_result_mat[7,8] <- summary(ia_mkt)[[8]]
Q1_result_mat[8,2] <- (mean(q_factors_1972$IA)/sd(q_factors_1972$IA)) * sqrt(length(q_factors_1972$IA))
Q1_result_mat[8,3] <- summary(ia_mkt)[["coefficients"]][, "t value"][1]
Q1_result_mat[8,4] <- summary(ia_mkt)[["coefficients"]][, "t value"][2]

# ia on mkt and smb and hml 
ia_mkt_smb_hml <- summary(lm(q_factors_1972$IA ~ FF_3Factor_1972$Mkt.RF + FF_3Factor_1972$SMB + FF_3Factor_1972$HML))
Q1_result_mat[9,3] <- ia_mkt_smb_hml[["coefficients"]][1] * 100
Q1_result_mat[9,4] <- ia_mkt_smb_hml[["coefficients"]][2]
Q1_result_mat[9,5] <- ia_mkt_smb_hml[["coefficients"]][3]
Q1_result_mat[9,6] <- ia_mkt_smb_hml[["coefficients"]][4]
Q1_result_mat[9,8] <- ia_mkt_smb_hml[[8]]
Q1_result_mat[10,3] <- ia_mkt_smb_hml[["coefficients"]][, "t value"][1]
Q1_result_mat[10,4] <- ia_mkt_smb_hml[["coefficients"]][, "t value"][2]
Q1_result_mat[10,5] <- ia_mkt_smb_hml[["coefficients"]][, "t value"][3]
Q1_result_mat[10,6] <- ia_mkt_smb_hml[["coefficients"]][, "t value"][4]


# ia on mkt  smb hml and umd
ia_mkt_smb_hml_umd <- summary(lm(q_factors_1972$IA ~ FF_3Factor_1972$Mkt.RF + FF_3Factor_1972$SMB + FF_3Factor_1972$HML + momentum_1972$Mom))
Q1_result_mat[11,3] <- ia_mkt_smb_hml_umd[["coefficients"]][1] * 100
Q1_result_mat[11,4] <- ia_mkt_smb_hml_umd[["coefficients"]][2]
Q1_result_mat[11,5] <- ia_mkt_smb_hml_umd[["coefficients"]][3]
Q1_result_mat[11,6] <- ia_mkt_smb_hml_umd[["coefficients"]][4]
Q1_result_mat[11,7] <- ia_mkt_smb_hml_umd[["coefficients"]][5]
Q1_result_mat[11,8] <- ia_mkt_smb_hml_umd[[8]]
Q1_result_mat[12,3] <- ia_mkt_smb_hml_umd[["coefficients"]][, "t value"][1]
Q1_result_mat[12,4] <- ia_mkt_smb_hml_umd[["coefficients"]][, "t value"][2]
Q1_result_mat[12,5] <- ia_mkt_smb_hml_umd[["coefficients"]][, "t value"][3]
Q1_result_mat[12,6] <- ia_mkt_smb_hml_umd[["coefficients"]][, "t value"][4]
Q1_result_mat[12,7] <- ia_mkt_smb_hml_umd[["coefficients"]][, "t value"][5]

# ROE 
# roe on market 
roe_mkt <- lm(q_factors_1972$ROE ~ FF_3Factor_1972$Mkt.RF)
Q1_result_mat[13,1] <- "ROE"
Q1_result_mat[13,2] <- mean(q_factors_1972$ROE) * 100
Q1_result_mat[13,3] <- roe_mkt$coefficients[1] * 100
Q1_result_mat[13,4] <- roe_mkt$coefficients[2] 
Q1_result_mat[13,8] <- summary(roe_mkt)[[8]]
Q1_result_mat[14,2] <- (mean(q_factors_1972$ROE)/sd(q_factors_1972$ROE)) * sqrt(length(q_factors_1972$ROE))
Q1_result_mat[14,3] <- summary(roe_mkt)[["coefficients"]][, "t value"][1]
Q1_result_mat[14,4] <- summary(roe_mkt)[["coefficients"]][, "t value"][2]

# roe on mkt and smb and hml 
roe_mkt_smb_hml <- summary(lm(q_factors_1972$ROE ~ FF_3Factor_1972$Mkt.RF + FF_3Factor_1972$SMB + FF_3Factor_1972$HML))
Q1_result_mat[15,3] <- roe_mkt_smb_hml[["coefficients"]][1] * 100
Q1_result_mat[15,4] <- roe_mkt_smb_hml[["coefficients"]][2]
Q1_result_mat[15,5] <- roe_mkt_smb_hml[["coefficients"]][3]
Q1_result_mat[15,6] <- roe_mkt_smb_hml[["coefficients"]][4]
Q1_result_mat[15,8] <- roe_mkt_smb_hml[[8]]
Q1_result_mat[16,3] <- roe_mkt_smb_hml[["coefficients"]][, "t value"][1]
Q1_result_mat[16,4] <- roe_mkt_smb_hml[["coefficients"]][, "t value"][2]
Q1_result_mat[16,5] <- roe_mkt_smb_hml[["coefficients"]][, "t value"][3]
Q1_result_mat[16,6] <- roe_mkt_smb_hml[["coefficients"]][, "t value"][4]


# roe on mkt  smb hml and umd
roe_mkt_smb_hml_umd <- summary(lm(q_factors_1972$ROE ~ FF_3Factor_1972$Mkt.RF + FF_3Factor_1972$SMB + FF_3Factor_1972$HML + momentum_1972$Mom))
Q1_result_mat[17,3] <- roe_mkt_smb_hml_umd[["coefficients"]][1] * 100
Q1_result_mat[17,4] <- roe_mkt_smb_hml_umd[["coefficients"]][2]
Q1_result_mat[17,5] <- roe_mkt_smb_hml_umd[["coefficients"]][3]
Q1_result_mat[17,6] <- roe_mkt_smb_hml_umd[["coefficients"]][4]
Q1_result_mat[17,7] <- roe_mkt_smb_hml_umd[["coefficients"]][5]
Q1_result_mat[17,8] <- roe_mkt_smb_hml_umd[[8]]
Q1_result_mat[18,3] <- roe_mkt_smb_hml_umd[["coefficients"]][, "t value"][1]
Q1_result_mat[18,4] <- roe_mkt_smb_hml_umd[["coefficients"]][, "t value"][2]
Q1_result_mat[18,5] <- roe_mkt_smb_hml_umd[["coefficients"]][, "t value"][3]
Q1_result_mat[18,6] <- roe_mkt_smb_hml_umd[["coefficients"]][, "t value"][4]
Q1_result_mat[18,7] <- roe_mkt_smb_hml_umd[["coefficients"]][, "t value"][5]


# panel B 
q1_panel_B_cols <- c("index", "IA","ROE","MKT-RF", "SMB", "HML", "UMD")
Q1_panel_B <- matrix(nrow=12, ncol=7)
colnames(Q1_panel_B) <- q1_panel_B_cols
Q1_panel_B[1,1] <- "ME"
Q1_panel_B[3,1] <- "IA"
Q1_panel_B[5,1] <- "ROE"
Q1_panel_B[7,1] <- "MKT-RF"
Q1_panel_B[9,1] <- "SMB"
Q1_panel_B[11,1] <- "HML"

# ME
Q1_panel_B[1,2] <- cor(q_factors_1972$ME, q_factors_1972$IA)
Q1_panel_B[1,3] <- cor(q_factors_1972$ME, q_factors_1972$ROE)
Q1_panel_B[1,4] <- cor(q_factors_1972$ME, q_factors_1972$MKT.RF)
Q1_panel_B[1,5] <- cor(q_factors_1972$ME, FF_3Factor_1972$SMB)
Q1_panel_B[1,6] <- cor(q_factors_1972$ME, FF_3Factor_1972$HML)
Q1_panel_B[1,7] <- cor(q_factors_1972$ME, momentum_1972$Mom)

Q1_panel_B[2,2] <- cor.test(q_factors_1972$ME, q_factors_1972$IA)$p.value
Q1_panel_B[2,3] <- cor.test(q_factors_1972$ME, q_factors_1972$ROE)$p.value
Q1_panel_B[2,4] <- cor.test(q_factors_1972$ME, q_factors_1972$MKT.RF)$p.value
Q1_panel_B[2,5] <- cor.test(q_factors_1972$ME, FF_3Factor_1972$SMB)$p.value
Q1_panel_B[2,6] <- cor.test(q_factors_1972$ME, FF_3Factor_1972$HML)$p.value
Q1_panel_B[2,7] <- cor.test(q_factors_1972$ME, momentum_1972$Mom)$p.value

# IA
Q1_panel_B[3,3] <- cor(q_factors_1972$IA, q_factors_1972$ROE)
Q1_panel_B[3,4] <- cor(q_factors_1972$IA, q_factors_1972$MKT.RF)
Q1_panel_B[3,5] <- cor(q_factors_1972$IA, FF_3Factor_1972$SMB)
Q1_panel_B[3,6] <- cor(q_factors_1972$IA, FF_3Factor_1972$HML)
Q1_panel_B[3,7] <- cor(q_factors_1972$IA, momentum_1972$Mom)

Q1_panel_B[4,3] <- cor.test(q_factors_1972$IA, q_factors_1972$ROE)$p.value
Q1_panel_B[4,4] <- cor.test(q_factors_1972$IA, q_factors_1972$MKT.RF)$p.value
Q1_panel_B[4,5] <- cor.test(q_factors_1972$IA, FF_3Factor_1972$SMB)$p.value
Q1_panel_B[4,6] <- cor.test(q_factors_1972$IA, FF_3Factor_1972$HML)$p.value
Q1_panel_B[4,7] <- cor.test(q_factors_1972$IA, momentum_1972$Mom)$p.value

# ROE 
Q1_panel_B[5,4] <- cor(q_factors_1972$ROE, q_factors_1972$MKT.RF)
Q1_panel_B[5,5] <- cor(q_factors_1972$ROE, FF_3Factor_1972$SMB)
Q1_panel_B[5,6] <- cor(q_factors_1972$ROE, FF_3Factor_1972$HML)
Q1_panel_B[5,7] <- cor(q_factors_1972$ROE, momentum_1972$Mom)

Q1_panel_B[6,4] <- cor.test(q_factors_1972$ROE, q_factors_1972$MKT.RF)$p.value
Q1_panel_B[6,5] <- cor.test(q_factors_1972$ROE, FF_3Factor_1972$SMB)$p.value
Q1_panel_B[6,6] <- cor.test(q_factors_1972$ROE, FF_3Factor_1972$HML)$p.value
Q1_panel_B[6,7] <- cor.test(q_factors_1972$ROE, momentum_1972$Mom)$p.value

# MKT 
Q1_panel_B[7,5] <- cor(q_factors_1972$MKT.RF, FF_3Factor_1972$SMB)
Q1_panel_B[7,6] <- cor(q_factors_1972$MKT.RF, FF_3Factor_1972$HML)
Q1_panel_B[7,7] <- cor(q_factors_1972$MKT.RF, momentum_1972$Mom)

Q1_panel_B[8,5] <- cor.test(q_factors_1972$MKT.RF, FF_3Factor_1972$SMB)$p.value
Q1_panel_B[8,6] <- cor.test(q_factors_1972$MKT.RF, FF_3Factor_1972$HML)$p.value
Q1_panel_B[8,7] <- cor.test(q_factors_1972$MKT.RF, momentum_1972$Mom)$p.value

# SMB 
Q1_panel_B[9,6] <- cor(q_factors_1972$MKT.RF, FF_3Factor_1972$HML)
Q1_panel_B[9,7] <- cor(q_factors_1972$MKT.RF, momentum_1972$Mom)

Q1_panel_B[10,6] <- cor.test(q_factors_1972$MKT.RF, FF_3Factor_1972$HML)$p.value
Q1_panel_B[10,7] <- cor.test(q_factors_1972$MKT.RF, momentum_1972$Mom)$p.value


# HMLs 
Q1_panel_B[11,7] <- cor(q_factors_1972$MKT.RF, momentum_1972$Mom)

Q1_panel_B[12,7] <- cor.test(q_factors_1972$MKT.RF, momentum_1972$Mom)$p.value

write.table(Q1_result_mat, file="panelA.csv", row.names=FALSE, sep=",")
write.table(Q1_panel_B, file="panelB.csv", row.names=FALSE, sep=",")
}

runQn1

# Qn 2 

########################## Run Qn 2 ########################## 
########################## clean CRSP ########################## 
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

########################################################################################################

########################## clean compustat annual and quarterly ########################## 
compustat_annual <- as.data.table(read.csv("compustat_annual.csv"))
# remove duplicates according to the instruction 
compustat_annual <- compustat_annual[indfmt=="INDL" & datafmt=="STD" & popsrc=="D" & consol=="C",]

# convert date to date datatype
compustat_annual[, date:= ymd(datadate)]
compustat_annual[, Year := year(date)]
compustat_annual[, Month := month(date)]

setkey(compustat_annual, gvkey, Year, Month)

# measure investment-to-assets, annual change in total assets (Compustat annual itemAT) divided by 1-year-lagged total assets
compustat_annual[, IA := at/ shift(at), by = gvkey]

# get ROE 
compustat_quarterly <- as.data.table(read.csv("compustat_quarterly.csv"))
# convert date to date datatype
compustat_quarterly[, date:= ymd(datadate)]
compustat_quarterly[, Year := year(date)]
compustat_quarterly[, Month := month(date)]

setkey(compustat_quarterly, GVKEY, fyearq, fqtr)

# calculates the shareholders equity (SHE)
compustat_quarterly[,SHE:= coalesce(seqq, ceqq + pstkq, atq - ltq)]

compustat_quarterly[, MinusPS := - pstkrq]

# calculates book equity 
compustat_quarterly[, BE:= rowSums(.SD, na.rm = T),.SDcols=c("SHE","txditcq","MinusPS")]
compustat_quarterly[, ROE := ibq/shift(BE), by = GVKEY]

########################################################################################################

########################## merge compustat and linktable ########################## 
linktable <- as.data.table(read.csv("linktable.csv"))

#### code from TA session
merged_crsp <- merge(crsp_monthly, linktable, by.x='PERMCO', by.y = 'LPERMCO', allow.cartesian = T)
setkey(merged_crsp)

merged_crsp[,LINKDT := ymd(LINKDT)]
merged_crsp[LINKENDDT == 'E', LINKENDDT := NA]
merged_crsp[,LINKENDDT := ymd(LINKENDDT)]
merged_crsp <- merged_crsp[(is.na(LINKDT) | date >= LINKDT) & (is.na(LINKENDDT) | date <= LINKENDDT)]
setorder(merged_crsp, gvkey, date)

# Multiple GVKEYs per PERMCO

### First, if LC not LC linktype, only keep LC
# identify Same PERMCO but different PERMNO
merged_crsp[, prob := .N > 1, by = .(PERMCO, date)]
merged_crsp[, Good_match := sum(LINKTYPE == 'LC'), by =.(PERMCO, date)]
merged_crsp <- merged_crsp[!(prob == T & Good_match == T & LINKTYPE != 'LC')]

### Second, if P and not P linkprim, only keep p
merged_crsp[, prob := .N > 1, by= .(PERMCO, date)]
merged_crsp[, Good_match := sum(LINKPRIM == 'P'), by =.(PERMCO, date)]
merged_crsp <- merged_crsp[!(prob == T & Good_match == T & LINKPRIM != 'P')]

### Third, if 1 and not liid, only keep 1 
merged_crsp[, prob := .N > 1, by = .(PERMCO, date)]
merged_crsp[, Good_match := sum(LIID == 1), by =.(PERMCO,date)]
merged_crsp <- merged_crsp[!(prob == T & Good_match == T & LIID != 1)]

### Fourth, use the link that's current
merged_crsp[, prob := .N > 1, by = .(PERMCO, date)]
merged_crsp[, Good_match := sum(is.na(LINKENDDT)), by = .(PERMCO, date)]
merged_crsp <- merged_crsp[!(prob==T & Good_match == T & !is.na(LINKENDDT))]

### Fifth, use the link that's been around the longest
merged_crsp[, prob := .N > 1, by = .(PERMCO, date)]
merged_crsp[, Good_match := NULL]
merged_crsp[is.na(LINKENDDT), LINKENDDT := as.Date('2019-12-31', '%Y-%m-%d')]
merged_crsp[, Date_diff := as.integer(LINKENDDT - LINKDT)]
setorder(merged_crsp, PERMCO, date, Date_diff)
merged_crsp[prob == T, Good_match := Date_diff == Date_diff[.N], by = .(PERMCO, date)]
merged_crsp <- merged_crsp[!(prob==T & Good_match != T)]

### Sixth, use the gvkey that has been around the longest
merged_crsp[, prob := .N > 1, by = .(PERMCO, date)]
merged_crsp[, Good_match :=NULL]
setorder(merged_crsp, gvkey, LINKDT)
merged_crsp[prob == T, start_Date := LINKDT[1], by = .(gvkey)]
setorder(merged_crsp, gvkey, LINKENDDT)
merged_crsp[prob == T, end_Date := LINKENDDT[.N], by = .(gvkey)]
merged_crsp[, Date_diff := as.integer(end_Date - start_Date)]
setorder(merged_crsp, PERMCO, date, Date_diff)
merged_crsp[prob == T, Good_match := Date_diff == Date_diff[.N], by = .(PERMCO, date)]
merged_crsp <- merged_crsp[!(prob == T & Good_match != T)]

### Seventh, use the smaller gvkey
setorder(merged_crsp, PERMCO, date, gvkey)
merged_crsp <- unique(merged_crsp, by = c('PERMCO', 'date'))

### Clean up extra variables and final check of match
merged_crsp <- merged_crsp[, .(gvkey, date, EXCHCD, Mkt_cap, PERMCO, PERMNO, Ret , Year, Month)]

#####################################################################################

################################ Merge CRSP and Compustat together ############################################### 
compustat_quarterly_cut <- compustat_quarterly[, .(GVKEY, fyearq, fqtr, ROE)]
compustat_annual_cut <- compustat_annual[, .(gvkey, Year, IA)]

# get quarterly for monthly data
merged_crsp[, qtr := ifelse(Month<4, 1,ifelse(Month<7, 2,ifelse(Month<10, 3, 4)))]

finaldata <- merged_crsp %>%
  left_join(compustat_quarterly_cut,by =c("gvkey"="GVKEY","Year"="fyearq", "qtr"="fqtr")) %>% 
  left_join(compustat_annual_cut,by =c("gvkey"="gvkey","Year"="Year")) %>%
  as.data.table

setkey(finaldata, Year, Month)

write.table(finaldata, file ="finaldata.csv", row.names=FALSE, sep=",")
#####################################################################################

########################### size sorting ###################################################
rm(list=ls())
finaldata <- as.data.table(read.csv("finaldata.csv"))
finaldata_cleaned<- finaldata[, MktCap := sum(Mkt_cap,na.rm = T), .(PERMCO, Year, Month)]
finaldata_cleaned[IA> 100000, IA:=NA]
finaldata_cleaned[ROE>100000, ROE:=NA]
finaldata_cleaned[ROE< -100000, ROE:=NA]
# PERMCO is a unique permanent identifier assigned by CRSP to all companies with issues on a CRSP file. 
# This number is permanent for all securities issued by this company regardless of name changes.
# The PERMNO identifies a firm's security through all its history, and companies may have several stocks at one time.
# In short: A PERMCO can have multiple PERMNOs

# Adds up stocks from the same company based on PERMCO, 
finaldata_JUNE <- finaldata_cleaned[Month == 6, .(PERMCO, Year, MktCap, EXCHCD, IA,ROE)]

# sorting into deciles
# NYSE stocks are used as breakpoints for all the stocks in NYSE AMEX and NASDAQ
finaldata_JUNE[,size_decile := findInterval(MktCap, median(.SD[EXCHCD==1,MktCap]), left.open = T) + 1,by = .(Year)]

# independently sort on IA
finaldata_JUNE_IA <- finaldata_cleaned[Month == 6, .(PERMCO, Year, MktCap, EXCHCD, IA,ROE)]
finaldata_JUNE_IA <- finaldata_JUNE_IA[!is.na(IA)]
finaldata_JUNE_IA[,IA_decile := findInterval(IA, quantile(.SD[EXCHCD==1, IA], c(0.3,0.7), na.rm = T), left.open = T) + 1, by = .(Year)]


finaldata_JUNE_size_clean <- finaldata_JUNE[,.(PERMCO, Year, size_decile)]
finaldata_JUNE_IA_clean <- finaldata_JUNE_IA[,.(PERMCO, Year, IA_decile)]
mergedDeciles <- merge(finaldata_JUNE_size_clean, finaldata_JUNE_IA_clean, by =c("PERMCO"="PERMCO","Year"="Year"), all = T)

# independently sort on ROE
finaldata_ROE <- finaldata_cleaned[, .(PERMCO, Year, Month, qtr, MktCap, EXCHCD, IA,ROE)]
finaldata_ROE <- finaldata_ROE[!is.na(ROE)]
finaldata_ROE[,ROE_decile := findInterval(ROE, quantile(.SD[EXCHCD==1, ROE], c(0.3,0.7), na.rm = T), left.open = T) + 1, by = .(Year, Month)]
