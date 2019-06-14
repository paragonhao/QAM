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



