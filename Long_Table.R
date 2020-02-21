#Combine 3 of the claims files into a single long table
#Once the tables have been combined, we create tables for the charlson
#and elixhauser scores for each patient, which are going to be combined
#with the beneficiary data

library(data.table)
library(tidyr)
library(dplyr)
library(comorbidity)

ip <- read.csv("/Users/dshenker/Desktop/Kharrazi_ML_Materials/teach/ip.csv", header = T)
op <- read.csv("/Users/dshenker/Desktop/Kharrazi_ML_Materials/teach/op.csv", header = T)
car <- read.csv("/Users/dshenker/Desktop/Kharrazi_ML_Materials/teach/carrier.csv", header = T)

#turn into data tables
ip <- as.data.table(ip)
op <- as.data.table(op)
car <- as.data.table(car)

#extract the years
ip <- ip[, year := substr(ip$CLM_FROM_DT, 1, 4)]
op <- op[, year := substr(op$CLM_FROM_DT, 1, 4)]
car <- car[, year := substr(car$CLM_FROM_DT, 1, 4)]

#get only 2008 data
ip_08 <- ip[year == "2008"]
op_08 <- op[year == "2008"]
car_08 <- car[year == "2008"]


ip_cols <- c(1, seq(21, 30, 1))
op_cols <- c(1, seq(13, 22, 1))
car_cols <- c(1, seq(5, 12, 1))

ip_08 <- subset(ip_08, select = ip_cols)
op_08 <- subset(op_08, select = op_cols)
car_08 <- subset(car_08, select = car_cols)

colnames(ip_08) <- paste("ip", colnames(ip_08), sep = "_")
colnames(op_08) <- paste("op", colnames(op_08), sep = "_")
colnames(car_08) <- paste("car", colnames(car_08), sep = "_")
names(ip_08)[1] <- "DESYNPUF_ID"
names(op_08)[1] <- "DESYNPUF_ID"
names(car_08)[1] <- "DESYNPUF_ID"


join_step1 <- full_join(ip_08, op_08, by = "DESYNPUF_ID")
table_final <- full_join(join_step1, car_08, by = "DESYNPUF_ID")
head(table_final)

table_long <- gather(table_final, name, code, ip_ICD9_DGNS_CD_1:car_ICD9_DGNS_CD_8)
table_long <- table_long %>% distinct() 
head(table_long)

charlson_table <- comorbidity(table_long, id = "DESYNPUF_ID", code = "code", score = "charlson", assign0 = FALSE, icd = "icd9")
head(charlson_table)
dim(charlson_table)

elixhauser_table <- comorbidity(table_long, id = "DESYNPUF_ID", code = "code", score = "elixhauser", assign0 = FALSE, icd = "icd9")
head(elixhauser_table)
dim(elixhauser_table)


