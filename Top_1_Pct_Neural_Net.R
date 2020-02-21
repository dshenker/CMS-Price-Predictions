library(data.table)
library(glmnet) 
library(moments)
library(psych)
library(dplyr)
library(pROC) 
library(neuralnet)
library(caret)

ben <- read.csv("/Users/dshenker/Desktop/Kharrazi_ML_Materials/teach/beneficiary.csv", header = T)
ben <- data.table(ben)
ben_2008 <- ben[year == "2008"]
ben_2009 <- ben[year == "2009"]
ben_2009 <- ben_2009[, PYMT_SUM := rowSums(.SD), .SDcols = 24:32][]
ben_2008 <- ben_2008[, PYMT_SUM := rowSums(.SD), .SDcols = 24:32][]
id_2008 <- ben_2008$DESYNPUF_ID
ben_2009 <- ben_2009[DESYNPUF_ID %in% id_2008]
id_2009 <- ben_2009$DESYNPUF_ID
ben_2008 <- ben_2008[DESYNPUF_ID %in% id_2009]
names(ben_2008)
ben_2008$BENE_BIRTH_DT
ben_2008 <- ben_2008[, BRTH_YR := as.numeric(substr(BENE_BIRTH_DT, 1, 4))]
ben_2008 <- ben_2008[, AGE := year - BRTH_YR]
ben_2009 <- ben_2009[, BRTH_YR := as.numeric(substr(BENE_BIRTH_DT, 1, 4))]
ben_2009 <- ben_2009[, AGE := year - BRTH_YR]
cols_drop = c(2,3,7,8,9,10,11,12,24,25,26,27,28,29,30,31,32)
ben_2008 <- subset(ben_2008, select = -cols_drop)
ben_2009 <- subset(ben_2009, select = -cols_drop)
ben_2008_predictors <- subset(ben_2008, select = -c(year, BRTH_YR))

##Bring in the tables from Long_Table.R
charlson_table <- data.table(charlson_table$DESYNPUF_ID, charlson_table$wscore)
names(charlson_table) <- c("DESYNPUF_ID", "wscore_charlson")

ben_2008_predictors <- left_join(ben_2008_predictors, charlson_table, by = "DESYNPUF_ID")
ben_2008_final[is.na(ben_2008_final)] <- 0
head(ben_2008_final)
ben_2009_cost <- subset(ben_2009, select = c(DESYNPUF_ID, PYMT_SUM))
ben_08_09 <- inner_join(ben_2008_predictors, ben_2009_cost, by = "DESYNPUF_ID")
ben_08_09 <- subset(ben_08_09, PYMT_SUM.x > 0)
ben_08_09 <- subset(ben_08_09, PYMT_SUM.y > 0)
ben_08_09 <- as.data.table(ben_08_09)

#top 1 percent for 2008
quantile(ben_08_09$PYMT_SUM.x, c(.99)) #67727.44

#top 1 percent for 2009
quantile(ben_08_09$PYMT_SUM.y, c(.99)) #56535.52

ben_08_09$PAST_1_PCT <- with(ben_08_09, PYMT_SUM.x >= 67727.44)
ben_08_09$FUTURE_1_PCT <- with(ben_08_09, PYMT_SUM.y >= 56535.52)
ben_08_09 <- ben_08_09[, COST_LOG := log(PYMT_SUM.x)]

ben_2009_top <- subset(ben_08_09, select = FUTURE_1_PCT)
ben_2008_final <- subset(ben_08_09, select = c(BENE_SEX_IDENT_CD, BENE_RACE_CD, AGE, wscore_charlson, COST_LOG, FUTURE_1_PCT))
head(ben_2008_final)
names(ben_2009_top) <- c("predTop")
names(ben_2008_final)[5] <- c("costParam")

#USE NEURAL NETWORK
index <- sample(1:nrow(ben_2008_final),round(0.75*nrow(ben_2008_final))) #get indices for split
train <- ben_2008_final[index,] #get training set
test <- ben_2008_final[-index,] #get test set
nn = neuralnet(FUTURE_1_PCT~BENE_SEX_IDENT_CD + BENE_RACE_CD + AGE + wscore_charlson + costParam, 
               data = train, 
               hidden = 4, 
               act.fct = "logistic", 
               linear.output = FALSE) #train the neural net

predict_train = neuralnet::compute(nn, train) #get predictions on training set
prob_train <- predict_train$net.result #extract the probabilities
predict_test = neuralnet::compute(nn, test) #get predictions on test set
prob_test <- predict_test$net.result #extract the probabilities
test$data_pat.predict <- as.vector(predict_test$net.result)
model_cost_top_roc <- roc(FUTURE_1_PCT ~ data_pat.predict, data = test)     # apply the ROC function to calculate the ROC information
plot(model_cost_top_roc) #plot the curve (looking for distance from diagonal line)
as.numeric(model_cost_top_roc$auc) #get area under curve value
coords(model_cost_top_roc, 'best', 'threshold', transpose = FALSE) #get best threshold - 0.01048064

#Confusion Matrix
table(test$FUTURE_1_PCT, test$data_pat.predict > 0.006561348)        







