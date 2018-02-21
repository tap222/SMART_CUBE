library(dplyr)
library(magrittr)
library(ggplot2)
library(lattice)
library(Amelia)
library(caret)
library(ROSE)
library(ROCR)
library(cvAUC)
library(xgboost)
library(DiagrammeR)
library(moments)

setwd('/home/tapas/chupak/')

# load the data --------------------------------------------------
trainDat<-read.csv('cs-training.csv',header = TRUE)
head(trainDat)

testDat<-read.csv('cs-test.csv',header = TRUE)
head(testDat)

# drop the row id column 
trainDat<-trainDat[,-1]
testDat<-testDat[,-1]


# Missing Data ---------------------------------------------------------
sapply(trainDat, function(x) sum(is.na(x))/length(x))*100
sapply(testDat, function(x) sum(is.na(x))/length(x))*100

# Data distributions & imputation --------------------------------------

# 1. age distribution ----------------------------------------------------
ggplot(data = trainDat,aes(age))+geom_histogram(col='red',fill='red')+
  labs(title='Histogram of Age')
qqnorm(trainDat$age, main = "Age")
qqline(trainDat$age)
sum(is.na(trainDat$age))
summary(trainDat$age)

# 2. RevolvingUtilizationOfUnsecuredLines imputation -----------------------
hist(trainDat$RevolvingUtilizationOfUnsecuredLines,col='steelblue')
summary(trainDat$RevolvingUtilizationOfUnsecuredLines)
boxplot(trainDat$RevolvingUtilizationOfUnsecuredLines)
# ratio should be between 0 and 1. 

# number of missing values
sum(is.na(trainDat$RevolvingUtilizationOfUnsecuredLines))

# outliers
sum(trainDat$RevolvingUtilizationOfUnsecuredLines>1)

# not a normal distribution 
qqnorm(trainDat$RevolvingUtilizationOfUnsecuredLines[trainDat$RevolvingUtilizationOfUnsecuredLines<=1])
qqline(trainDat$RevolvingUtilizationOfUnsecuredLines)
skewness(trainDat$RevolvingUtilizationOfUnsecuredLines)

# replace the abnormal values with median

trainDat$RevolvingUtilizationOfUnsecuredLines[trainDat$RevolvingUtilizationOfUnsecuredLines>1]=0.15

ggplot(data = trainDat,aes(RevolvingUtilizationOfUnsecuredLines))+geom_histogram(col='red',fill='red')+
  labs(title='Histogram of RevolvingUtilizationOfUnsecuredLines')

boxplot(trainDat$RevolvingUtilizationOfUnsecuredLines)

# 3. NumberOfTime30.59DaysPastDueNotWorse imputations -------------------------
summary(trainDat$NumberOfTime30.59DaysPastDueNotWorse)

# number of missing values 
sum(is.na(trainDat$NumberOfTime30.59DaysPastDueNotWorse))

# distribution 
ggplot(data = trainDat,aes(NumberOfTime30.59DaysPastDueNotWorse))+geom_histogram(col='black',fill='red')+
  labs(title='Histogram Num of time past 30-59days')

table(trainDat$NumberOfTime30.59DaysPastDueNotWorse)
# shows a few people with 96 and 98 times which is quite absurd

# mean.impu<- mean(trainDat$NumberOfTime30.59DaysPastDueNotWorse[trainDat$NumberOfTime30.59DaysPastDueNotWorse<96])

trainDat$NumberOfTime30.59DaysPastDueNotWorse[trainDat$NumberOfTime30.59DaysPastDueNotWorse>=96]<-0

ggplot(data = trainDat,aes(NumberOfTime30.59DaysPastDueNotWorse))+geom_histogram(col='black',fill='red')+
  labs(title='Histogram Num of time past 30-59days')

# 4. DebtRatio imputations ---------------------------------------------------------

summary(trainDat$DebtRatio)

boxplot(trainDat$DebtRatio)
sum(is.na(trainDat$DebtRatio))

# debt ratio greater than 100k seems weird 
# lets remove them 
trainDat<-trainDat[-which(trainDat$DebtRatio>100000),]
summary(trainDat$DebtRatio)

boxplot(trainDat$DebtRatio)

# 5. MonthlyIncome imputations ---------------------------------------------
sum(is.na(trainDat$MonthlyIncome))

summary(trainDat$MonthlyIncome)

ggplot(data = trainDat,aes(MonthlyIncome))+geom_histogram(col='black',fill='red')+
  labs(title='Histogram of MonthlyIncome')
# lets do a median imputation for the monthly income
trainDat$MonthlyIncome[is.na(trainDat$MonthlyIncome)]<-median(trainDat$MonthlyIncome,na.rm = TRUE)
boxplot(trainDat$MonthlyIncome)



# remove the richie rich, itll be a levrage point and we dont need 
# that for the model fitting
trainDat<-trainDat[-which(trainDat$MonthlyIncome>300000),]
ggplot(data = trainDat,aes(MonthlyIncome))+geom_histogram(col='black',fill='red')+
  labs(title='Histogram of MonthlyIncome')

# 6. NumberOfOpenCreditLinesAndLoans imputations -----------------------------------------

sum(is.na(trainDat$NumberOfOpenCreditLinesAndLoans))

summary(trainDat$NumberOfOpenCreditLinesAndLoans)

ggplot(data = trainDat,aes(NumberOfOpenCreditLinesAndLoans))+geom_histogram(col='red',fill='green')+
  labs(title='Histogram of num of open credit lines')

boxplot(trainDat$NumberOfOpenCreditLinesAndLoans)
# seems like this variable needs no imputations

# 7. NumberOfTimes90DaysLate imputations -------------------------------------

sum(is.na(trainDat$NumberOfTimes90DaysLate))
summary(trainDat$NumberOfTimes90DaysLate)

ggplot(data = trainDat,aes(NumberOfTimes90DaysLate))+geom_histogram(col='red',fill='green')+
  labs(title='Histogram of num of open credit lines')

boxplot(trainDat$NumberOfTimes90DaysLate)

# trainDat$SeriousDlqin2yrs[trainDat$NumberOfTimes90DaysLate,1]

table(trainDat$NumberOfTimes90DaysLate)
# NO ONE can default over 90 so many times thats silly must be some typo
# impute zero coz median seems to be zero anyway and the mean wouldnt make sense

trainDat$NumberOfTimes90DaysLate[trainDat$NumberOfTimes90DaysLate>90]<-0

ggplot(data = trainDat,aes(NumberOfTimes90DaysLate))+geom_histogram(col='red',fill='green')+
  labs(title='Histogram of num of open credit lines')

# 8. NumberRealEstateLoansOrLines imputations ------------------------------
summary(trainDat$NumberRealEstateLoansOrLines)
sum(is.na(trainDat$NumberRealEstateLoansOrLines))
table(trainDat$NumberRealEstateLoansOrLines)
trainDat<-trainDat[-(which(trainDat$NumberRealEstateLoansOrLines==54)),]

ggplot(data = trainDat,aes(NumberRealEstateLoansOrLines))+geom_histogram(col='red',fill='green')+
  labs(title='Histogram of num of real estate loans')

# 9. NumberOfTime60.89DaysPastDueNotWorse imputations ----------------------

sum(is.na(trainDat$NumberOfTime60.89DaysPastDueNotWorse))

ggplot(data = trainDat,aes(NumberOfTime60.89DaysPastDueNotWorse))+geom_histogram(col='red',fill='green')+
  labs(title='Histogram of num of times 60-89 days')
table(trainDat$NumberOfTime60.89DaysPastDueNotWorse)

summary(trainDat$NumberOfTime60.89DaysPastDueNotWorse)
# same as above push the absurd values to zero 
trainDat$NumberOfTime60.89DaysPastDueNotWorse[trainDat$NumberOfTime60.89DaysPastDueNotWorse>90]<-0

ggplot(data = trainDat,aes(NumberOfTime60.89DaysPastDueNotWorse))+geom_histogram(col='red',fill='green')+
  labs(title='Histogram of num of times 60-89 days')

# 10. NumberOfDependents imputations -----------------------------------

# some missing values
sum(is.na(trainDat$NumberOfDependents))

summary(trainDat$NumberOfDependents)

# lets just make them 0 the missing vals

trainDat$NumberOfDependents[is.na(trainDat$NumberOfDependents)]<-0

# Data Balance testing -------------------------------------------------- 

prop.table(table(trainDat$SeriousDlqin2yrs))
barplot(prop.table(table(trainDat$SeriousDlqin2yrs)),col = 'steelblue')
# serious data imbalance
# addressing the data imbalance by downsampling the 0 class in serious
# dlwin2yrs 

sum(trainDat$SeriousDlqin2yrs==1)
newtrainDat<-trainDat[trainDat$SeriousDlqin2yrs==1,]
DownsampleDat<-trainDat[trainDat$SeriousDlqin2yrs==0,]
downsam<-sample(1:139948,11000)

nDat<-rbind(newtrainDat,DownsampleDat[downsam,])
nDat<-nDat[sample(nrow(nDat)),]
rownames(nDat)<-NULL

set.seed(36)
trainIndex <- createDataPartition(nDat$SeriousDlqin2yrs, p = .8, 
                                  list = FALSE, 
                                  times = 1)
ntrain<-nDat[trainIndex,]
ntest<-nDat[-trainIndex,]

# testdata imputations ------------------------------------------------

testDat$MonthlyIncome<- median(trainDat$MonthlyIncome,na.rm = TRUE)
testDat$NumberOfDependents[is.na(testDat$NumberOfDependents)] <- 0

ntrain.gbm<-ntrain
ntrain$SeriousDlqin2yrs<-as.factor(ntrain$SeriousDlqin2yrs)

# Data viz ------------------------------------------------------------
library(reshape2)

# melting the data frame
feature.names<-names(nDat)[-1]

vizDat<- melt(nDat,id.vars = 'SeriousDlqin2yrs'
              ,measure.vars = feature.names, variable.name = "Feature"
              ,value.name = "Value")

# conditional box plots for each feature on the response variable
p <- ggplot(data = vizDat, aes(x=Feature, y=Value)) + 
  geom_boxplot(aes(fill=SeriousDlqin2yrs))
p <- p + facet_wrap( ~ Feature, scales="free")
p + ggtitle("Conditional Distributions of each variable")

# lets look at the data in the first two principal components

nDat.s<- scale(nDat[,-1],center=TRUE, scale=TRUE) #standardization
Dat.pc<-prcomp(nDat.s) #princpal component
summary(Dat.pc)
# pr1 and pr2 only explain 36% of the variance but still 
# lets look at the distribution of for fun
plot(Dat.pc$x[,1:2],col=as.factor(nDat[,1]))

# log trainsforming all the predictor
LnDat<-log(ntrain[,-1]+0.001)
LnDat<-cbind(LnDat,ntrain[,1])

################
# lets build models!!
################



# logistic regression -------------------------------------------------
library(effects)

# Main effect models 
logi.model<-glm(SeriousDlqin2yrs~.,data = ntrain,family = binomial)
summary(logi.model)
plot(allEffects(logi.model))

# ROC and AUROC
pred.logi.model<-predict(logi.model,ntest[,-1],type='response')
pr <- prediction(pred.logi.model, ntest$SeriousDlqin2yrs)
prf0 <- performance(pr, measure = "tpr", x.measure = "fpr")

# random forest -------------------------------------------------------
library(randomForest)

a<-proc.time()
credit.forest<-randomForest(SeriousDlqin2yrs~.,
                            data = ntrain,mtry=2,
                            importance=TRUE,
                            ntree=50)
proc.time()-a

varImpPlot(credit.forest)

pred.forest<-predict(credit.forest,newdata = ntest[,-1],'prob')
output<-pred.forest[,2]
pr <- prediction(output, ntest$SeriousDlqin2yrs)
prf1 <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf1)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

# xgboost -------------------------------------------------------------

dtrain <- xgb.DMatrix(data = as.matrix(ntrain[,-1])
                      , label = as.numeric(ntrain$SeriousDlqin2yrs)-1)

dtest<- xgb.DMatrix(data = as.matrix(ntest[,-1])
                    , label = as.numeric(ntest$SeriousDlqin2yrs)-1)

watchlist <- list(train=dtrain, test=dtest)

bst <- xgb.train(data=dtrain, max.depth=3
                 , eta=0.01, nthread = 2, nround=2000
                 , watchlist=watchlist, eval.metric = "error"
                 , eval.metric = "logloss"
                 
                 , objective = "binary:logistic")
print(xgb.importance(model = bst))
xgb.plot.importance(importance_matrix = xgb.importance(model = bst))
pred.xg<-predict(bst,dtest)
pr <- prediction(pred.xg, ntest$SeriousDlqin2yrs)
prf2 <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf2)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

ftest<- xgb.DMatrix(data = as.matrix(testDat[,-1])
                    , label = rep(0,nrow(testDat)))



# kaggle entries ------------------------------------------------------

# main effects logistic model 
pred.logi.model<-predict(logi.model,newdata=testDat[,-1],type='response')
write.csv(cbind(1:101503,pred.logi.model),file = 'logi_mainEff_entry.csv',row.names = F)

# random forest 
pred.forest<-predict(credit.forest,newdata = testDat[,-1],'prob')
output<-pred.forest[,2]
write.csv(cbind(1:101503,output)
          ,file = 'random_forest_entry.csv'
          ,row.names = F)

# xgboost
pred.xg<-predict(bst,ftest)
write.csv(cbind(1:101503,pred.xg),file = 'xgboost_entry.csv',row.names = F)

# ROC curves of all models
plot( prf0) #black
plot(prf1, add = TRUE,col=2 ) #red
plot(prf2, add = TRUE, col=3) #green

