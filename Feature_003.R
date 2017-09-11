setwd('/home/tapas')
data = read.csv('cs-training-feature-02.csv',header =T)
test = read.csv('cs-test-feature-02.csv',header =T)

new = data %>% filter(MonthlyIncome != 0)%>%group_by(NumberOfDependents) %>% summarise(avg=median(MonthlyIncome,na.rm=T))

DepdRisk = 1:150000
for(i in 1:150000){
  if(is.na(data$MonthlyIncome[i])){
    DepdRisk[i] = NA
  }else{
    if(data$MonthlyIncome[i] == 0){
      DepdRisk[i] = data$NumberOfDependents[i]/(new[new$NumberOfDependents==data$NumberOfDependents[i],]$avg)
    }else{
      DepdRisk[i] = data$NumberOfDependents[i]/as.numeric(data$MonthlyIncome[i])
    }
  }
}

data = as.data.frame(cbind(data,AgeRisk,default_time,DebtAmt,DepdRisk))
data = data[,-c(4,5,6,7,9,11,12)]
write.csv(data,'cs-training-feature-03.csv')

########################Test#######################################

default_time = w1 * test$NumberOfTime3059DaysPastDueNotWorse +w2 * test$NumberOfTime6089DaysPastDueNotWorse + w3 * test$NumberOfTimes90DaysLate
DebtAmt = test$DebtRatio * as.numeric(test$MonthlyIncome)
new = test %>% filter(MonthlyIncome != 0)%>%group_by(NumberOfDependents) %>% summarise(avg=median(MonthlyIncome,na.rm=T))

DepdRisk = 1:101503
for(i in 1:101503){
  if(is.na(test$MonthlyIncome[i])){
    DepdRisk[i] = NA
  }else{
    if(test$MonthlyIncome[i] == 0){
      DepdRisk[i] = test$NumberOfDependents[i]/(new[new$NumberOfDependents==test$NumberOfDependents[i],]$avg)
    }else{
      DepdRisk[i] = test$NumberOfDependents[i]/as.numeric(test$MonthlyIncome[i])
    }
  }
}
test = as.data.frame(cbind(test,AgeRisk,default_time,DebtAmt,DepdRisk))
test = test[,-c(4,5,7,9,11,12)]

write.csv(test,'cs-test-feature-03.csv')
