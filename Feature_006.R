setwd('/home/tapas')
data = read.csv('cs-training-feature-05.csv', header =T)
data = data[,-1]
Debt = data$MonthlyIncome * data$DebtRatio
data = as.data.frame(cbind(data,Debt))
data = data[,-c(6,7)]

write.csv(data, 'cs-training-feature-06.csv')

test = read.csv('cs-test-feature-05.csv', header =T)
test = test[,-1]
Debt = test$MonthlyIncome * test$DebtRatio
test = as.data.frame(cbind(test,Debt))

test = test[,-c(6,7)]
write.csv(data, 'cs-test-feature-06.csv')
