setwd('/home/tapas/')
data = read.csv('cs-trainig-outlier.csv', header =T)

Debt = data$MonthlyIncome * data$DebtRatio
data = as.data.frame(cbind(data,Debt))
data = data[,-c(6,7)]

test = read.csv('cs-test.csv', header =T)

Debt = test$MonthlyIncome * test$DebtRatio
test = as.data.frame(cbind(test,Debt))
test = test[,-c(6,7)]


write.csv(data,'cs-test-feature-01.csv')
write.csv(data,'cs-training-feature-01.csv')
