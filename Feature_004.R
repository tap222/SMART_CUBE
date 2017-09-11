######In this file, we do the feature engineering by multiply DebtRatio with MonthlyIncome to get DebtAmt. Then drop MonthlyIncome and
#####DebtRatio.

setwd('/home/tapas')
data = read.csv('cs-training-feature-03.csv', header =T)
test = read.csv('cs-test-feature-03.csv', header = T)
names(data)


DebtAmt = data$DebtRatio * as.numeric(data$MonthlyIncome)
DebtAmt = test$DebtRatio * as.numeric(test$MonthlyIncome)

data = as.data.frame(cbind(data, DebtAmt))
data = data[,-c(6,7)]

test = as.data.frame(cbind(test, DebtAmt))
test = test[,-c(6,7)]

write.csv(data,'cs-training-feature-04.csv')
write.csv(test,'cs-test-feature-04.csv')
