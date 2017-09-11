rm(list=ls())
setwd('/home//tapas/')
data = read.csv('cs-training-feature-01.csv', header =T)
test = read.csv('cs-test-feature-01.csv',header=T)
names(data)
summary(data)
describe()

##By the summary, we can apply log transformation on RevolvingUtilizationOfUnsecuredLines,NumberOfTime30.59DaysPastDueNotWorse,
##DebtRatio, MonthlyIncome, NumberOfOpenCreditLinesAndLoans, NumberOfTimes90DaysLate, NumberRealEstateLoansOrLines, NumberOfTime60.89DaysPastDueNotWorse
##NumberOfDependents


data$RevolvingUtilizationOfUnsecuredLines = log(1/(sqrt(data$RevolvingUtilizationOfUnsecuredLines))-2)
ggplot(data=data, aes(data$RevolvingUtilizationOfUnsecuredLines)) + geom_histogram()

data$NumberOfTime30.59DaysPastDueNotWorse = log(data$NumberOfTime30.59DaysPastDueNotWorse+1)
ggplot(data=data, aes(data$NumberOfTime30.59DaysPastDueNotWorse)) + geom_histogram()

data$DebtRatio = log((1/((data$DebtRatio))^1/2)-1)
ggplot(data=data, aes(data$DebtRatio)) + geom_histogram()

data$MonthlyIncome = log(((data$MonthlyIncome)^1/2) -1)
ggplot(data=data, aes(data$MonthlyIncome)) + geom_histogram()

data$NumberOfOpenCreditLinesAndLoans = log((data$NumberOfOpenCreditLinesAndLoans^2)+0.5)
ggplot(data=data, aes(data$NumberOfOpenCreditLinesAndLoans)) + geom_histogram()

data$NumberOfTimes90DaysLate = log((data$NumberOfTimes90DaysLate)+1)
ggplot(data=data, aes(data$NumberOfTimes90DaysLate)) + geom_histogram()

data$NumberOfTime60.89DaysPastDueNotWorse = log(data$NumberOfTime60.89DaysPastDueNotWorse+1)
ggplot(data=data, aes(data$NumberOfTime60.89DaysPastDueNotWorse)) + geom_histogram()

data$NumberOfDependents = log(((0.5)^(data$NumberOfDependents))+0.35)
ggplot(data=data, aes(data$NumberOfDependents)) + geom_histogram()

data$NumberRealEstateLoansOrLines = log((data$NumberRealEstateLoansOrLine)+1)
ggplot(data=data, aes(data$NumberRealEstateLoansOrLines)) + geom_histogram()

write.csv(data,'cs-training-feature-last.csv')

test = read.csv('cs-test.csv', header =T)
summary(test)
decribe(test)

test$RevolvingUtilizationOfUnsecuredLines = log(1/(sqrt(test$RevolvingUtilizationOfUnsecuredLines))-2)
ggplot(data=test, aes(test$RevolvingUtilizationOfUnsecuredLines)) + geom_histogram()

test$NumberOfTime30.59DaysPastDueNotWorse = log(test$NumberOfTime30.59DaysPastDueNotWorse+1)
ggplot(data=test, aes(test$NumberOfTime30.59DaysPastDueNotWorse)) + geom_histogram()

test$DebtRatio = log((1/((test$DebtRatio))^1/2)-1)
ggplot(data=test, aes(test$DebtRatio)) + geom_histogram()

test$MonthlyIncome = log(((test$MonthlyIncome)^1/2) -1)
ggplot(data=data, aes(test$MonthlyIncome)) + geom_histogram()

test$NumberOfOpenCreditLinesAndLoans = log((test$NumberOfOpenCreditLinesAndLoans^2)+0.5)
ggplot(data=test, aes(test$NumberOfOpenCreditLinesAndLoans)) + geom_histogram()

test$NumberOfTimes90DaysLate = log((test$NumberOfTimes90DaysLate)+1)
ggplot(data=test, aes(test$NumberOfTimes90DaysLate)) + geom_histogram()

data$NumberOfTime60.89DaysPastDueNotWorse = log(data$NumberOfTime60.89DaysPastDueNotWorse+1)
ggplot(data=test, aes(test$NumberOfTime60.89DaysPastDueNotWorse)) + geom_histogram()

test$NumberOfDependents = log(((0.5)^(test$NumberOfDependents))+0.35)
ggplot(data=test, aes(test$NumberOfDependents)) + geom_histogram()

data$NumberRealEstateLoansOrLines = log((test$NumberRealEstateLoansOrLine)+1)
ggplot(data=test, aes(data$NumberRealEstateLoansOrLines)) + geom_histogram()

write.csv(test,'cs-test-feature-last.csv')
