setwd('/home/tapas')
train.data = read.csv('cs-training.csv')
test.data = read.csv('cs-test.csv')

###### K-NN for Missing Values ######

# Use k-NN to deal with NA in training data
# For each case with any NA value it will search for its k most similar cases 
# and use the values of these cases to fill in the unknowns
set.seed(123)
dd <- knnImputation(train.data[,-1],k=5)
train.data[,2:11] <- dd

set.seed(123)
dt <- knnImputation(test.data[,-1],k=5)
test.data[,2:11] <- dt
write.csv(test,'cs-test-missing-knn.csv')

############################# MICE (Package) ###################################
#If we can reasonably impute them we will have a lot more data to feed to a model
#that we would otherwise have to throw out. Enter the mice package.
#Here I use the mice function, the main workhorse of the mice package. Mice stands 
#for multiple imputation by chained equations. The arguments I am using are the name
#of the dataset on which we wish to impute missing data. the ‘m’ argument indicates
#how many rounds of imputation we want to do. Often we will want to do several and 
#pool the results. For simplicity however, I am just going to do one for now. The 
#‘method’ argument indicates which of the many methods for imputation we wish to 
#use. We can specify one method, in which case that method will be used for all the
#variables. We can also specify method individually for each variable. 
#Alternatively we can specify a default method for each data type 
#(numeric, logical, ordered factor.) Here I specify ‘cart’ as the only method. 
#CART stands for classification and regression trees, otherwise know as decision 
#trees. Most of you will be familiar with these. If not, I suggest reading more
#here. Fortunately cart will work for all variable types, which makes it very 
#convenient.
imp.train_raw <- mice(train, m=5, method='cart', printFlag=FALSE)
xyplot(imp.train_raw, train$MonthlyIncome ~ train$NumberOfDependents)
densityplot(imp.train_raw, ~train$MonthlyIncome)
densityplot(imp.train_raw, ~train$NumberOfDependents)
imp.train_raw_mean <- mice(train_raw, 
                           m=5, 
                           defaultMethod=c('mean', 'cart', 'cart', 'cart'),
                           printFlag=FALSE)
xyplot(imp.train_raw_mean, train$MonthlyIncome~ train$NumberOfDependents)

train_complete <- complete(imp.train_raw)

#Confirm no NAs
sum(sapply(train_complete, function(x) { sum(is.na(x)) }))
# Plot MonthlyIncome distributions
par(mfrow=c(1,2))
hist(train$MonthlyIncome, freq=F, main='MonthlyIncome: Original Data', 
     col='darkgreen', ylim=c(0,0.04))
hist(imp.train_raw$MonthlyIncome, freq=F, main='MonthlyIncome: MICE Output', 
     col='lightgreen', ylim=c(0,0.04))
# Plot Number of Dependents distributions
par(mfrow=c(1,2))
hist(train$NumberOfDependents, freq=F, main='NumberOfDependents: Original Data', 
     col='darkgreen', ylim=c(0,0.04))
hist(imp.train_raw$NumberOfDependents, freq=F, main='NumberOfDependents: MICE Output', 
     col='lightgreen', ylim=c(0,0.04))
write.csv(test,'cs-test-missing-mice.csv')
