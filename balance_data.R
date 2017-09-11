##### SMOTE for Imbalanced Data #####
train.data = read.csv('cs-train-missing.csv')
# Use SMOTE to re-sampling the training data
# The minority class is over-sampled by taking each minority class sample and 
# introducing synthetic examples along the line segments joining any/all of the 
# k minority class nearest neighbors. 
set.seed(123)
train.data$SeriousDlqin2yrs <- as.factor(train.data$SeriousDlqin2yrs)
train.data <- train.data[,c(2:11,1)] ; test.data <- test.data[,c(2:11,1)]
train.smote <- SMOTE(SeriousDlqin2yrs~.,data=train.data,perc.over=500,perc.under=120)
write.csv(data,'cs-training-feature-01.csv')
