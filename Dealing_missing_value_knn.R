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

