
data <- read.csv('cs-test-missing.csv')
####PCA#####
fa.parallel(data, #The data in question.
            fa = "pc", #Display the eigenvalues for PCA.
            n.iter = 100) #Number of simulated analyses to perform.
abline(h = 1) #Adding a horizontal line at 1.
pc_bodies = principal(data, #The data in question.
                      nfactors = 5, #The number of PCs to extract.
                      rotate = "none")
pc_bodies

heatmap <- qplot(x=Var1, y=Var2, data=melt(cor(data)), geom="tile",
                 fill=value)
heatmap

###### Scatter Plot Matrix ######

pairs(train.smote[,c(1,2,4,5,6,7,8)],col=c("seagreen","indianred")[unclass(train.smote$Dlqin2yrs)])


nearZeroVar(train.smote) # no zero variance predictors

my.theme <- trellis.par.get()
mycolors <- c("seagreen","indianred")
trellis.par.set(superpose.symbol = list(col = mycolors),
                superpose.line= list(col = mycolors))
featurePlot(x = train.smote[,1:5],y = train.smote$Dlqin2yrs,plot = "density",
            ## Pass in options to xyplot() to make it prettier
            scales = list(x = list(relation="free"),y = list(relation="free")),
            adjust = 1.5, pch = "|", layout = c(5, 1),
            auto.key = list(columns = 2))
featurePlot(x = train.smote[,6:10],y = train.smote$Dlqin2yrs,plot = "density",
            ## Pass in options to xyplot() to make it prettier
            scales = list(x = list(relation="free"),y = list(relation="free")),
            adjust = 1.5, pch = "|", layout = c(5, 1),
            auto.key = list(columns = 2))

####### Correlation Matrix #######
corr.matrix <- cor(train.smote[,-11])  ## Calculate the correlation matrix
corrplot(corr.matrix, main="\nCorrelation Matrix")  ## Correlation Matrix

###### Skewness ######
skewValues <- apply(train.smote[,-11], 2, skewness);skewValues # skewness

###### Remove outliers #####
find.outlier = which(outlier(train.smote[,-11],logical=T)==TRUE,arr.ind=TRUE)
train.new <- train.smote[-find.outlier[,1],]

###### Centering and Scaling #####
trans <- preProcess(train.smote,method = c("center", "scale")); trans
train.centered <- predict(trans,train.smote)
test.centered <- predict(trans, test.data)
####### PCA ########
train.pca <- prcomp(train.smote[,-11], center=TRUE, scale. = TRUE) 
plot(train.pca,typ="l",col="indianred",lwd=2)
train.pca$rotation
biplot(train.pca)
plot(train.pca$x[,1],train.pca$x[,2],
     col=c("seagreen","indianred")[unclass(train.smote[,11])],
     xlab="PC1",ylab="PC2",main="PC1 vs PC2")
