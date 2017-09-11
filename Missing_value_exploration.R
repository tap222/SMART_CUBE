rm(list = ls()); gc()
setwd('/home/tapas/')
data = read.csv('cs-training.csv', header =T)
data = read.csv('cs-test.csv', header =T)
data = data[ ,-1]

library(VIM)
library(mice)
library(corrplot)
library(doMC)
library(pROC)
library(mice)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(psych)
library(ggpubr)
library(reshape2)
library(Amelia) ##missmap

###Data Summary####
describe(data)
str(data)
describe(test)
str(test)
##Rowwise miss value Train
sort(sapply(data, function(x) { sum(is.na(x)) }), decreasing=TRUE)
####Plot missing value train####
missmap(data, y.labels = NULL, y.at = NULL, col = c("#CC3399", "#6699FF"), main = "Missing values vs observed")
aggr(data,prop = F, numbers = T)
md.pattern(data)
matrixplot(data, interactive = F)
ggplot(data, aes(data$SeriousDlqin2yrs,fill=!is.na(data$MonthlyIncome))) + geom_bar(position="dodge") + labs(title="Monthly Income",fill="Has Monthly Income")
ggplot(data, aes(data$SeriousDlqin2yrs,fill=!is.na(data$NumberOfDependents))) + geom_bar(position="dodge") + labs(title="Number_of_dependent",fill="Has Dependent Income")
##Rowwise miss value Test
sort(sapply(data, function(x) { sum(is.na(x)) }), decreasing=TRUE)
####Plot missing value test####
missmap(test, y.labels = NULL, y.at = NULL, col = c("#CC3399", "#6699FF"), main = "Missing values vs observed")
aggr(test,prop = F, numbers = T)
md.pattern(test)
matrixplot(test, interactive = F)

library(reshape2)
library(ggplot2)

library(reshape2)
library(ggplot2)

ggplot_missing <- function(x){
  
  x %>% 
    is.na %>%
    melt %>%
    ggplot(data = .,
           aes(x = X2,
               y = X1)) +
    geom_raster(aes(fill = value)) +
    scale_fill_grey(name = "",
                    labels = c("Present","Missing")) +
    theme_minimal() + 
    theme(axis.text.x  = element_text(angle=45, vjust=0.5)) + 
    labs(x = "Variables in Dataset",
         y = "Rows / observations")
}

#Implementing function
ggplot_missing(data)
ggplot_missing(test)
####Correltion plot Train####
corrplot(cor(data[complete.cases(data),]), method="circle", type = "lower", order="hclust",
         col=colorRampPalette(brewer.pal(11,"Spectral"))(8))
cor(data[complete.cases(data),])
####Correltion plot####
corrplot(cor(test[complete.cases(test),]), method="circle", type = "lower", order="hclust",
         col=colorRampPalette(brewer.pal(11,"Spectral"))(8))
cor(data[complete.cases(test),])


####Heat map for correlation Train####
qplot(x=Var1, y=Var2, data=melt(cor(data[complete.cases(data),], use="p")), fill=value, geom="tile") +
  scale_fill_gradient2(limits=c(-1, 1))
####Heat map for correlation Test####
qplot(x=Var1, y=Var2, data=melt(cor(test[complete.cases(test),], use="p")), fill=value, geom="tile") +
  scale_fill_gradient2(limits=c(-1, 1))
