rm(list = ls()); gc()
setwd('/home/tapas/')
data = read.csv('cs-training.csv', header =T)
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
##Rowwise miss value
sort(sapply(data, function(x) { sum(is.na(x)) }), decreasing=TRUE)
####Plot missing value####
missmap(data, y.labels = NULL, y.at = NULL, col = c("#CC3399", "#6699FF"), main = "Missing values vs observed")
aggr(data,prop = F, numbers = T)
md.pattern(data)
matrixplot(data, interactive = F)
ggplot(data, aes(data$SeriousDlqin2yrs,fill=!is.na(data$MonthlyIncome))) + geom_bar(position="dodge") + labs(title="Monthly Income",fill="Has Monthly Income")
ggplot(data, aes(data$SeriousDlqin2yrs,fill=!is.na(data$NumberOfDependents))) + geom_bar(position="dodge") + labs(title="Number_of_dependent",fill="Has Dependent Income")


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

#Let’s test it out

ggplot_missing(data)


####Correltion plot####
corrplot(cor(data[complete.cases(data),]), method="circle", type = "lower", order="hclust",
         col=colorRampPalette(brewer.pal(11,"Spectral"))(8))
cor(data[complete.cases(data),])


####Heat map for correlation####
qplot(x=Var1, y=Var2, data=melt(cor(data[complete.cases(data),], use="p")), fill=value, geom="tile") +
  scale_fill_gradient2(limits=c(-1, 1))

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
