##Creating a decision tree with rpart and rattle

library("rpart")
data <- read.csv("rev3.csv")
View(data)
str(data)
adm_data<-as.data.frame(data)
tree<- rpart(SL_YN ~ Temp + Calls + Talk_Time,
data=adm_data,
method="class")
plot(tree)
text(tree, pretty=0)


library(rattle)
rattle()
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(tree)
printcp(tree)
plotcp(tree)
ptree<- prune(tree,
cp= tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"])
fancyRpartPlot(ptree, uniform=TRUE,
cex=.7,
main="Will we meet SL goals?")
