##Fitting Classification Tree Models

##package to fit decision trees
require(tree)

##SL dataset
mydata<- read.csv("SL_data.csv", header=T, sep=',')
View(mydata)

##Create categorical variables of SL >80%
High = ifelse(mydata$Service.Level >=80, "Yes", "No")

##Append "High" to mydata
mydata = data.frame(mydata, High)

##Remove "Service.Level" field
mydata = mydata[,-1]

##Split data into testing and training
set.seed(2)
train = sample(1:nrow(mydata), nrow(mydata)/2)
test = -train
training_data = mydata[train,]
testing_data = mydata[test,]
testing_High = High[test]

##fit the tree model using the training data
tree_model = tree(High~., training_data)

##plot the tree use pretty = 0 for categorical variable
plot(tree_model)
text(tree_model, pretty = 0)

##check the model using the test data
tree_pred = predict(tree_model, testing_data, type ="class")

mean(tree_pred != testing_High) #6.1%

##Prune the tree
##Run cross validation to check where to prune
set.seed(3)
cv_tree = cv.tree(tree_model, FUN = prune.misclass)
names(cv_tree)

##plot size vs error rate shows where to prune tree
plot(cv_tree$size, cv_tree$dev, type = "b")

##prune the tree - create model
pruned_model = prune.misclass(tree_model, best = 6)
plot(pruned_model)
text(pruned_model, pretty = 0, cex=0.7)
title(main="Will we meet Service Level?")

##check how model is doing - if pruned model better than unpruned
tree_pred = predict(pruned_model, testing_data, type = "class")
mean(tree_pred != testing_High)  #value compare to 6.1% above

