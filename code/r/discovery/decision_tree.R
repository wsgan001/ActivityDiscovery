library(ISLR)
library(tree)

attach(Carseats)
High = ifelse(Sales>8, 'Yes', 'No')
carseats = data.frame(Carseats, High)
names(carseats)
carseats = carseats[, -1]
names(carseats)

set.seed(2)
train = sample(1:nrow(carseats), nrow(carseats)/2)
test = -train
training_data = carseats[train, ]
test_data = carseats[test,]
test_High = High[test]
tree_model = tree(High~., training_data)

plot(tree_model)
text(tree_model, pretty=0)
tree_pred = predict(tree_model, test_data, type='class')
mean(tree_pred != test_High)
#tree_pred = predict(tree_model, test_data, type='vector')

#prune the tree to avoid overfitting
#cross-validation on training set
set.seed(3)
cv_tree = cv.tree(tree_model, FUN=prune.misclass)
names(cv_tree)
plot(cv_tree$size, cv_tree$dev, type='b')

#prune the tree
pruned_model = prune.misclass(tree_model, best=9) 
plot(pruned_model)
text(pruned_model, pretty=0)
#set layer = 9 according to the result of cross-validation
# depth == 9 has the minimum cross validation error

# check how the pruned_model is doing
tree_pred = predict(pruned_model, test_data, type='class')
mean(tree_pred != test_High)


