#Lakshman Arunachalam
# OMGT 5653 Introduction to Analytics Class
# University of Arkansas
# Script 5.2 for students
# Classification Trees
#
# This script follows Viswanathan Table 44 p. 207 for classification trees
# With additions for U-A


# Students, feel free to make modifications to this code as you like
# For example, you may want to print summary values of variables
# or the head or tail of a data frame.
# You are also required to add comments to this script

# Make sure we are in the repective workSpace directory (R:/WorkSpace)
#Modified setwd() because my personal WorkSpace is R:/WorkSpace and it is aleady in that workspace
#Use getwd() to ensure that you are in the required workspance
getwd()

#Getting the seed from the user.
seed <- readline("Please type in the random seed given by your homework: ")
seed <- ifelse(grepl("\\D",seed),-1,as.integer(seed))
if(is.na(seed)){break}  # breaks when hit enter

#Step-1 Read the boston housing data into the variable bh
bh <- read.csv("boston-housing-classification.csv") 

#Step-2 Load the caret library and set the seed required to the book
library(caret)
set.seed(seed)

#Step-3 partition the bh data into 70% of training data and 30% of test data 
sam <- createDataPartition(bh$MEDV_CAT, p = 0.7, list = FALSE)
train <- bh[sam, ]
test <- bh[-sam, ]

#Step-3 Load the rpart library inorder to create the classification Tree
library(rpart)

#Step-4 create the Classification tree and store it in bh.tree with minbucket as 10 so that each leaf node
#has more than 10 cases
bh.tree <- rpart(MEDV_CAT ~ ., data = train, control = rpart.control(minbucket = 10, cp = 0))

#Get to know about each variable, total cases.
summary(bh)

#Step-5 Load rpart.plot library to print the tree
library(rpart.plot)

#Step-6 Print the Tree in tree structure
prp(bh.tree, type = 2, extra = 104, 
    nn = TRUE, fallen.leaves = TRUE,
    faclen = 4, varlen = 8, 
    shadow.col = "gray")


#Step-7 Print the tree in textual format to get the actual numbers
bh.tree

#Step-8 Create a model prediction on training data
pred.train <- predict(bh.tree, train, type="class")

#Step-9 create a table (error matrix)
table(train$MEDV_CAT, pred.train, dnn = c("Actual", "Predicted"))


#Step-10 Generate model prediction test data
pred.test <- predict(bh.tree, test, type="class")

#Step-11 Create table for error matrix on test data
table(test$MEDV_CAT, pred.test, 
      dnn = c("Actual", "Predicted"))

#Step-12 Now create a classification tree with large size having minslpit as 10
bh.tree <- rpart(MEDV_CAT ~ ., data=train, control = rpart.control(minsplit = 10, cp=0))

#Step-13 Print the created tree
prp(bh.tree, type=2, extra=104, nn=TRUE, fallen.leaves = TRUE, faclen=4, varlen = 8, shadow.col = "gray")

#Step-14 Print the tree in text format to get more insights
bh.tree

#Step-15 Plot the cp table to decide the value of cp to be used for pruned tree
plotcp(bh.tree)

#Step-16 Prune and print the new tree with new value to cp as 0.01
bh.pruned <- prune(bh.tree, 0.01)
prp(bh.pruned, type=2, extra = 104, nn=TRUE, faclen = 4, varlen = 8, shadow.col = "gray")

#Step-17 Now create a prediction model for pruned tree with test data
pred.test <- predict(bh.pruned, test, type="class")

#Step-18 Create error matrix for the pruned tree
table(test$MEDV_CAT, pred.test, dnn = c("Actual", "Predicted"))
