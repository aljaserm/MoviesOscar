movies <- read.csv("C:/Users/aljas/OneDrive/Documents/Development/R/MoviesWinOscar/MoviesOscar/Movie_classification.csv", header = TRUE)
# View(movies)

#Data Preprocessing
summary(movies)
movies$Time_taken[is.na(movies$Time_taken)] <- mean(movies$Time_taken,na.rm = TRUE)

# Test-Train Split
# install.packages('caTools')
library(caTools)
set.seed(0)
split =sample.split(movies,SplitRatio = 0.8)
trainc = subset(movies,split == TRUE)
testc = subset(movies,split == FALSE)

#install required packages
# install.packages('rpart')
# install.packages('rpart.plot')
library(rpart)
library(rpart.plot)

#Run Classification tree model on train set
classtree <- rpart(formula = Start_Tech_Oscar~., data = trainc, method = 'class', control = rpart.control(maxdepth = 3))
#press F1 on rpart for help on this function

#Plot the decision Tree
rpart.plot(classtree, box.palette="RdBu", digits = -3)

#Predict value at any point
testc$pred <- predict(classtree, testc, type = "class")

table(testc$Start_Tech_Oscar,testc$pred)


# Bagging
# install.packages('randomForest')
library (randomForest)
set.seed (0)

bagging =randomForest(formula = Collection~., data = trainc ,mtry=17)
testc$bagging <- predict(bagging, testc)
MSE2bagging <- mean((testc$bagging - testc$Collection)^2)


#Random forest
# install.packages('randomForest')
library(randomForest)

randomfor <- randomForest(Collection~., data = trainc,ntree=500)
#Predict Output 
testc$random <- predict(randomfor, testc)
MSE2random <- mean((testc$random - testc$Collection)^2)

#Boosting
# install.packages('gbm')
library (gbm)
set.seed (0)
boosting = gbm(Collection~., data = trainc, distribution="gaussian",n.trees =5000 , interaction.depth =4, shrinkage =0.2,verbose =F)
#distribution = 'Gaussian' for regression and 'Bernoulli' for classification
testc$boost = predict (boosting, testc, n.trees =5000)
MSE2boost <- mean((testc$boost - testc$Collection)^2)

# Adaboost

#install.packages("adabag")
library(adabag);

trainc$Start_Tech_Oscar <- as.factor(trainc$Start_Tech_Oscar)

adaboost <- boosting(Start_Tech_Oscar~., data=trainc, boos=TRUE,mfinal=10)

predada <- predict(adaboost,testc)
table(predada$class,testc$Start_Tech_Oscar)
70/113
77/113


#XGBOOST

# install.packages("xgboost")
library(xgboost)

trainY = trainc$Start_Tech_Oscar == "1"


trainX <- model.matrix(Start_Tech_Oscar ~ .-1, data = trainc)
trainX <- trainX[,-12]

testY = testc$Start_Tech_Oscar == "1"

testX <- model.matrix(Start_Tech_Oscar ~ .-1, data = testc)
testX <- testX[,-12]
#delete additional variable

Xmatrix <- xgb.DMatrix(data = trainX, label= trainY)
Xmatrix_t <- xgb.DMatrix(data = testX, label = testY)

Xgboosting <- xgboost(data = Xmatrix, # the data   
                      nround = 50, # max number of boosting iterations
                      objective = "multi:softmax",eta = 0.3, num_class = 2, max_depth = 10)

xgpred <- predict(Xgboosting, Xmatrix_t)
table(testY, xgpred)

74/113

t1<-adaboost$trees[[1]]
plot(t1)
text(t1,pretty=100)
