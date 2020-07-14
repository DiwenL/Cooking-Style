#load workspace
setwd("C:/Users/12535/OneDrive - Queen's University/STAT457/final_project/input")
load("processed_text.RData")
source("C:/Users/12535/OneDrive - Queen's University/STAT457/final_project/src/preprocessing.R")
library(randomForest)

Data = data.frame(Xtrain)
test = data.frame(Xtest)
Y = factor(Ytrain)

trees = c(100,150,200,250,300)
index = sample(nrow(Data),nrow(Data)*0.9)
trainData = Data[index,]
trainY = Y[index]
testData = Data[-index,]
testY = Y[-index]

errMatrix = matrix(NA,1,5)

for ( t in 1:5) {
  cat("tree = ",trees[t],"\n")
  rf.fit = randomForest(trainY~.,
                        data = trainData,
                        importance = T,
                        mtry = 5,
                        ntree = trees[t])
  rf.pred = predict(rf.fit,testData,type = "class")
  errMatrix[t] = mean(rf.pred != testY)
}
errMatrixNtree100_300 = errMatrix
#errMatrix shows ntree = 250 has the lowest err


ntrys = c(5,7,9,11,13,15)
errMatrixNtry5_15 = matrix(NA,1,6)

for (t in 1:6){
  cat("ntry = ",ntrys[t],"\n")
  rf.fit = randomForest(trainY~.,
                        data = trainData,
                        importance = T,
                        mtry = ntrys[t],
                        ntree = 250)
  rf.pred = predict(rf.fit,testData,type = "class")
  errMatrixNtry5_15[t] = mean(rf.pred != testY)
}


rfModel = randomForest(Y ~ ., 
                       data = Data, 
                       importance = T, 
                       mtry = 20, 
                       ntree=250 ) 
rf.pred = predict(rfModel,test)
output(testID,rf.pred,"rfModel_ntry20_ntree250.csv")
