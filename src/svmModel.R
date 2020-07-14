#load workspace
setwd("C:/Users/12535/OneDrive - Queen's University/STAT457/final_project/input")
load("C:/Users/12535/OneDrive - Queen's University/STAT457/final_project/input/processed_text.RData")
source("C:/Users/12535/OneDrive - Queen's University/STAT457/final_project/src/preprocessing.R")
library(e1071)


train = data.frame(Xtrain)
test = data.frame(Xtest)
Y = factor(Ytrain)

cs = c(1,2,3,4,5)
cs = c(2.5,2.7,2.9,3.1,3.3,3.5)

errMatrix = matrix(NA,1,5)
index = sample(nrow(train),size = nrow(train)*0.9)
trainData = train[index,]
trainY = Y[index]
testData = train[-index,]
testY = Y[-index]
for(i in 1:5){
  cat("c = ",cs[i],"\n")
  svm.fit = svm(trainY~.,data = trainData,
                scale = TRUE,
                type = "C-classification",
                cost = cs[i])
  svm.pred = predict(svm.fit,testData,type = "class")
  errMatrix[i] = mean(svm.pred != testY)

}

errMat1to5 = errMatrix
errMat25to35 = errMatrix

#from errmat1to5 we can see when c = 3, it has lowest err
#from errmat2.5to3.5 we can see, when c = 3.5, it has lowest err.
#so pick c = 3.5


svm.fit = svm(Y~.,data = train,
              scale = TRUE,
              type = "C-classification",
              cost = 3.5)

svm.pred = predict(svm.fit,test,type = "class")



output(test_ID = testID,test_class = svm.pred,filename = " svmModel_c=3.5.csv")
