#load workspace
setwd("C:/Users/12535/OneDrive - Queen's University/STAT457/final_project/input")
load("processed_text.RData")
source("C:/Users/12535/OneDrive - Queen's University/STAT457/final_project/src/preprocessing.R")
library(kknn)
Data = data.frame(Xtrain)
test = data.frame(Xtest)

Y = factor(Ytrain)


#10 fold cross validation to find best k

nfold = 10
infold = sample(rep(1:nfold,length.out = length(Data)))
allk = c(10,20,30,50,100,150,200,250,500)
errMatrix = matrix(NA,length(allk),nfold)

for (f in 1:nfold){
  for (k in 1:length(allk)){
    cat("fold: ",f,"  k: ",k,"\n")
    knn.fit = kknn(Y[infold != f]~.,
                   train = Data[infold != f,],
                   test = Data[infold == f,],
                   k = allk[k])
    errMatrix[k,f] = mean(knn.fit$fitted.values != Y[infold == f])
  }
}

plot(rep(allk, nfold), as.vector(errMatrix), pch = 19, cex = 0.5, xlab = "k", ylab = "prediction error")
points(allk, apply(errMatrix, 1, mean), col = "red", pch = 19, type = "l", lwd = 3)


#from the graph we can see when k~(30,100),it has the lowest prediction err
#so doing another 10 fold cv with k ~(30,80)
nfold = 10
infold = sample(rep(1:nfold,length.out = length(Data)))
allk = seq(30,80,10)
errMatrix = matrix(NA,length(allk),nfold)

for (f in 1:nfold){
  for (k in 1:length(allk)){
    cat("fold: ",f,"  k: ",k,"\n")
    knn.fit = kknn(Y[infold != f]~.,
                   train = Data[infold != f,],
                   test = Data[infold == f,],
                   k = allk[k])
    errMatrix[k,f] = mean(knn.fit$fitted.values != Y[infold == f])
  }
}

plot(rep(allk, nfold), as.vector(errMatrix), pch = 19, cex = 0.5, xlab = "k", ylab = "prediction error")
points(allk, apply(errMatrix, 1, mean), col = "red", pch = 19, type = "l", lwd = 3)

#from the err matrix we can see when k ~ 50, we got the lowest prediction err.
train = data.frame(Xtrain)
test = data.frame(Xtest)
knn.fit = kknn(Y~.,train,test,k = 50)

output(testID,knn.fit$fitted.values,"knn_k50.csv")
