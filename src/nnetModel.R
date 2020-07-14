#load workspace
setwd("C:/Users/12535/OneDrive - Queen's University/STAT457/final_project/input")
load("C:/Users/12535/OneDrive - Queen's University/STAT457/final_project/input/processed_text.RData")
source("C:/Users/12535/OneDrive - Queen's University/STAT457/final_project/src/preprocessing.R")
library(nnet)


?train = data.frame(Xtrain)
Y = factor(Ytrain)

nfold = 5
infold = sample(rep(1:nfold, length.out = nrow(train)))
allsize = seq(2,10,2)
errMat = matrix(NA,5,5)

for (f in 1:5){
  for (s in 1:5){
    cat("fold: ",f," size: ",s,"\n")
    nnet.fit = nnet(Y[infold != f]~.,
                    data = train[infold != f,],
                    size = allsize[s],
                    rang = 0.1,
                    decay =0.001,
                    maxit = 1000,
                    MaxNWts = 5000)
    
    nnet.pred = predict(nnet.fit,
                        train[infold == f,],
                        type = "class")
    errMat[s,f] = mean(nnet.pred != Y[infold == f])
  }
}


index = sample(1:nrow(Xtrain),round(0.9*nrow(Xtrain)))
t = train[index,]
test = train[-index,]
yt = Y[index]
nnet.fit = nnet(Y~.,data = train,size = 10,rang = 0.1,decay =0.0001,maxit = 1000,MaxNWts = 5000)
nnet.pred = predict(nnet.fit,test,type = "class")
table(nnet.pred,Y[-index])
mean(nnet.pred != Y[-index])
table(Y)

output(test_ID = testID,test_class = nnet.pred,filename = "nnetModel.csv")
