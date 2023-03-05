#https://raw.githubusercontent.com/palVJ/subSAGE/main/R/GenerateData.R
#Create data and compute the function

library(tidymodels)
library(randomForest)
library(glmnet)
library(corrplot)

gendata=function(n)
{
  x1 = rbinom(n = n, size = 2, prob = 0.4)
  x2 = rbinom(n = n, size = 2, prob = 0.04)
  x3 = rgamma(n = n,shape = 10, rate = 2)
  x4 = runif(n = n,min = 0, max = pi)
  x5 = rpois(n = n, lambda = 15)
  x6 = rnorm(n = n, mean = 0, sd = 10)
#add noise variables as normal distributions and binomials:
  means = runif(n = 47, min = -20, max = 20)
  sds = runif(n = 47, min = 1, max = 10)
  probs = runif(n = 47, min = 0.02, max = 0.5)
  noise = matrix(ncol = 94,nrow = n)
  colnames(noise) = paste("x",7:100,sep = "")
  for(i in 1:47){
    noise[,i] = rnorm(n = n, mean = means[i], sd = sds[i])
  }
  for(i in 48:94){
    noise[,i] = rbinom(n = n,size = 2, prob = probs[i-47])
  }
#The unknown function depends on x1,x2,x3,x4,x5 and x6. The rest do not matter.
#Add systematic error:
  error = rnorm(n = n,sd = 2)
  data = data.frame(x1,x2,x3,x4,x5,x6,noise,error)
  return(data)
}

yfunc = function(x){
  beta_0 = -0.5
  beta_1 = 0.03
  beta_2 = -0.05
  beta_12 = 0.3
  beta_3 = 0.02
  beta_4 = 0.35
  beta_5 = -0.2
  beta_6 = -1
  response =  beta_0 + beta_1*(x$x1) + beta_2*(x$x2) + beta_12*(x$x1)/(exp(-x$x2)) +
    beta_3*(x$x3)^2 + beta_4*sin(x$x4) +  beta_5*log(1+x$x5) + beta_6*(x$x5)*(x$x6 > 7) + x$error
return(response)
}

# running
set.seed(8701)
n=1000
part=0.2
# train+test
xs=gendata(n)
response = yfunc(x = xs)
data = cbind(xs[,-ncol(xs)],response)
colnames(data)

r = sample(1:nrow(data),round(nrow(data)*part),replace = FALSE)
test = data[r,]
train=data[-r,]

save(test,file = "test.RData")
save(train,file = "train.RData")

v=10
#trainv=sample(1:nrow(train),round(nrow(train)),replace=FALSE)
# not needed since already randomly sampled
idfold=rep(1:v,each=round(nrow(train)/10))


fit1=lm(response~.,data=train)
pred1=predict(fit1,newdata=train)
predtest1=predict(fit1,newdata=test)

cvpred1=rep(NA,length=nrow(train))
for (k in 1:v)
{
  fit1=lm(response~.,data=train[idfold!=k,])
  cvpred1[idfold==k]=predict(fit1,newdata=train[idfold==k,])
}

fit2=randomForest::randomForest(response~.,data=train) #mtry default
pred2=predict(fit2,newdata=train)
predtest2=predict(fit2,newdata=test)

cvpred2=rep(NA,length=nrow(train))
for (k in 1:v)
{
  fit2=randomForest::randomForest(response~.,data=train[idfold!=k,])
  cvpred2[idfold==k]=predict(fit2,newdata=train[idfold==k,])
}

fit=cv.glmnet(y=train$response,x=as.matrix(train[,-ncol(train)]))
fit$lambda.1se
fit3=glmnet(y=train$response,x=as.matrix(train[,-ncol(train)]),
            lambda=fit$lambda.1se)
pred3=predict(fit3,newx=as.matrix(train[,-ncol(train)]))
predtest3=predict(fit3,newx=as.matrix(test[,-ncol(test)]))

cvpred3=rep(NA,length=nrow(train))
for (k in 1:v)
{
  
fit=cv.glmnet(y=train$response,x=as.matrix(train[,-ncol(train)]))

fit3=glmnet(y=train$response,x=as.matrix(train[,-ncol(train)]),
            lambda=fit$lambda.1se)
pred3=predict(fit3,newx=as.matrix(train[,-ncol(train)]))


predx=data.frame("pred1"=pred1,"pred2"=pred2,"pred3"=unlist(pred3))
colnames(predx)[3]="pred3"
predtestx=data.frame(pred1=predtest1,pred2=predtest2,pred3=predtest3)
colnames(predtestx)[3]="pred3"

plot(predx$pred2,predx$pred3)

metaLS=lm(train$respons~pred1+pred2+pred3,data=predx)
predytest=predict(metaLS,newdata=predtestx)

mse=sum((test$response-predytest)^2)
mse
summary(metaLS)
