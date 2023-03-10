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
  fit=cv.glmnet(y=train$response[idfold!=k],x=as.matrix(train[idfold!=k,-ncol(train)]))
  fit3=glmnet(y=train$response[idfold!=k],x=as.matrix(train[idfold!=k,-ncol(train)]),
            lambda=fit$lambda.1se)
  cvpred3[idfold==k]=predict(fit3,newx=as.matrix(train[idfold==k,-ncol(train)]))
}

predx=data.frame("pred1"=pred1,"pred2"=pred2,"pred3"=unlist(pred3))
colnames(predx)[3]="pred3"
predtestx=data.frame(pred1=predtest1,pred2=predtest2,pred3=predtest3)
colnames(predtestx)[3]="pred3"
cvpredx=data.frame(pred1=cvpred1,pred2=cvpred2,pred3=cvpred3)
colnames(cvpredx)[3]="pred3"

metaLSorg=lm(train$respons~pred1+pred2+pred3-1,data=predx)
predytest=predict(metaLSorg,newdata=predtestx)
metaLScv=lm(train$respons~pred1+pred2+pred3-1,data=cvpredx)
cvpredytest=predict(metaLScv,newdata=predtestx)

mseorg=sum((test$response-predytest)^2)
msecv=sum((test$response-cvpredytest)^2)
mse
msecv
summary(metaLS)

sum((train$response-predytest)^2)

# using automl - 
library(h2o)
h2o.init()

# Import a sample binary outcome train/test set into H2O
train <- h2o.importFile("https://s3.amazonaws.com/erin-data/higgs/higgs_train_10k.csv")
test <- h2o.importFile("https://s3.amazonaws.com/erin-data/higgs/higgs_test_5k.csv")

# Identify predictors and response
y <- "response"
x <- setdiff(names(train), y)

# For binary classification, response should be a factor
train[, y] <- as.factor(train[, y])
test[, y] <- as.factor(test[, y])

# Number of CV folds (to generate level-one data for stacking)
nfolds <- 5

# There are a few ways to assemble a list of models to stack toegether:
# 1. Train individual models and put them in a list
# 2. Train a grid of models
# 3. Train several grids of models
# Note: All base models must have the same cross-validation folds and
# the cross-validated predicted values must be kept.


# 1. Generate a 2-model ensemble (GBM + RF)

# Train & Cross-validate a GBM
my_gbm <- h2o.gbm(x = x,
                  y = y,
                  training_frame = train,
                  distribution = "bernoulli",
                  ntrees = 10,
                  max_depth = 3,
                  min_rows = 2,
                  learn_rate = 0.2,
                  nfolds = nfolds,
                  keep_cross_validation_predictions = TRUE,
                  seed = 1)

# Train & Cross-validate a RF
my_rf <- h2o.randomForest(x = x,
                          y = y,
                          training_frame = train,
                          ntrees = 50,
                          nfolds = nfolds,
                          keep_cross_validation_predictions = TRUE,
                          seed = 1)

# Train a stacked ensemble using the GBM and RF above
ensemble <- h2o.stackedEnsemble(x = x,
                                y = y,
                                training_frame = train,
                                base_models = list(my_gbm, my_rf))

# Eval ensemble performance on a test set
perf <- h2o.performance(ensemble, newdata = test)

# Compare to base learner performance on the test set
perf_gbm_test <- h2o.performance(my_gbm, newdata = test)
perf_rf_test <- h2o.performance(my_rf, newdata = test)
baselearner_best_auc_test <- max(h2o.auc(perf_gbm_test), h2o.auc(perf_rf_test))
ensemble_auc_test <- h2o.auc(perf)
print(sprintf("Best Base-learner Test AUC:  %s", baselearner_best_auc_test))
print(sprintf("Ensemble Test AUC:  %s", ensemble_auc_test))
# [1] "Best Base-learner Test AUC:  0.76979821502548"
# [1] "Ensemble Test AUC:  0.773501212640419"

# Generate predictions on a test set (if neccessary)
pred <- h2o.predict(ensemble, newdata = test)
head(pred)

# 2. Generate a random grid of models and stack them together

# GBM Hyperparamters
learn_rate_opt <- c(0.01, 0.03)
max_depth_opt <- c(3, 4, 5, 6, 9)
sample_rate_opt <- c(0.7, 0.8, 0.9, 1.0)
col_sample_rate_opt <- c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8)
hyper_params <- list(learn_rate = learn_rate_opt,
                     max_depth = max_depth_opt,
                     sample_rate = sample_rate_opt,
                     col_sample_rate = col_sample_rate_opt)

search_criteria <- list(strategy = "RandomDiscrete",
                        max_models = 3,
                        seed = 1)

gbm_grid <- h2o.grid(algorithm = "gbm",
                     grid_id = "gbm_grid_binomial",
                     x = x,
                     y = y,
                     training_frame = train,
                     ntrees = 10,
                     seed = 1,
                     nfolds = nfolds,
                     keep_cross_validation_predictions = TRUE,
                     hyper_params = hyper_params,
                     search_criteria = search_criteria)

# Train a stacked ensemble using the GBM grid
ensemble <- h2o.stackedEnsemble(x = x,
                                y = y,
                                training_frame = train,
                                base_models = gbm_grid@model_ids)

# Eval ensemble performance on a test set
perf <- h2o.performance(ensemble, newdata = test)

# Compare to base learner performance on the test set
.getauc <- function(mm) h2o.auc(h2o.performance(h2o.getModel(mm), newdata = test))
baselearner_aucs <- sapply(gbm_grid@model_ids, .getauc)
baselearner_best_auc_test <- max(baselearner_aucs)
ensemble_auc_test <- h2o.auc(perf)
print(sprintf("Best Base-learner Test AUC:  %s", baselearner_best_auc_test))
print(sprintf("Ensemble Test AUC:  %s", ensemble_auc_test))
# [1] "Best Base-learner Test AUC:  0.748146530400473"
# [1] "Ensemble Test AUC:  0.773501212640419"

# Generate predictions on a test set (if neccessary)
pred <- h2o.predict(ensemble, newdata = test)


# 2. Generate a random grid of models and stack them together

# GBM Hyperparamters
learn_rate_opt <- c(0.01, 0.03)
max_depth_opt <- c(3, 4, 5, 6, 9)
sample_rate_opt <- c(0.7, 0.8, 0.9, 1.0)
col_sample_rate_opt <- c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8)
hyper_params <- list(learn_rate = learn_rate_opt,
                     max_depth = max_depth_opt,
                     sample_rate = sample_rate_opt,
                     col_sample_rate = col_sample_rate_opt)

search_criteria <- list(strategy = "RandomDiscrete",
                        max_models = 3,
                        seed = 1)

gbm_grid <- h2o.grid(algorithm = "gbm",
                     grid_id = "gbm_grid_binomial",
                     x = x,
                     y = y,
                     training_frame = train,
                     ntrees = 10,
                     seed = 1,
                     nfolds = nfolds,
                     keep_cross_validation_predictions = TRUE,
                     hyper_params = hyper_params,
                     search_criteria = search_criteria)

# Train a stacked ensemble using the GBM grid
ensemble <- h2o.stackedEnsemble(x = x,
                                y = y,
                                training_frame = train,
                                base_models = gbm_grid@model_ids)

# Eval ensemble performance on a test set
perf <- h2o.performance(ensemble, newdata = test)

# Compare to base learner performance on the test set
.getauc <- function(mm) h2o.auc(h2o.performance(h2o.getModel(mm), newdata = test))
baselearner_aucs <- sapply(gbm_grid@model_ids, .getauc)
baselearner_best_auc_test <- max(baselearner_aucs)
ensemble_auc_test <- h2o.auc(perf)
print(sprintf("Best Base-learner Test AUC:  %s", baselearner_best_auc_test))
print(sprintf("Ensemble Test AUC:  %s", ensemble_auc_test))
# [1] "Best Base-learner Test AUC:  0.748146530400473"
# [1] "Ensemble Test AUC:  0.773501212640419"

# Generate predictions on a test set (if neccessary)
pred <- h2o.predict(ensemble, newdata = test)
