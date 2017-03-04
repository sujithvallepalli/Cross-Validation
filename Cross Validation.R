#####################################################################################

#This script can be used to perform cross validation and assess the model accuracy of models(linear and non-linear)
#cross-validation = 5
k = 5
data=dataset_name # replace the dataset_name with the data for which model is to be built.
####Also make sure the target variable is placed in 1st column of the dataset
#create id column to assing random values of 1 to 5 to each row
data$id <- sample(1:k, nrow(data), replace = TRUE)
list <- 1:k
# prediction and test set data frames that we add to with each iteration over the folds
prediction <- data.frame()
testsetCopy <- data.frame()
#function for K-fold
for(i in 1:k)
{
  # remove rows with id i from dataframe to create training set
  # select rows with id i to create test set
  trainingset <- subset(data, id %in% list[-1])
  testset <- subset(data, id %in% c(1))
  #run a the model
  attach(trainingset)
  mymodel <- formula #place your model equation with dat = trainingset
  detach(trainingset)
  #remove the prediction variable from test data set
  temp <- as.data.frame(predict(mymodel, testset[,-1]))
  #append the prediction to test dataset
  prediction <- rbind(prediction, temp)
  #str(prediction)
  #append this iteration's prediction to testcopy Autoset
  testsetCopy <- rbind(testsetCopy, as.data.frame(testset[,1]))
  #str(testsetCopy)
}
# add predictions and actual mpg values
result <- cbind(prediction, testsetCopy[, 1])
names(result) <- c("Predicted", "Actual")

#Modelmetric function is used to calculate model performance stats (Run the function, which is placed at end of the script before calling it)
#Call the modelmetrics functon to assess the model performance by calculating MAD RMSE MAPE MPSE R2 TMAD P90
modelmetrics(result$Actual,result$Predicted)


##########################################################################################################
#This function calculates the numeric model metrics like MAD, MSE, MAPE R2.
#a = vector of actual values
#m = vector of predicted values
modelmetrics=function(a,m)
{
  metrics=c(MAD=0,RMSE=0,MAPE=0,MPSE=0,R2=0,TMAD=0,P90=0)
  metrics["MAD"]=mean(abs(a-m))
  metrics["RMSE"]=sqrt(mean((a-m)^2))
  metrics["MAPE"]=mean(abs((a-m)/a))
  metrics["MPSE"]= mean(((a-m)/a)^2)
  SST= sum((a-mean(a))^2)
  SSE=sum((a-m)^2)
  metrics["R2"]=1-(SSE/SST)
  metrics["TMAD"]=mean(abs(a-m),trim = 0.05)
  metrics["P90"]=quantile(abs(a-m),probs = 0.9)
  return(metrics)
}

####Example to compute numeric model metrics
#NumericPred is the dataset which also contains predictions of 2 models
#Call the function
modelmetrics(NumericPred$Target,NumericPred$Model1)
modelmetrics(NumericPred$Target,NumericPred$Model2)
modelmetrics(m=NumericPred$Model2,a=NumericPred$Target)

#This function calculates the binary model metrics
#a = vector of actual values
#m = vector of predicted values
#k= no. of predictors

binmodelmetrics=function(a,m,k=10)
{
  metrics = c(LL=0,AIC=0,BIC=0,R2=0)
  metrics["LL"]=sum(ifelse(a==1,log(m),log(1-m)))#Compute log-likelihood
  metrics["AIC"]=-2*metrics["LL"]+2*k
  metrics["BIC"]=-2*metrics["LL"]+2*k*log(length(a))
  SST= sum(a-mean(a))^2
  SSE=sum((a-m)^2)
  metrics["R2"]=1-(SSE/SST)
  return(metrics)
}

######This function builds a contingency table for binary model prediction
#a = vector of actual values
#m = vector of predicted values
#p=cut-off probability value
#inc = increment value to  change p values in iterative loop
#Confusion matrix gives TN FP FN TP

binresults=function(a,m,inc)
{
  mresult=data.frame()
  cutoffs=seq(min(m)+inc,max(m),inc)
  for (p in cutoffs) 
  {
    mnew = ifelse(m<p,0,1)
    contable=table(factor(mnew),factor(a))
    #vec=as.vector(table(factor(mnew),factor(a)))
    vec=c(p,as.vector(table(factor(mnew),factor(a))))
    mresult=rbind(mresult,vec)
  }
  colnames(mresult)=c("Cut-off","TN","FP","FN","TP")
  return(mresult)
}

#Call the function
#BinaryPred is the dataset which also contains model predictions
x=binresults(BinaryPred$Target,BinaryPred$Model1,0.05)

###Plot ROC

fp1=x$FP/max(x$FP)
tp1=x$TP/max(x$TP)
plot(fp1,tp1,col="blue")

###Area Under the curve
AUC = mean(tp1)
AUC


