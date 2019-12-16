Thera_Bank_dataset=read.csv('Thera_Bank_dataset.csv',header = TRUE)
Thera_Bank_dataset=ba
Thera_Bank_dataset.scaled=scale(Thera_Bank_dataset[,3:9])
print(Thera_Bank_dataset.scaled)


 
##Check the dimension or shape of the data
dim(Thera_Bank_dataset)

##View top 5 rows
Thera_Bank_dataset[1:5,]

##Lets see the variable list in the data
names(Thera_Bank_dataset)

##Lets see the datatype of the data set. Can see several factor variables as factors
str(Thera_Bank_dataset)

##Convert all variables into factors where necessary. Use the dplyr package
library(dplyr)
Thera_Bank_dataset <- mutate_if(Thera_Bank_dataset, is.character, as.factor)
str(Thera_Bank_dataset)

#checking na values
is.na(Thera_Bank_dataset)

seed=1000
set.seed(seed) 

clust2=kmeans(x=Thera_Bank_dataset.scaled,centers=2,nstart=5)#5 times iteration will be performed
print(clust2)

library(cluster)
clusplot(Thera_Bank_dataset.scaled,clust2$cluster,color=TRUE,shade = TRUE,label = 2,lines = 1)

#to find right no. of cluster
totWss=rep(0,5)
for (k in 1:5) {
  set.seed(seed)
  clust=kmeans(x=Thera_Bank_dataset.scaled,centers=k,nstart=5)
  totWss[k]=clust$tot.withinss
}
print(totWss)
plot(c(1:5),totWss,type='b')#plot(x,y,type)


install.packages('NbClust')# to run multiple experiments together
library(NbClust)
set.seed(seed)
nc=NbClust(Thera_Bank_dataset[,3:9],min.nc=2,max.nc=8,method='kmeans')#nc means no. of clusters
table(nc$Best.n[1,])# votes for no. of cluster  

#now perform using 3 clusters
seed=1000
set.seed(seed)
clust3=kmeans(x=Thera_Bank_dataset.scaled,centers=3,nstart=5)#5 times iteration will be performed
print(clust3)
clusplot(Thera_Bank_dataset.scaled,clust3$cluster,color=TRUE,shade = TRUE,label = 2,lines = 1)


Thera_Bank_dataset$cluster=clust3$cluster
print(Thera_Bank_dataset)
custProfile=aggregate(Thera_Bank_dataset[,2:14],list(Thera_Bank_dataset$cluster),FUN='mean')
print(custProfile)


#cart/rf and PM

str(Thera_Bank_dataset)

##Convert all variables into factors where necessary. Use the dplyr package
library(dplyr)
Thera_Bank_dataset <- mutate_if(Thera_Bank_dataset, is.character, as.factor)
str(Thera_Bank_dataset)

##Converting target variable from Yes/No to 1/0 and convert it into factor
attach(Thera_Bank_dataset)
Personal.Loan = as.factor(Personal.Loan)
table(Thera_Bank_dataset$Personal.Loan)

## Employeecount, Over18 and StandardHours are variables where all the values are same.So we are not going to use these 3 variables.

##Summary Satatistics Measure of central tendency and dispersion (Univariate Analysis)
##(count,missing value,mean,0.01,0.05,0.10,0.25,Median,0.75,0.90,0.95,0.99,min,max,range,skew,kurtosis,SD,IQR) 
#for continous variable
library(psych)
describe(Thera_Bank_dataset[,2:14],na.rm = TRUE,quant = c(0.01,0.05,0.10,0.25,0.75,0.90,0.95,0.99),IQR=TRUE,check=TRUE)

##Frequency distribution for categorical variable (Univariate Analysis)
table(Thera_Bank_dataset[,c(10)])
table(Thera_Bank_dataset[,c(12)])
table(Thera_Bank_dataset[,c(13)])
table(Thera_Bank_dataset[,c(14)])
table(Thera_Bank_dataset[,c(15)])
table(Thera_Bank_dataset[,c(16)])
#the above relation shows that there is a relation between online transactions and personal loan

##correlation between continous variables (Bivariate Analysis)
library(DataExplorer)
plot_correlation(Thera_Bank_dataset[,2:9])

##Chi Square test for categorical Variable (Bivariate Analysis)
#The Chi Square statistic is commonly used for testing relationships between 
#categorical variables.  The null hypothesis of the Chi-Square test is that 
#no relationship exists on the categorical variables in the population; they are independent
chisq.test(unlist(Thera_Bank_dataset[,11]),unlist(Thera_Bank_dataset[,10]))
chisq.test(unlist(Thera_Bank_dataset[,11]),unlist(Thera_Bank_dataset[,12]))
chisq.test(unlist(Thera_Bank_dataset[,11]),unlist(Thera_Bank_dataset[,13]))
chisq.test(unlist(Thera_Bank_dataset[,11]),unlist(Thera_Bank_dataset[,14]))
chisq.test(unlist(Thera_Bank_dataset[,11]),unlist(Thera_Bank_dataset[,15]))
chisq.test(unlist(Thera_Bank_dataset[,11]),unlist(Thera_Bank_dataset[,16]))

#Use the unlist functions for all the other combinations of 
#categorical variables in your data set and then check the p-value


##We dont have any outlier in our data and we have used the above distribution to find out outliers

## But for a practice, we are mentioning below code for outlier treatment
##Applying flooring and capping on all continous variables. Here we are using 1 and 99 percentile for outlier treatment.
##Even you can take 5 and 95 percentile also for performing the outlier treatment

##HRData[,3] = ifelse(HRData[,3]<=quantile(HRData[,3],0.01),quantile(HRData[,3],0.01),HRData[,3])
##HRData[,3] = ifelse(HRData[,3]>=quantile(HRData[,3],0.99),quantile(HRData[,3],0.99),HRData[,3])

## removing unwanted variables. Employee Number, Employee Count, Over18 and Standard Hours
Thera_Bank_dataset = Thera_Bank_dataset[,-c(1,2)]

########################################################################################################################
##Build a decision tree using CART technique
########################################################################################################################

## Spliting the dataset into train and test for development and out of sample testing respectively


library(caTools)
Thera_Bank_dataset=ba
Thera_Bank_dataset=Thera_Bank_dataset[,-c(1,5)]
Thera_Bank_dataset$`Personal Loan`=as.factor(Thera_Bank_dataset$`Personal Loan`)
names(Thera_Bank_dataset)[8]="Personal.Loan"
names(Thera_Bank_dataset)[1]="Age"
names(Thera_Bank_dataset)[2]="Experience"
names(Thera_Bank_dataset)[3]="Income"
names(Thera_Bank_dataset)[4]="Family"
names(Thera_Bank_dataset)[9]="Security.Account"
names(Thera_Bank_dataset)[10]="CD.Account"
Thera_Bank_dataset=na.omit(Thera_Bank_dataset)
colSums(is.na(Thera_Bank_dataset))

set.seed(1234)
sample=sample.split(Thera_Bank_dataset,SplitRatio = 0.7)
#Use subset function to get the data that is TRUE for the training data set
CARTtrain = subset(Thera_Bank_dataset,sample == TRUE)

#Use subset function to get the data that is FALSE for the testing data set
CARTtest = subset(Thera_Bank_dataset,sample == FALSE)


## Calculate the response rate. To see number of actual employees that have left organization.
table(CARTtrain$Personal.Loan)
sum(CARTtrain$Personal.Loan== "1")/nrow(CARTtrain)

##Import rpart and rpart.plot library for creating CART model
library(rpart)
library(rpart.plot)

#Define the parameters
r.ctrl = rpart.control(minsplit=100, minbucket = 10, cp = 0, xval = 10)

#Building the CART model using the rpart function and the pre defined parameters
m1 <- rpart(formula = Personal.Loan~., data = CARTtrain, method = "class", control = r.ctrl)
m1

#Displaying the decision tree
install.packages('rattle')
library(rattle)
fancyRpartPlot(m1)

#Examine the complexity plot
printcp(m1)
plotcp(m1)

#Prune the tree at cp=0.077 and view it
m1_pruned <- prune(m1, cp = 0.040)
m1_pruned

#Display the new pruned tree
fancyRpartPlot(m1_pruned)

##Use this pruned tree to do the prediction on train as well as test data set
CARTtrain$CART.Pred = predict(m1_pruned,data=CARTtrain,type="class")
CARTtrain$CART.Score = predict(m1_pruned,data=CARTtrain,type="prob")[,"1"]
CARTtest$CART.Pred = predict(m1_pruned,CARTtest,type="class")
CARTtest$CART.Score = predict(m1_pruned,CARTtest,type="prob")[,"1"]

#Confusion matrix
## CART Model Confusion Metrix
CART_CM_train = table(CARTtrain$Personal.Loan,CARTtrain$CART.Pred)
CART_CM_train
CART_CM_test = table(CARTtest$Personal.Loan,CARTtest$CART.Pred)
CART_CM_test

## Misclassification Rate
(CART_CM_train[1,2]+CART_CM_train[2,1])/nrow(CARTtrain) #Misclassified in train
(CART_CM_test[1,2]+CART_CM_test[2,1])/nrow(CARTtest) #Misclassified in test

##Accuracy
(CART_CM_train[1,1]+CART_CM_train[2,2])/nrow(CARTtrain) #Correct classification in train
(CART_CM_test[1,1]+CART_CM_test[2,2])/nrow(CARTtest) #Correct classification in test

#Area under the ROC curve....check performance
install.packages('ROCR')
library(ROCR)
pred_dtrain <- prediction(CARTtrain$CART.Score, CARTtrain$Personal.Loan)
perf_dtrain <- performance(pred_dtrain, "tpr", "fpr")
plot(perf_dtrain,main = "ROC curve for train")

#Check area under the ROC curve for train
auc_train_dt <- performance(pred_dtrain,"auc")
auc_train_dt <- as.numeric(auc_train_dt@y.values)
auc_train_dt
pred_dtest <- prediction(CARTtest$CART.Score, CARTtest$Personal.Loan)
perf_dtest <- performance(pred_dtest, "tpr", "fpr")
plot(perf_dtest,main = "ROC curve for test")

#Check area under the ROC curve for test
auc_test_dt <- performance(pred_dtest,"auc")
auc_test_dt <- as.numeric(auc_test_dt@y.values)
auc_test_dt

#Gain chart
install.packages('gains')
library(gains)
gain_dtrain <- performance(pred_dtrain, "tpr", "rpp")
plot(gain_dtrain, col="orange", lwd=2)
lines(x=c(0, 0.5, 1), y=c(0, 1, 1), col="darkgreen", lwd=2)

gain_dtest <- performance(pred_dtest, "tpr", "rpp")
plot(gain_dtest, col="orange", lwd=2)
lines(x=c(0, 0.5, 1), y=c(0, 1, 1), col="darkgreen", lwd=2)

#Kolmogorov-Smirnov (KS) statistic and plot
ks_dtrain <- max(perf_dtrain@y.values[[1]]- perf_dtrain@x.values[[1]])
plot(perf_dtrain,main=paste0('KS=',round(ks_dtrain*100,1),'%'))
lines(x = c(0,1),y=c(0,1))

ks_dtest <- max(perf_dtest@y.values[[1]]- perf_dtest@x.values[[1]])
plot(perf_dtest,main=paste0('KS=',round(ks_dtest*100,1),'%'))
lines(x = c(0,1),y=c(0,1))

#Gini
install.packages("ineq")
library(ineq)
ineq(CARTtrain$CART.Score,"gini")
ineq(CARTtest$CART.Score,"gini")

#Concordance
install.packages("InformationValue")
library(InformationValue)
Concordance(actuals=CARTtrain$Personal.Loan,predictedScores = CARTtrain$CART.Score)
Concordance(actuals=CARTtest$Personal.Loan,predictedScores = CARTtest$CART.Score)

# Lift chart
install.packages("lift")
library(lift)

plotLift(CARTtrain$CART.Score,CARTtrain$Personal.Loan,cumulative = TRUE)
plotLift(CARTtest$CART.Score,CARTtest$Personal.Loan,cumulative = TRUE)


########################################################################################################################
##Build a Random Forest model
########################################################################################################################

## Spliting the dataset into train and test for development and out of sample testing respectively
set.seed(1234)
Thera_Bank_dataset=read.csv('Thera_Bank_data.csv',header = TRUE)
Thera_Bank_dataset=Thera_Bank_dataset[,-c(1,5)]
Thera_Bank_dataset$Personal_Loan=as.factor(Thera_Bank_dataset$Personal_Loan)
names(Thera_Bank_dataset)[8]="Personal.Loan"
names(Thera_Bank_dataset)[1]="Age"
names(Thera_Bank_dataset)[2]="Experience"
names(Thera_Bank_dataset)[3]="Income"
names(Thera_Bank_dataset)[4]="Family"
names(Thera_Bank_dataset)[9]="Security.Account"
names(Thera_Bank_dataset)[10]="CD.Account"
Thera_Bank_dataset=na.omit(Thera_Bank_dataset)
colSums(is.na(Thera_Bank_dataset))
sample1 = sample.split(Thera_Bank_dataset,SplitRatio = 0.7)

head(sample1)
str(Thera_Bank_dataset)
#Use subset function to get the data that is TRUE for the training data set
RFtrain = subset(Thera_Bank_dataset,sample1 == TRUE)
table(is.na(RFtrain))

#Use subset function to get the data that is FALSE for the testing data set
RFtest = subset(Thera_Bank_dataset,sample1 == FALSE)

##import randomForest library for building random forest model
install.packages("randomForest")
library(randomForest)

## set a seed to start the randomness
seed=1234
set.seed(seed)


##Build the first RF model
#Node size shall be ~2% of the population. This is similar to minbucket parameter in CART
Rforest=randomForest(Personal.Loan~.,data=RFtrain,mtry=5,nodesize=100,importance=T)
##Print the model to see the OOB and error rate
print(Rforest)

##Plot the RF to know the optimum number of trees
plot(Rforest)

##The above plot shows - Misclassification/error rate as a function of trees grown
#The black line represents the entire sample, 
#the green line represents the error rate 
#where Y = 0 and the red line represents the error rate when Y = 1.

##Identify the importance of the variables
importance(Rforest)

##Tune up the RF model to find out the best mtry
set.seed(seed)
tRforest = tuneRF(x=RFtrain[,-(8)],y=RFtrain$Personal.Loan,mtrystart = 6,stepfactor=1.5,ntree=51,improve=0.0001,
                  nodesize=10,trace=TRUE,plot=TRUE,doBest=TRUE,importance=TRUE)

plot(tRforest)
##Build the refined RF model

##Use this tree to do the prediction on train as well as test data set
RFtrain$RF.Pred = predict(tRforest,RFtrain,type="class")
RFtrain$RF.Score = predict(tRforest,RFtrain,type='prob')[,"1"]
RFtest$RF.Pred = predict(tRforest,RFtest,type="class")
RFtest$RF.Score = predict(tRforest,RFtest,type='prob')[,"1"]
head(RFtrain)
head(RFtest)
#Confusion matrix
## RF  Model Confusion Metrix
RF_CM_train = table(RFtrain$Personal.Loan,RFtrain$RF.Pred)
RF_CM_test = table(RFtest$Personal.Loan,RFtest$RF.Pred)
RF_CM_train
RF_CM_test
## Misclassification Rate
(RF_CM_train[1,2]+RF_CM_train[2,1])/nrow(RFtrain) #Misclassified in train
(RF_CM_test[1,2]+RF_CM_test[2,1])/nrow(RFtest) #Misclassified in test

##Accuracy
(RF_CM_train[1,1]+RF_CM_train[2,2])/nrow(RFtrain) #Correct classification in train
(RF_CM_test[1,1]+RF_CM_test[2,2])/nrow(RFtest) #Correct classification in test

#Check performance
library(ROCR)
pred_rftrain <- prediction(RFtrain$RF.Score,RFtrain$Personal.Loan)
perf_rftrain <- performance(pred_rftrain, "tpr", "fpr")
plot(perf_rftrain,main = "ROC curve for train")

pred_rftest <- prediction(RFtest$RF.Score,RFtest$Personal.Loan)
perf_rftest <- performance(pred_rftest, "tpr", "fpr")
plot(perf_rftrain,main = "ROC curve for test")

#Check area under the ROC curve
auc_train_rf <- performance(pred_rftrain,"auc"); 
auc_train_rf <- as.numeric(auc_train_rf@y.values)
auc_train_rf


#Check area under the ROC curve
auc_test_rf <- performance(pred_rftest,"auc") 
auc_test_rf <- as.numeric(auc_test_rf@y.values)
auc_test_rf
## List the importance of the variables.
impVar <- round(randomForest::importance(Rforest), 2)
impVar[order(impVar[,3], decreasing=TRUE),]

#Gain chart
gain_rftrain <- performance(pred_rftrain, "tpr", "rpp")
plot(gain_rftrain, col="orange", lwd=2)
lines(x=c(0, 0.5, 1), y=c(0, 1, 1), col="darkgreen", lwd=2)

gain_rftest <- performance(pred_rftest, "tpr", "rpp")
plot(gain_rftest, col="orange", lwd=2)
lines(x=c(0, 0.5, 1), y=c(0, 1, 1), col="darkgreen", lwd=2)

#Kolmogorov-Smirnov (KS) statistic and plot
ks_rftrain <- max(perf_rftrain@y.values[[1]]- perf_rftrain@x.values[[1]])
plot(perf_rftrain,main=paste0('KS=',round(ks_rftrain*100,1),'%'))
lines(x = c(0,1),y=c(0,1))

ks_rftest <- max(perf_rftest@y.values[[1]]- perf_rftest@x.values[[1]])
plot(perf_rftest,main=paste0('KS=',round(ks_rftest*100,1),'%'))
lines(x = c(0,1),y=c(0,1))

#Gini
RFtrain$RF.Score = predict(tRforest,RFtrain,type='prob')[,"1"]
#install.packages("ineq")
library(ineq)
ineq(RFtrain$RF.Score,"gini")
ineq(RFtest$RF.Score,"gini")

#Concordance
#install.packages("InformationValue")
library(InformationValue)
Concordance(actuals=RFtrain$Personal.Loan,predictedScores = RFtrain$RF.Score)
Concordance(actuals=RFtest$Personal.Loan,predictedScores = RFtest$RF.Score)

# Lift chart
install.packages("lift")
library(lift)

plotLift(RFtrain$RF.Score,RFtrain$Personal.Loan,cumulative = TRUE)
plotLift(RFtest$RF.Score,RFtest$Personal.Loan,cumulative = TRUE)
q()
