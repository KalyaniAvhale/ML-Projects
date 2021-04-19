# @Script for Loan Eligibility Prediction Script  
# 
# @Author : Kalyani Avhale
# @Language : R
# @Dataset source : https://www.kaggle.com/vikasukani/loan-eligible-dataset
# @Date : 17th April , 2021

setwd("D:/Trisem_2/R/Project/packages")
#Install Required Packages

# install.packages('tidyverse') # metapackage of all tidyverse packages
# install.packages('dplyr')    #data manuplation(included in tidyverse)
# install.packages('caret')    #for Classification and regression 
# install.packages('ggpubr')   #arranging plots into grids
# install.packages('modeest')  #Estimation of the mode
# install.packages('ggplot2')  #for plotting graphs(included in tidyverse)
# install.packages('ggcorrplot')#for plotting correlation matrix
# install.packages('randomForest')#Random forest
# install.packages('xgboost')     #Gradient Boosting
##########################################################################
#import packages

library('tidyverse') 
library('caret')
library('ggpubr')
library('modeest')
library('ggcorrplot')
library('randomForest')
library('xgboost') 
##########################################################################
#directry for saving Plots
setwd("D:/Trisem_2/R/Project/DataSet")
##load data from CSV 
data=read.csv('loan-train.csv',na.strings=c(""))
head(data)

#Summarize Dataset

dim(data) #Dimension of dataset
#We have 614 instances and 12 Features

str(data) #returns type of attribute along with firstfew values

attr_type <- sapply(data,class) ## list types for each attribute 
print(data.frame(attr_type))

#Peek at the Data (overview few rows of dataset)
head(data)
tail(data)
#we can see 'Dependents' ha numerical values but its listed as character type
# we will preprocess it in later steps

#Levels of classes
unique(data$Loan_Status)
# Since we have 2 classes its a binary classification problem

#class distribution
percent = prop.table(table(data$Loan_Status))*100
cbind(freq=table(data$Loan_Status),percentage=percent)
#We see that only 31% of all the people in the dataset had a loan being approved. This means that our baseline model has an accuracy of 69%. An important measure when evaluating our model we be the sensitivity (aka recall aka the probability of detection as positive). If this value is low then our model is not very good at detecting true positive cases, even if the accuracy is very high. There are several ways to deal with imbalance


#Statistical Summary
summary(data)
# we can see few cols has NA's and the scale for variables differ 
# We can perform scaling in later steps
# We can see few outlier values 

#Pre-processing/Cleaning
#copy data so we don't modify original dataset we loaded
loan_dataset <- data
head(loan_dataset)

#check for duplicate rows
dim(loan_dataset[duplicated(loan_dataset$Loan_ID),])
#we have 0 duplicates across 13 columns for Loan_Id

#check for all unique values across dataset (probably for char type values)
Col_names=c("Gender","Married","Dependents","Education",
            "Self_Employed","Loan_Amount_Term",
            "Credit_History","Property_Area","Loan_Status")

lapply(loan_dataset[Col_names], function(x) unique(x))
# Dependents,Gender,Self_Employed has NA's and 3 suffixed with +
# Loan_Amount_Term,Credit_History has NA's

##check for null values count for numerical type cols
colSums(is.na(loan_dataset))
#Gender :13          Married :3       Dependents :15  Self_Employed :32  LoanAmount :22  Loan_Amount_Term :14   Credit_History :50

#replace 3+ with 3 
loan_dataset$Dependents <- replace(loan_dataset$Dependents,loan_dataset$Dependents=='3+',3)

###############################################################################

## handle NA values
data_beforeImputation <- loan_dataset
head(data_beforeImputation)#check

numeric_cols <- c('ApplicantIncome','CoapplicantIncome','LoanAmount','Loan_Amount_Term')#numerical cols
cat_cols <- c('Gender','Married','Dependents','Self_Employed','Credit_History')#categorical cols

#numeric NA : mean
data_beforeImputation[numeric_cols] <- sapply(data_beforeImputation[numeric_cols], function(x)ifelse(is.na(x), mean(x, na.rm=TRUE), x))
colSums(is.na(data_beforeImputation))

#categorical NA : fill with mode
data_beforeImputation[cat_cols] <- sapply(data_beforeImputation[cat_cols], function(x)ifelse(is.na(x), mfv(x), x))
colSums(is.na(data_beforeImputation))

data_afterImputation <- data_beforeImputation

######################################################################################

#EDA
setwd("D:/Trisem_2/R/Project/Plots")#derictory to save plots
#visualizaing categorical variables first with respect to Loan Status
ls_plt <- ggplot(data = data_afterImputation) +
  geom_bar(mapping = aes(x = Loan_Status,fill=Loan_Status)) 

g_plt <- ggplot(data = data_afterImputation) +
  geom_bar(mapping = aes(x = Gender,fill=Loan_Status)) 

m_plt <- ggplot(data = data_afterImputation) +
  geom_bar(mapping = aes(x = Married,fill=Loan_Status)) 

d_plt <- ggplot(data = data_afterImputation) +
  geom_bar(mapping = aes(x = Dependents,fill=Loan_Status)) 

e_plt <- ggplot(data = data_afterImputation) +
  geom_bar(mapping = aes(x = Education,fill=Loan_Status)) 

se_plt <- ggplot(data = data_afterImputation) +
  geom_bar(mapping = aes(x = Self_Employed,fill=Loan_Status)) 

ch_plt <- ggplot(data = data_afterImputation,aes(x=Credit_History,fill=Loan_Status)) +geom_bar() 

figure <- ggarrange(ls_plt,g_plt,m_plt,d_plt,e_plt,se_plt,ch_plt,nrow=3,ncol=3,common.legend=TRUE)
annotate_figure(figure,top = "Categorical Cols w.r.t Loan Status")
ggsave("Categorical_cols_plot.png")

# Inference :
#   
# Male applicant has high loan approval
# Applicant with 0 dependents has been approved with loan as compared to applicants with dependents
# Self_Employed applicants with loanapproval are low as commpared with which are not self employeed(can be with other profession such as jobs,business,etc)
# Applicant with Credit History has highest loan approval
# 

#Analyzing the three continuous variables w.r.t Loan_Status:
#ApplicantIncome,CoapplicantIncome,LoanAmount,Loan_Amount_Term
ai_plt <-  ggplot(data_afterImputation, aes(y= ApplicantIncome, x = "", fill = Loan_Status)) + geom_boxplot()
cai_plt <- ggplot(data_afterImputation, aes(y= CoapplicantIncome, x = "", fill = Loan_Status)) + geom_boxplot()
la_plt <-  ggplot(data_afterImputation, aes(y= LoanAmount, x = "", fill = Loan_Status)) + geom_boxplot()
lat_plt <- ggplot(data_afterImputation, aes(y= Loan_Amount_Term, x = "", fill = Loan_Status)) + geom_boxplot()

figure <- ggarrange(ai_plt,cai_plt,la_plt,lat_plt,nrow=2,ncol=2,common.legend=TRUE)
annotate_figure(figure,top = "Continuous Cols w.r.t Loan Status")
ggsave('Numerical_col_plot.png')

##Pair Plot
numeric_cols <- c('ApplicantIncome','CoapplicantIncome','LoanAmount','Loan_Amount_Term')
options(repr.plot.width =10, repr.plot.height = 10)             #adjust size of plot
pairs(data_afterImputation[numeric_cols], pch = 19,cex.labels=1.5,lower.panel=NULL)
#ggsave('Numerical_col_plot.png')

#each feature
png("LoanAmount.png")
options(repr.plot.width =10, repr.plot.height = 10)  #adjust size of plot
par(mfrow=c(2,2))  #arrange plot (matrix grid)

boxplot(data_afterImputation$LoanAmount,horizontal=TRUE,main="Loan Amount",col='red')
plot(density(data_afterImputation$LoanAmount),main="Density Graph",col='red')

boxplot(log(data_afterImputation$LoanAmount),horizontal=TRUE,main="Loan Amount after log",col='blue')
plot(density(log(data_afterImputation$LoanAmount)),main="Density Graph after log",col='blue')
#LoanAmount log(removed the skewness)
dev.off()


#ApplicantIncome
png("ApplicantIncome.png")
options(repr.plot.width =10, repr.plot.height = 10)  #adjust size of plot
par(mfrow=c(2,2))

boxplot(data_afterImputation$ApplicantIncome,horizontal=TRUE,main="Applicant Income",col='red')
plot(density(data_afterImputation$ApplicantIncome),main="Density Graph ",col='red')

boxplot(log(data_afterImputation$ApplicantIncome),horizontal=TRUE,main="Applicant Income after log",col='blue')
plot(density(log(data_afterImputation$ApplicantIncome)),main="Density Graph after log",col='blue')
dev.off()

#CoapplicantIncome
png("CoapplicantIncome.png")
options(repr.plot.width =10, repr.plot.height = 10)  #adjust size of plot
par(mfrow=c(2,2))

boxplot(data_afterImputation$CoapplicantIncome,horizontal=TRUE,main="Coapplicant Income",col='red')
plot(density(data_afterImputation$CoapplicantIncome),main="Density Graph ",col='red')

boxplot(log(data_afterImputation$CoapplicantIncome),horizontal=TRUE,main="Coapplicant Income after log",col='blue')
plot(density(log(data_afterImputation$CoapplicantIncome)),main="Density Graph after log",col='blue')
dev.off()

#Loan_Amount_Term
png("Loan_Amount_Term.png")
options(repr.plot.width =10, repr.plot.height = 10)  #adjust size of plot
par(mfrow=c(2,2))

boxplot(data_afterImputation$Loan_Amount_Term,horizontal=TRUE,main="Loan_Amount_Term",col='red')
plot(density(data_afterImputation$Loan_Amount_Term),main="Density Graph ",col='red')

boxplot(log(data_afterImputation$Loan_Amount_Term),horizontal=TRUE,main="Loan_Amount_Term after log",col='blue')
plot(density(log(data_afterImputation$Loan_Amount_Term)),main="Density Graph after log",col='blue')
dev.off()


#add log values to data
data_afterImputation$ApplicantIncome_log = log(data_afterImputation$ApplicantIncome)
data_afterImputation$LoanAmount_log = log(data_afterImputation$LoanAmount)
data_afterImputation$CoapplicantIncome_log = log(data_afterImputation$CoapplicantIncome)

#log(0) is -Inf , so replace -Inf to 0
data_afterImputation[data_afterImputation== -Inf]<-0


#lets check the covariance matrics
round(cor(data_afterImputation[numeric_cols]),3)
# we have +ve correlations : LoanAmount and ApplicantIncome and LoanAmount and coapplicationIncome
# As Loan Amount depends on Income of applicant ,the more the Income has high probablity of getting more Loan amount

#HAndle Ctegorical features
loanData <- subset(data_afterImputation,select = -c(ApplicantIncome,LoanAmount,CoapplicantIncome))

#Education
loanData$Education <- sapply(loanData$Education,function(x) ifelse(x=='Graduate',1,0)) #replace "Graduate" with 1 and "Not Graduate" with 0

#Loan_status
loanData$Loan_Status <- sapply(loanData$Loan_Status,function(x) ifelse(x=='Y',1,0))    

#Gender
loanData$Gender <- sapply(loanData$Gender,function(x) ifelse(x=='Male',1,0))   #replace "Y" with 1 and "N" with 0      

#Married
loanData$Married <- sapply(loanData$Married,function(x) ifelse(x=='Yes',1,0))   #replace "Y" with 1 and "N" with 0      

#Self_Employed
loanData$Self_Employed <- sapply(loanData$Self_Employed,function(x) ifelse(x=='Yes',1,0))   #replace "Y" with 1 and "N" with 0      

#Property_Area
loanData$Property_Area <- as.integer(factor(loanData$Property_Area))

#Credit History and Dependents convert to numeric
loanData$Credit_History<-as.integer(loanData$Credit_History)
loanData$Dependents<-as.integer(loanData$Dependents)

cleanData_bfscale <- subset(loanData,select = -c(Loan_ID,Loan_Status))

#Scaling 
af_scale <- data.frame(scale(cleanData_bfscale))

af_scale$Loan_Status <-loanData$Loan_Status


#co-relatirn Plot
options(repr.plot.width =20, repr.plot.height = 20)#adjust size of plot
loan_corr <- round(cor(af_scale),3) #get corr matrix
ggcorrplot(loan_corr,  title = "Correlation Plot",type = "lower",lab=TRUE,insig = "blank")#plot corr matrix to heatmap
#ggsave('Correlation_Plot.png')
# Inference :
#   
# Credit History has Positive correlation with target feature Loan Status
# Dependents and Married , Loan Amount and Applicant Income are positivly correlated
# Applicant Income and CoApplicant Income has negative correlation

#################################################################################################################################################################################


#Splitting the dataset into the Training set and Test set
set.seed(100) #randomization`

train_sample <- sample(nrow(af_scale), 0.75 * nrow(af_scale))
#splitting data into training/testing data using the trainIndex object
trainData <- af_scale[train_sample, ]  #training data (75% of data)
testData  <- af_scale[-train_sample, ] #testing data (25% of data)


# Check whether data set fairly even split
prop.table(table(trainData$Loan_Status))#train
prop.table(table(testData$Loan_Status))#test


#Model Building

#1. Logistic Regression
# glm() --> for generalized linear model and can be used to compute Logistic Regression
# fimaly = binomial is specified to perform binary classification
# Predictions can be easily made using the function predict(). Use the option type = “response” to directly obtain the probabilities
# 

# glm model with all Features
model_all <- glm(Loan_Status ~., data = trainData, family = binomial)
pred_all <- predict.glm(model_all,testData[-12],type = 'response')
pclass_all <- ifelse(pred_all<0.5,0,1)
confusionMatrix(table(as.factor(testData$Loan_Status),pclass_all),positive = '1')
# Confusion Matrix and Statistics
# 
# pclass_all
#     0   1
# 0  18  31
# 1   1 104
# 
# 
# Call:
#   glm(formula = Loan_Status ~ ., family = binomial, data = trainData)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.3655  -0.3957   0.5849   0.6894   2.4211  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)            0.84884    0.12720   6.673  2.5e-11 ***
#   Gender                -0.11211    0.13480  -0.832   0.4056    
# Married                0.06621    0.14596   0.454   0.6501    
# Dependents             0.03122    0.14039   0.222   0.8240    
# Education              0.23665    0.12295   1.925   0.0543 .  
# Self_Employed          0.06279    0.12545   0.500   0.6167    
# Loan_Amount_Term      -0.10278    0.14226  -0.723   0.4700    
# Credit_History         1.33221    0.15944   8.356  < 2e-16 ***
#   Property_Area          0.12935    0.12777   1.012   0.3113    
# ApplicantIncome_log    0.20152    0.16690   1.207   0.2273    
# LoanAmount_log        -0.26746    0.16693  -1.602   0.1091    
# CoapplicantIncome_log  0.24675    0.15325   1.610   0.1074    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 570.21  on 459  degrees of freedom
# Residual deviance: 422.25  on 448  degrees of freedom
# AIC: 446.25
# 
# Number of Fisher Scoring iterations: 5

# Features will minimum p value are good features
# glm model with single feature Credit_History
model1ch <- glm(Loan_Status ~Credit_History, data = trainData, family = binomial)
pred_m1 <- predict.glm(model1ch,testData[-12],type = 'response')
pclass_m1 <- ifelse(pred_m1<0.5,0,1)
confusionMatrix(table(as.factor(testData$Loan_Status),pclass_m1),positive = '1')
# Confusion Matrix and Statistics
# 
# pclass_m1
#     0   1
# 0  18  31
# 1   1 104
# Accuracy : 0.7922 
#Logistic curve

ggplot(data=trainData,aes(Credit_History, Loan_Status)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(
    title = "Logistic Regression Model", 
    x = "Credit Hisrtory",
    y = "Probability of getting Loan approved"
  )
ggsave('Logistic_curve.png')
#No improvement in model accuracy


#2.Random Forest
# Bagging stands for bootstrap aggregating. It consists of building multiple different decision tree models from a single training data set by repeatedly using multiple bootstrapped subsets of the data and averaging the models. Here, each tree is build independently to the others.
# Random Forest algorithm, is one of the most commonly used and the most powerful machine learning techniques. It is a special type of bagging applied to decision trees.

#random forest with default parameters and all features
set.seed(100)
original_rf<-randomForest(as.factor(Loan_Status)~ ., trainData,OOB=TRUE)
original_rf
# Confusion matrix:
#    0   1 class.error
# 0 66  77  0.53846154
# 1 16 301  0.05047319
##Train Accuracy : > (66+301)/(66+77+16+301) --> 0.7978261

pred <- predict(original_rf,testData[-12])
confusionMatrix(as.factor(testData$Loan_Status),pred)
# Confusion Matrix and Statistics
# 
# Reference
# Prediction   0   1
# 0  19  30
# 1   2 103
# 
# Accuracy : 0.7922  
#OR 
#accuracy
accuracy = sum(testData$Loan_Status == pred)/length(testData$Loan_Status)
accuracy #0.7922078

#no improvement observed as compared to glm model

# Hyperparameter tunning for Random Forest

set.seed(10)
tune_grid<-expand.grid(mtry=c(1:10), ntree=c(500,1000,1500,2000,2500,3000)) #expand a grid of parameters
mtry<-tune_grid[[1]]
ntree<-tune_grid[[2]] #using vectors instead of dataframe to subset is faster in for loop
OOB<-NULL #use to store calculated OOB error estimate

for(i in 1:nrow(tune_grid)){
  rf<-randomForest(as.factor(Loan_Status)~. ,trainData, mtry=mtry[i], ntree=ntree[i])
  confusion<-rf$confusion
  temp<-(confusion[2]+confusion[3])/614 #calculate the OOB error estimate
  OOB<-append(OOB,temp)
}

tune_grid$OOB<-OOB
head(tune_grid[order(tune_grid["OOB"]), ], 4) #order the results 
# mtry ntree       OOB
# 22    2  1500 0.1400651
# 52    2  3000 0.1400651
# 2     2   500 0.1416938
# 32    2  2000 0.1416938

#We have optimal paramater with lowest OOB score mtry:2 and ntree:1500
#fit model with optimal Parameters
final_rf <- randomForest(as.factor(Loan_Status)~. ,trainData, mtry=2, ntree=1500)
final_rf
# Confusion matrix:
# 0   1 class.error
# 0 67  76  0.53146853
# 1 24 293  0.07570978
## Train accuracy : (67+297)/460  ---> 0.7913043
#prediction
pred <- predict(final_rf,testData[-12])
confusionMatrix(as.factor(testData$Loan_Status),pred)
# Confusion Matrix and Statistics
# 
# Reference
# Prediction   0   1
# 0  19  30
# 1   4 101
# 
# Accuracy : 0.7792 

#Comparing the above random forest models : 
#model original_rf has better train accuracy but low test accuracy (its getting over fitted)
#model obtained with parameter tunning is the best fitted model as compared


# Important Features
png("FeatureImpPlot.png")
featureImp_plt <- varImpPlot(final_rf)
dev.off()
#Important Features are :
#Credit_History(strongest of all)     , ApplicantIncome_log   ,LoanAmount_log   , CoapplicantIncome_log 




#3.Gradient Boosting
set.seed(101)

xgmat <-data.matrix(trainData[-12]) #convert dataframe to matrix
xgmat_test <- data.matrix(testData[-12])

xgb <- xgboost(data=xgmat,label = trainData$Loan_Status,nrounds =100)
xgb
# evaluation_log:
#   iter train_rmse
# 1   0.422693
# 2   0.370246
# ---                
# 99   0.013028
# 100   0.012902
xgb_pred <- predict(xgb,newdata = xgmat_test)
xgb_pclass <- ifelse(xgb_pred<0.5,0,1)

confusionMatrix(table(as.factor(testData$Loan_Status),xgb_pclass),positive = '1')
# Confusion Matrix and Statistics
# 
# xgb_pclass
#    0  1
# 0 24 25
# 1 13 92
# 
# Accuracy : 0.7532          #                                           
# > (24+92)/154
# [1] 0.7532468


#Conclusion :

# This is the end of the analysis, we started from data cleaning and processing, missing value imputation , 
# then exploratory analysis and feature engineering, and finally model building and evaluation.
# The best accuracy we obtained on our validation data is  0.7792 ,Since dataset is not huge enough models performance does not vary but Randome forest performed 
# with hyperparameter tunning.
# The insights about loan approval status from the analyis is:
# 
# Applicants with credit history not passing guidelines mostly fails to get approved, probably because that they have a higher probability of not paying back.
# 
# Most of the time, applicants with high income, loaning low amount is more likely to get approved, which makes sense, those applicants are more likely to pay back their loans.
# 
# Having a strong coapplicant can be a plus to the probability of getting approve.
# 
# 








##### for submission 
setwd("D:/Trisem_2/R/Project/DataSet")
test_data=read.csv('loan-test.csv',na.strings=c(""))
View(test_data)

dim(test_data)
Col_names=c("Gender","Married","Dependents","Education",
            "Self_Employed","Loan_Amount_Term",
            "Credit_History","Property_Area")

lapply(test_data[Col_names], function(x) unique(x))
colSums(is.na(test_data))
test_data$Dependents <- replace(test_data$Dependents,test_data$Dependents=='3+',3)


numeric_cols <- c('LoanAmount','Loan_Amount_Term')#numerical cols
cat_cols <- c('Gender','Dependents','Self_Employed','Credit_History')#categorical cols

#numeric NA : mean
test_data[numeric_cols] <- sapply(test_data[numeric_cols], function(x)ifelse(is.na(x), mean(x, na.rm=TRUE), x))
colSums(is.na(test_data))

#categorical NA : fill with mode
test_data[cat_cols] <- sapply(test_data[cat_cols], function(x)ifelse(is.na(x), mfv(x), x))
colSums(is.na(test_data))


#add log values to data
test_data$ApplicantIncome_log = log(test_data$ApplicantIncome)
test_data$LoanAmount_log = log(test_data$LoanAmount)
test_data$CoapplicantIncome_log = log(test_data$CoapplicantIncome)

#log(0) is -Inf , so replace -Inf to 0
test_data[test_data== -Inf]<-0

#HAndle Ctegorical features
test_data <- subset(test_data,select = -c(ApplicantIncome,LoanAmount,CoapplicantIncome))

#Education
test_data$Education <- sapply(test_data$Education,function(x) ifelse(x=='Graduate',1,0)) #replace "Graduate" with 1 and "Not Graduate" with 0


#Gender
test_data$Gender <- sapply(test_data$Gender,function(x) ifelse(x=='Male',1,0))   #replace "Y" with 1 and "N" with 0      

#Married
test_data$Married <- sapply(test_data$Married,function(x) ifelse(x=='Yes',1,0))   #replace "Y" with 1 and "N" with 0      

#Self_Employed
test_data$Self_Employed <- sapply(test_data$Self_Employed,function(x) ifelse(x=='Yes',1,0))   #replace "Y" with 1 and "N" with 0      

#Property_Area
test_data$Property_Area <- as.integer(factor(test_data$Property_Area))

#Credit History and Dependents convert to numeric
test_data$Credit_History<-as.integer(test_data$Credit_History)
test_data$Dependents<-as.integer(test_data$Dependents)

cleanData_bfscale <- subset(test_data,select = -c(Loan_ID))

#Scaling 
af_scale <- data.frame(scale(cleanData_bfscale))

#prediction
predictions <- predict(final_rf,af_scale)
predClass <- ifelse(predictions ==1,'Y','N')
#submission
solution<-data.frame(Loan_ID=test_data$Loan_ID,Loan_Status=predClass) #predict the test set
write.csv(solution,"SolutionSubmission.csv",row.names=FALSE)
#Final Submission score 	0.770833333333333





