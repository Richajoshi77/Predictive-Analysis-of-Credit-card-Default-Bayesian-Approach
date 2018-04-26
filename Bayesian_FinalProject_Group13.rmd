---
# Bayesian Methods for Data Science (DATS 6450 - 11, Spring 2018)
# This is the code file for Bayesian Final Group project
# Data Science @ George Washington University
# title: Credit Card Default 
# Group #13: Richa, Hninn, Zhaoyang
# output: html_document
# date: April 13, 2018

---
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
# Importing the CSV file

dataset = read.csv("CreditCard.csv")
#colnames(dataset)
summary(dataset)
```

```{r}
# Change the column name PAY_0 to PAY_1

names(dataset)[names(dataset) == "PAY_0"] <- "PAY_1"
```

```{r}
# Replace Education level 0,5,6 to 4 and marriage 0 to 3

dataset$EDUCATION[dataset$EDUCATION==0]<- 4
dataset$EDUCATION[dataset$EDUCATION==5]<- 4
dataset$EDUCATION[dataset$EDUCATION==6]<- 4
dataset$MARRIAGE[dataset$MARRIAGE==0]<- 3
```

```{r}
# Replace all Pay delays (-2, -1 ) to 0 
#PAY_1 to PAY_6 are pay delays

dataset$PAY_1[dataset$PAY_1== -1]<-0
dataset$PAY_1[dataset$PAY_1== -2]<-0
dataset$PAY_2[dataset$PAY_2== -1]<-0
dataset$PAY_2[dataset$PAY_2== -2]<-0
dataset$PAY_3[dataset$PAY_3== -1]<-0
dataset$PAY_3[dataset$PAY_3== -2]<-0
dataset$PAY_4[dataset$PAY_4== -1]<-0
dataset$PAY_4[dataset$PAY_4== -2]<-0
dataset$PAY_5[dataset$PAY_5== -1]<-0
dataset$PAY_5[dataset$PAY_5== -2]<-0
dataset$PAY_6[dataset$PAY_6== -1]<-0
dataset$PAY_6[dataset$PAY_6== -2]<-0
```

```{r}
#The misclassified data was classified correctly and changed variables in factors variables

dataset$SEX<-as.factor(dataset$SEX)
dataset$EDUCATION<-as.factor(dataset$EDUCATION)
dataset$MARRIAGE<-as.factor(dataset$MARRIAGE)
dataset$default_payment_next_month<-as.factor(dataset$default_payment_next_month)
str(dataset)
```

```{r}
# Add new column for average delays of 6 months payment delays

dataset$AVG_DELAY = rowMeans(dataset[,c ("PAY_1", "PAY_2", "PAY_3", "PAY_4", "PAY_5", "PAY_6")], na.rm = TRUE)
View(dataset)
```

```{r}
#Checking to see how many number of row for default payment and no default payment in the dataset

default<-dataset[dataset$default_payment_next_month==1,]
no_default<-dataset[dataset$default_payment_next_month==0,]
nrow(default)
nrow(no_default)
```


```{r}

# Exploratoty data analysis (EDA)
# Load Packages

library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library(corrplot)
library(RColorBrewer)
library(gridExtra)

# ggplot of sex and credit limit balance in term of education
d1 <- ggplot(dataset, aes(SEX, (LIMIT_BAL/1000), fill=EDUCATION)) + 
  geom_boxplot() +
  xlab("Gender") + 
  ylab("BLimit(x1000 NT$)") + 
  scale_fill_brewer(palette = "Accent")

#ggplot of education and credit limit balance in term of gender
d2 <- ggplot (dataset, aes(EDUCATION, (LIMIT_BAL/1000), fill=SEX)) + 
  geom_boxplot() +
  xlab("Education") + 
  ylab("BLimit(x1000 NT$)") + 
  scale_fill_brewer(palette = "Paired")

#ggplot of education and credit limit balance in term of martial status
d3 <-ggplot(dataset, aes(EDUCATION, (LIMIT_BAL/1000), fill=MARRIAGE)) + 
  geom_boxplot() +
  xlab("Education") + 
  ylab("BLimit(x1000 NT$)") 

#combing above three ggplot in one output as grid
grid.arrange(d1, d2, d3)

#ggplot of martial status and credit limit balance in term of gender
ggplot(dataset, aes(MARRIAGE, (LIMIT_BAL/1000), fill=SEX)) + 
  geom_boxplot() +
  xlab("Marital Status") + 
  ylab("Balance Limit ( x 1000 NT$)") + 
  coord_cartesian(ylim = c(0,350)) +
  scale_fill_brewer(palette = "Paired")

```


```{r}
#Spliting data randomly into Training (70%) and Testing (30%) 
# install arm & caTools packages

library(arm)
library(caTools)
set.seed(123)
sample.train = sample.split(dataset, SplitRatio = 0.70)

dataset_train = subset(dataset, sample.train == TRUE)
dataset_test = subset(dataset, sample.train == FALSE)
```

```{r}
# GLM Model 1 Training

train_glm_model1 <- glm(default_payment_next_month~
                           LIMIT_BAL+
                           SEX+
                           EDUCATION+
                           MARRIAGE+
                           AGE+
                           AVG_DELAY+
                           AMT_Owed_1+
                           AMT_Owed_2+
                           AMT_Owed_3+
                           AMT_Owed_4+
                           AMT_Owed_5+
                           AMT_Owed_6+
                           Total_Amt_Owed+
                           AVG_DELAY,
                         family =binomial(link = "logit"),dataset_train)
summary(train_glm_model1) 

```

```{r}
# Run GLM Model 2 using significant variables
train_glm_model2 <- glm(default_payment_next_month~
                           LIMIT_BAL+
                           SEX+
                           EDUCATION+
                           AGE+
                           MARRIAGE+
                           AVG_DELAY+
                           AMT_Owed_2,
                         family =binomial(link = "logit"),dataset_train)
summary(train_glm_model2) 

```

```{r}
#Variable Importance for glm models

library(caret)
varImp(train_glm_model2)
```

```{r}
#Model Goodness of fit Homer-Lemeshow 
#p-values below 0.05 is a poor fit
#For classic GLM

library(MKmisc)
HLgof.test(fit = fitted(train_glm_model2), obs = dataset_train$default_payment_next_month)

library(ResourceSelection)
hoslem.test(dataset_train$default_payment_next_month, fitted(train_glm_model2), g=10)

# p-value here is < 2.2e-16  
```

```{r}
#Predict the train_glm_model2 in test data (dataset_test)

library(ROCR)
predict_glm_model <- predict(train_glm_model2, dataset_test, type = 'response')
predict_glm_model


library(arm)
library(caTools)
pr <- prediction(predict_glm_model, dataset_test$default_payment_next_month)

accuracy <- table(predict_glm_model, dataset_test[,"default_payment_next_month"])
sum(diag(accuracy))/sum(accuracy)

## Compute AUC for predicting Default payment with the variables 
library(pROC)
predict_glm_model_roc =roc(default_payment_next_month~predict_glm_model, data=dataset_test)
predict_glm_model_roc

plot(predict_glm_model_roc, col = "red")
#Area under the curve: 0.7341


#specify the the range for confusion matrix
predict_glm_model_cf <-ifelse(predict_glm_model > 0.1,1,0)
predict_glm_model_cf

#confusion matrix for glm model
conf_matrix_glm <-table(predict_glm_model_cf,dataset_test$default_payment_next_month)
conf_matrix_glm
```

```{r}
# Bayesian GLM model
# Bayesglm Model 1  
train_bayesglm_model1 <- bayesglm(default_payment_next_month~
                           LIMIT_BAL+
                           SEX+
                           EDUCATION+
                           MARRIAGE+
                           AGE+
                           AVG_DELAY+
                           AMT_Owed_1+
                           AMT_Owed_2+
                           AMT_Owed_3+
                           AMT_Owed_4+
                           AMT_Owed_5+
                           AMT_Owed_6+
                           Total_Amt_Owed+
                           AVG_DELAY,
                         family =binomial(link = "logit"),dataset_train)
summary(train_bayesglm_model1)
```

```{r}
# Bayesglm Model 2  

train_bayesglm_model2 <- glm(default_payment_next_month~
                           LIMIT_BAL+
                           SEX+
                           EDUCATION+
                           MARRIAGE+
                           AGE+
                           AVG_DELAY+
                           AMT_Owed_2,
                         family =binomial(link = "logit"),dataset_train)
summary(train_bayesglm_model2) 
```

```{r}
# checking variable importance for bayes glm model 2 

library(caret)
varImp(train_bayesglm_model2)
```

```{r}
#Model Goodness of fit Homer-Lemeshow 
#p-values below 0.05 is a poor fit
#For Bayes glm model 2

library(MKmisc)
HLgof.test(fit = fitted(train_bayesglm_model2), obs = dataset_train$default_payment_next_month)

library(ResourceSelection)
hoslem.test(dataset_train$default_payment_next_month, fitted(train_bayesglm_model2), g=10)
```


```{r}
#Prediction for the Bayes GLM model (train_glm_model2) on testing data (dataset_test)

library(ROCR)
predict_bayesglm_model <- predict(train_bayesglm_model2, dataset_test, type = 'response')
predict_bayesglm_model

pr <- prediction(predict_bayesglm_model, dataset_test$default_payment_next_month)

accuracy <- table(predict_bayesglm_model, dataset_test[,"default_payment_next_month"])
sum(diag(accuracy))/sum(accuracy)

# Compute AUC for predicting Default payment with the variables 
library(pROC)

predict_bayesglm_model_roc = roc(default_payment_next_month~predict_bayesglm_model, data=dataset_test)
predict_bayesglm_model_roc

plot(predict_bayesglm_model_roc, col = "red")

#Area under the curve: 0.7341

#specify the the range for confusion matrix
predict_bayesglm_model_cf <-ifelse(predict_bayesglm_model > 0.1,1,0)
predict_bayesglm_model_cf

#Confusion matrix for bayes glm model
conf_matrix_bayesglm<-table(predict_bayesglm_model_cf,dataset_test$default_payment_next_month)
conf_matrix_bayesglm

```


```{r}
#create new dataset to run faster in random forest
#variables that are used here are the same as both glm and bayglm model 2 

new_dataset <- dataset[,c("LIMIT_BAL", "SEX", "EDUCATION", "MARRIAGE", "AGE","AVG_DELAY","AMT_Owed_2", "default_payment_next_month")]
new_dataset
```


```{r}
# install arm and caTool packages

set.seed(123)
library(arm)
library(caTools)
sample.train2 = sample.split(new_dataset, SplitRatio = 0.70)

dataset_train2 = subset(new_dataset, sample.train == TRUE)
dataset_test2 = subset(new_dataset, sample.train == FALSE)
```


```{r}
#build Random Forest model

install.packages("randomForest")
library(randomForest)

# Create a Random Forest model with parameters
model_RF <- randomForest(default_payment_next_month ~ ., data = dataset_train2, importance = TRUE)
model_RF
```

```{r}
#variable importance of the first model

importance(model_RF)
varImpPlot(model_RF)
```

```{r}
#Now, we will use ‘for’ loop and check for different values of mtry.
# Using For loop to identify the right mtry for model

a=c()
i=1
for (i in 1:10) {
  model_RF2 <- randomForest(default_payment_next_month ~ ., data = dataset_train2, ntree = 500, mtry = i, importance = TRUE)
  predValid <- predict(model_RF2, dataset_test2, type = "class")
  a[i-1] = mean(predValid == dataset_test2$default_payment_next_month)
}

a
# We got mtry = 1 based on the a value.
# We got all possible 'a' values along with an error message, 'try with valid 'mtry', We looked it up and found that it is a default error message.

```

```{r}
# create the optimal random forest model with mtry = 1 which we get from for loop

model_RF_final <- randomForest(default_payment_next_month ~ ., data =  dataset_train2, ntree = 500, mtry = 1, importance = TRUE)
model_RF_final
```

```{r}
#variable importance of the second model

importance(model_RF_final)
varImpPlot(model_RF_final)
```

