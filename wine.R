setwd("~/Downloads")

# Set number of significant digits
options(digits = 3)

# install packages

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(ROCR)) install.packages("ROCR", repos = "http://cran.us.r-project.org")



#Load packages


library(ROCR)
library(corrplot)
library(caret)
library(tidyverse)

winequality.white <- read.csv("~/Downloads/winequality-white.csv", sep=";")

Whitewine<-winequality.white

View(Whitewine)

str(Whitewine)

head(Whitewine)
# 11 attributes are given and 12(quality) is dependent entity

# find if there are any null value in the dataset

sum(is.na(Whitewine))

# there are not null values in the dataset. 

# sort by quality

Whitewine%>%count(quality)

# there are very few entries in low and high quality

# correlation between all the attributes of the data set Whitewine


# correlation for all variables
str(Whitewine)

c<-cor(Whitewine)
      
corrplot(c)

# we can see that density is highly correlated with total sulfur dioxide and residual sugar. 
# total sulfur dioxide and free sulfur dioxide are also moderately correlated
# we will remove these attributes to solve the problem of multi-collinearity

wine<-Whitewine[,c(-4,-7,-8)]

head(wine)

#correlation between remaining attributes

c1<-cor(wine)

corrplot(c1)  


# Five number summary 
summary(wine)

# we can see from summary function above that attributes have different ranges
# to normalize the data  write a function normalize


normalize <- function(x) {
  return((x-min(x))/(max(x)-min(x)))
}

# scaling the 8 numeric attributes

Whitewine <- Whitewine %>%
  mutate_at(vars(-quality), .funs = normalize)
  
summary(Whitewine)
  
# divide dependent attribute quality into 2 levels so that we can use classification methods
# if quality is in 0 and 5 then it is "low"
# if it is in 6 and 9 then it is "high"

wine <- Whitewine[,c(-4,-7,-8)] %>% 
  mutate(quality = factor(case_when(
    quality %in% c(0:5) ~ "low",
    quality %in% c(6:9) ~ "high"),
    levels = c("low", "high")))


str(wine)

#Removing duplicate rows
#wine <-  unique(wine)

wine <- wine[!duplicated(wine), ]
dim(wine)

wine%>% group_by(quality)%>%count()


#Data Visualization

# bar graph of quality

  wine%>%ggplot(aes(quality)) +
  geom_bar(color="black", fill="blue")

 
# we will see effect of all attributes of quality of the wine using box plot.
# we have created another column called "quality" to reduce number of quality identifiers into 3
# we will use quality[with two levels: low and high] instead of original quality to make these boxplots

# Independent attributes behavior to define quality of the wine

#Fixed Acidity

ggplot(data=wine, aes(fixed.acidity)) +
  geom_histogram(binwidth = 0.2, color = 'black', fill = 'blue') +
  # Split data by category and display plots horizontally
  facet_wrap(~quality)

ggplot(data = wine, aes(x = quality, y = fixed.acidity, color = quality, group = quality )) +
  geom_boxplot()

#The low and high means are not far from each other and their IQRs overlap
#The variability between low and high quality categories is moderate 

# Volatile Acidity

ggplot(data =wine, aes(volatile.acidity)) +
  geom_histogram(binwidth = 0.2, color = 'black', fill = 'blue') +
  # Split data by category and display plots horizontally
  facet_wrap(~quality)

ggplot(data = wine, aes(x = quality, y = volatile.acidity, color = quality, group = quality )) +
  geom_boxplot()

# There is a slight variability between the low and high mean but not substantial

#Citric Acid

ggplot(data=wine, aes(citric.acid)) +
  geom_histogram(binwidth = 0.2, color = 'black', fill = 'blue') +
  # Split data by category and display plots horizontally
  facet_wrap(~quality)

ggplot(data = wine, aes(x = quality, y = citric.acid, color = quality, group = quality)) +
  geom_boxplot()



#Chlorides

ggplot(data = wine, aes(chlorides)) +
  geom_histogram(binwidth = 0.2, color= 'black', fill= 'blue') +
  facet_wrap(~quality)

ggplot(data = wine, aes(x = quality, y = chlorides, color = quality, group = quality )) +
  geom_boxplot()


#Free_Sulfur_Dioxide

ggplot(data = wine, aes(free.sulfur.dioxide)) +
  geom_histogram(binwidth = 0.2, color= 'black', fill= 'blue') +
  facet_wrap(~quality)

ggplot(data = wine, aes(x = quality, y = free.sulfur.dioxide, color = quality, group = quality )) +
  geom_boxplot()


#pH

ggplot(data = wine, aes(pH)) +
  geom_histogram(binwidth = 0.2, color= 'black', fill= 'blue') +
  facet_wrap(~quality)
ggplot(data = wine, aes(x = quality, y = pH, color = quality, group = quality )) +
  geom_boxplot()



#sulphates

ggplot(data = wine, aes(sulphates)) +
  geom_histogram(binwidth = 0.2, color= 'black', fill= 'blue') +
  facet_wrap(~quality)
ggplot(data = wine, aes(x = quality, y = sulphates, color = quality, group = quality )) +
  geom_boxplot()

 

#Alcohol

ggplot(data = wine, aes(alcohol)) +
  geom_histogram(binwidth = 0.2, color= 'black', fill= 'blue') +
  facet_wrap(~quality)
ggplot(data = wine, aes(x = quality, y = alcohol, color = quality, group = quality )) +
  geom_boxplot()



# Machine Learning Models


#choosing learning algorithm will depend on type of learning
#This is a supervised learning because dependent variable (quality) is given in the dataset

#we can see that this is an unbalanced data set
#to balance it we will use downSample function in caret package. it is called undersampling

wine2<-downSample(wine[,-9], wine$quality, yname = "quality")

wine2%>% group_by(quality)%>%count()

# divide data wine into test and training set
#set seed
set.seed(20)
y <- wine2$quality
test_index <- createDataPartition(y, times = 1, p = 0.2, list = FALSE)
train_set <- wine2 %>% slice(-test_index)
test_set <- wine2 %>% slice(test_index)

# a function that returns the accuracy of a confusion matrix

method_acc <- function(conf) {
  sum(diag(conf)) / sum(conf)
}

#Logistic Regression

# cross validation
control_specs<-trainControl(method = "CV", number = 20, savePredictions = "all", classProbs = TRUE)

#set random seed
set.seed(10)

# Logistic regression model

#estimate variable importance


glm_cv<-train(quality~ ., method = "glm", data = train_set, family = binomial, trControl= control_specs )


glm_pred<-predict(glm_cv, newdata = test_set)

#variable importance
imp<-varImp(glm_cv)
imp
plot(imp)


# create confusion matrix and find accuracy

glm_accu<-confusionMatrix(glm_pred, test_set$quality)$overall["Accuracy"]
glm_accu

# misclassification error
glm_error <- 1 - glm_accu
glm_error

#Model Performance Evaluation

#ROC and AUC

pred<-predict(glm_cv, test_set, type = 'prob')[,1]

#performance check

glm_pred2 <- prediction(pred, test_set$quality)
glm_perf <- performance(glm_pred2, measure = "tpr", x.measure = "fpr")
plot(glm_perf)

#ROC curve

glm_roc <-performance(glm_pred2,"tpr","fpr")
plot(glm_roc, colorize = T, lwd = 2)
abline(a = 0, b = 1) 


auc <- performance(glm_pred2, measure = "auc")
auc_glm <- auc@y.values[[1]]
auc_glm


Method_Accuracy <- (tibble(method="glm",  
                                    accuracy = glm_accu, error = glm_error, auc_value = auc_glm[[1]]))
Method_Accuracy


# Decision tree

 
set.seed(20)

tree_cv <- train(quality ~ .,
                 data = train_set,
                 method = "rpart",
                 tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 25)))
                 

ggplot(tree_cv)

plot(tree_cv$finalModel, margin = 0.1)
text(tree_cv$finalModel, cex = 0.75)

tree_pred<-predict(tree_cv, test_set)



# confusion matrix
tree_con <- table(pred = tree_pred, true = test_set$quality)
tree_con

#accuracy
tree_accuracy<-method_acc(tree_con)
tree_accuracy

# misclassification error
tree_error <- 1 - tree_accuracy
tree_error

#Model Performance Evaluation

#ROC and AUC

pred<-predict(tree_cv, test_set, type = 'prob')[,1]

#performance check

tree_pred = prediction(pred, test_set$quality)
tree_perf = performance(tree_pred, "acc")
plot(tree_perf)

#ROC curve

tree_roc <-performance(tree_pred,"tpr","fpr")
plot(tree_roc, colorize = T, lwd = 2)
abline(a = 0, b = 1) 



#AUC

#The AUC represents the area under the ROC curve.
#We can evaluate the model the performance by the value of AUC. 
#Higher than 0.5  shows a better model performance. 
#If the curve changes to rectangle it is perfect classifier with AUC value 1.

tree_auc <- performance(tree_pred, measure = "auc")
auc_tree<-tree_auc@y.values
auc_tree

Method_Accuracy <- bind_rows(Method_Accuracy,tibble(method="rpart",  
                                    accuracy = tree_accuracy, error = tree_error, auc_value = auc_tree[[1]]))
Method_Accuracy

#With an accuracy rate of 0.754, this decision tree model is not excellent

# the next model

#k-Nearest Neighbors (kNN)

library(class)

set.seed(20)

grid <- data.frame(k = seq(20, 52, 2))

knn_cv <- train(quality ~. -quality, method = "knn", data = train_set, tuneGrid = grid)

#plot knn_cv
ggplot(knn_cv)

knn_pred<-predict(knn_cv, test_set)

#accuracy
(knn_acc<-confusionMatrix(knn_pred, test_set$quality)$overall["Accuracy"])


# misclassification error
knn_err <- 1 - knn_acc
knn_err

pred<-predict(knn_cv, test_set, type = 'prob')[,1]

#performance check

knn_pred = prediction(pred, test_set$quality)
knn_perf = performance(knn_pred, "acc")
plot(knn_perf)

#ROC curve

knn_roc <-performance(knn_pred,"tpr","fpr")
plot(knn_roc, colorize = T, lwd = 2)
abline(a = 0, b = 1) 



#AUC


knn_auc <- performance(knn_pred, measure = "auc")
auc_knn<-knn_auc@y.values
auc_knn



Method_Accuracy <- bind_rows(Method_Accuracy,tibble(method="knn",  
                                    accuracy = knn_acc, error = knn_err, auc_value= auc_knn[[1]]))
Method_Accuracy


#Random Forest

library(randomForest)
set.seed(20)

nodesize <- seq(1, 71, 10)

rf_acc <- sapply(nodesize, function(ns){
  train(quality~ ., method = "rf", data = train_set,
        tuneGrid = data.frame(mtry = 3),
        nodesize = ns)$results$Accuracy
})

plot(nodesize, rf_acc)

rf_cv <- randomForest(quality ~ ., data=train_set,
                           nodesize = nodesize[which.max(rf_acc)])

# predict over test set

rf_pred<-predict(rf_cv, test_set)

rf_tab<-table(rf_pred, test_set$quality)
rf_tab
# Accuracy

rf_acc<-method_acc(rf_tab)
rf_acc

# misclassification error

rf_err <- 1 - rf_acc
rf_err

#ROC and AUC

# Building the ROC Curve
rf_pred <- as.data.frame(predict(rf_cv, newdata = test_set, type = "prob"))
rf_pred <- rf_pred[,1]

rf_roc_pred <- prediction(rf_pred, test_set$quality)
rf_perf <- performance(rf_roc_pred,
                        "tpr",
                        "fpr")

plot(rf_perf,colorize = T, lwd = 2)
abline(a=0, b=1)

#AUC
rf_auc <- performance(rf_roc_pred, measure = "auc")
auc_rf<-rf_auc@y.values
auc_rf


Method_Accuracy <- bind_rows(Method_Accuracy,tibble(method="RandomForest",
                                                    accuracy = rf_acc, error = rf_err, auc_value= auc_rf[[1]]))  
Method_Accuracy                                                                                      

#variable importance

varImpPlot(rf_cv)

# Modification in Random Forest based on variable importance "varImpPlot(rf_cv)"
# we can see from variable importance plot how important an attribute is in classification
# alcohol, volatile.acidity and free.sulfur.dioxide are three most important attributes
# we will consider on these three in next model (Random Forest)

# Random Forest with three predictors

set.seed(20)

rf_cv2 <- randomForest(
  formula = quality ~ alcohol + volatile.acidity + free.sulfur.dioxide,
  data = train_set, nodesize = nodesize[which.max(rf_acc)])
  
rf_pred2<-predict(rf_cv2, test_set)

rf_tab2<-table(rf_pred2, test_set$quality)

# Accuracy

rf_acc2<-method_acc(rf_tab2)
rf_acc2

# misclassification error

rf_err2 <- 1 - rf_acc2
rf_err2

# Building the ROC Curve
rf_pred2 <- as.data.frame(predict(rf_cv2, newdata = test_set, type = "prob"))
rf_pred2 <- rf_pred2[,1]

rf_roc_pred2 <- prediction(rf_pred2, test_set$quality)
rf_perf2 <- performance(rf_roc_pred2,
                       measure = "tpr",
                       x.measure = "fpr")

plot(rf_perf2,colorize = T, lwd = 2)
abline(a=0, b=1)

#AUC
rf_auc2 <- performance(rf_roc_pred, measure = "auc")
auc_rf2<-rf_auc2@y.values
auc_rf2


Method_Accuracy <- bind_rows(Method_Accuracy,tibble(method="RandomForestlimited",
                                                    accuracy = rf_acc2, error = rf_err2,auc_value= auc_rf2[[1]]))  
Method_Accuracy



#accuracy and AUC for all of the models

Method_Accuracy%>%ggplot(aes(accuracy, auc_value, col = method)) +
  geom_point(size = 3) + ggtitle("AUC vs Accuracy of Methods")

# Random Forest gives the best result

