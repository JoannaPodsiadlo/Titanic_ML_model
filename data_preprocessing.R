if (!require("ggthemes")) install.packages("ggthemes")
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("caTools")) install.packages("caTools")
if (!require("VIM")) install.packages("VIM")
if (!require("randomForest")) install.packages("randomForest")
if (!require("pROC")) install.packages("pROC")
if (!require("gplots")) install.packages("gplots")


library(pROC)
require(class)
library("randomForest")
library(dplyr)
library(ggplot2)
library(VIM)
require(caTools)



#################################### FUNKCJE ##################
select_cols <- function(data)
{
  # delete embarked 2 missing values
  data <- data[!is.na(data$Embarked), ]
  data2 <- data %>% select(Pclass,Sex,SibSp,Parch,Embarked)
  #dummies <- fastDummies::dummy_cols(dummies) %>% select(-c(Parch,Pclass,Sex,SibSp,Embarked))
  data <- data %>% select(Survived, Age,Fare)
  #titanic_data <- data.frame(data,dummies)
  titanic_data <- data.frame(data,data2)
}
convert_types <- function(titanic_data){
  titanic_data$Survived<-as.factor(titanic_data$Survived)
  titanic_data$Fare <- as.double(titanic_data$Fare)
  titanic_data$Age <- as.double(titanic_data$Age)
  titanic_data$Pclass <- as.factor(titanic_data$Pclass)
  titanic_data["family_member_no"] <- (titanic_data$SibSp)+(titanic_data$Parch)
  titanic_data$SibSp <- as.factor(titanic_data$SibSp)
  titanic_data$Parch <- as.factor(titanic_data$Parch)
  titanic_data$family_member_no <- as.factor(titanic_data$family_member_no)
  return(titanic_data)
}

###################################################################
setwd("~/Titanic_projekt")
data <- read.csv2('titanic-passengers.csv', sep=";", na.strings = "")
titanic_data <- select_cols(data)
rm(data)
#for visuals with facotrs
titanic_data <- convert_types(titanic_data)

#zaleznosci 
library("gplots")
balloonplot(t(table(titanic_data$Survived, titanic_data$Sex) ), main ="Gender vs Survived", xlab ="", ylab="",
            label = TRUE, show.margins = TRUE)
balloonplot(t(table(titanic_data$Survived, titanic_data$SibSp) ), main ="SibSp vs Survived", xlab ="", ylab="",
            label = TRUE, show.margins = TRUE)
balloonplot(t(table(titanic_data$Sex, titanic_data$SibSp) ), main ="SibSp vs Sex", xlab ="", ylab="",
            label = TRUE, show.margins = TRUE)

#prepare data for KNN without factors
sapply(titanic_data,levels)
titanic_data$Sex <- sapply(as.character(titanic_data$Sex), switch, 'male' = 0, 'female' = 1)
levels(titanic_data$Survived) <- c(0,1)
titanic_data$Embarked[titanic_data$Embarked == ''] <- 'S'
titanic_data$Embarked <- sapply(as.character(titanic_data$Embarked), switch, 'C' = 0, 'Q' = 1, 'S' = 2)
titanic_data$SibSp <- as.numeric(titanic_data$SibSp)
titanic_data$Parch <- as.numeric(titanic_data$Parch)
titanic_data$Pclass <- as.numeric(titanic_data$Pclass)
titanic_data$family_member_no <- as.numeric(titanic_data$family_member_no)
sapply(titanic_data,class)

# Random sample indexes for knn
set.seed(101) 
sample = sample.split(titanic_data, SplitRatio = .8)
train = subset(titanic_data, sample == TRUE)
test  = subset(titanic_data, sample == FALSE)

#missing data using knn 
missing_values_imputation <- function(train){
  k<- as.integer(sqrt(nrow(train)))
  train <- kNN(train,  variable = colnames(train),  k = k)
  train <- train[,1:9]
  return (train)
}
train <-missing_values_imputation(train)
test <- na.omit(test)


#outliers
sapply(train,class)
p2 <- ggplot(data = train, aes(x=Sex, y=Age)) +
  scale_y_log10() +
  geom_point(aes(color=Sex), alpha=0.2, position='jitter') + 
  geom_boxplot(outlier.size=5, alpha=0.1)
plot(p2)
p2 <- ggplot(data = train, aes(x=Sex, y=Fare)) +
  scale_y_log10() +
  geom_point(aes(color=Sex), alpha=0.2, position='jitter') + 
  geom_boxplot(outlier.size=5, alpha=0.1)
plot(p2)


#outliers
remove_outliers <- function(data, colselect){
  Q <- quantile(colselect, probs=c(.25, .75), na.rm = FALSE)
  iqr <- IQR(colselect)
  up <-  Q[2]+1.5*iqr # Upper Range  
  low<- Q[1]-1.5*iqr # Lower Range
  eliminated<- subset(data, colselect > (Q[1] - 1.5*iqr) & colselect < (Q[2]+1.5*iqr))
  return (eliminated)
}
train <- remove_outliers(train, train$Age)


ggplot(data = train , aes(x=as.factor(Sex), y=Age)) +
  scale_y_log10() +
  geom_point(aes(color=Sex), alpha=0.2, position='jitter') + 
  geom_boxplot(outlier.size=5, alpha=0.1)
#brak outliers w fare 






write.csv(test,"test.csv")
write.csv(train,"test.csv")

#modele
set.seed(1234)
#knn model v1
knn_model <- function(train_X,labels,test_X,test_Y){
  k<- as.integer(sqrt(nrow(train_X)))
  knn_titanic <- class::knn(train_X, test_X, labels, k = k,  l = 0, prob = FALSE, use.all = TRUE)
  ##create the confucion matrix
  tab <- table(knn_titanic,test_Y)
  ##return check the accuracy
  return (sum(diag(tab)/(sum(rowSums(tab)))) * 100)
}

knn_accuracy1 <- knn_model(train[,-1],train$Survived,test[,-1],test$Survived)



# Do knn
k<- as.integer(sqrt(nrow(train[,-1])))
fit <- class::knn(train[,-1],test[,-1],train$Survived, k = k)
roc_obj <- roc(test$Survived, fit)
auc(roc_obj)



#random forest 
model <- randomForest(Survived ~ ., data=train, importance=TRUE,proximity=TRUE)
round(importance(model), 2)

titanic.mds <- cmdscale(1 - model$proximity, eig=TRUE)
op <- par(pty="s")
pairs(cbind(train[,-1], titanic.mds$points), cex=0.6, gap=0,
      col=c("red", "green")[as.numeric(test$Survived)],
      main="Titanic Data: Predictors and MDS of Proximity Based on RandomForest")
par(op)
print(titanic.mds$GOF)

set.seed(17)
titanic.urf <- randomForest(train[,-1])
MDSplot(titanic.urf, test$Survived)


