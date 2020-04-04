ggplot(titanic_data, aes(Survived)) +
  geom_histogram( stat="count")

# Create the barplot

ggplot(titanic_data , aes(x=Age, fill=Survived)) +
  geom_histogram(position="identity", alpha=0.8) +
  facet_grid(Sex ~ .)

ggplot(titanic_data , aes(x=Fare, fill=Survived)) +
  geom_histogram(position="identity", alpha=0.8) +
  facet_grid(Sex ~ .)

ggplot(titanic_data, aes(Sex, fill=Survived))+geom_bar()
ggplot(titanic_data, aes(Embarked, fill=Survived))+geom_bar()
ggplot(titanic_data, aes(Parch, fill=Survived))+geom_bar()
ggplot(titanic_data, aes(Pclass, fill=Survived))+geom_bar()
ggplot(titanic_data, aes(SibSp, fill=Survived))+geom_bar()
ggplot(titanic_data, aes(family_member_no, fill=Survived))+geom_bar()
ggplot(titanic_data%>%filter(Sex=="male"), aes(family_member_no, fill=Survived))+geom_bar()
ggplot(titanic_data%>%filter(Sex=="female"), aes(family_member_no, fill=Survived))+geom_bar()


#correlation between numerical
library("ggpubr")
ggscatter(train_X, x = "Age", y = "Fare", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Miles/(US) gallon", ylab = "Weight (1000 lbs)")

p <- ggplot(titanic_data, aes(Age, Pclass))
p + geom_boxplot()












knn_model <- function(train_X,labels,test_X,test_Y){
  k<- as.integer(sqrt(nrow(train_X)))
  knn_titanic <- class::knn(train_X, test_X, labels, k = k,  l = 0, prob = FALSE, use.all = TRUE)
  ##create the confucion matrix
  tab <- table(knn_titanic,test_Y)
  ##return check the accuracy
  return (sum(diag(tab)/(sum(rowSums(tab)))) * 100)
}
#knn model v2 + dodac normalizacje 
normalize <-function(x) { (x -min(x))/(max(x)-min(x))   }
train$Age <- normalize(train$Age)
train$Fare <- normalize(train$Fare)
#knn model
knn_accuracy2 <- knn_model(train[,-1],train$Survived,test[,-1],test$Survived)

#knn model v3 + bez family_member_no
data <- titanic_data %>% select(-family_member_no)
train <- subset(data, sample == TRUE)
test  = subset(data, sample == FALSE)
test <- na.omit(test)
train <-missing_values_imputation(train)
train <- remove_outliers(train, train$Age)
train <- train[,1:8]
knn_accuracy3 <- knn_model(train[,-1],train$Survived,test[,-1],test$Survived)

#knn model v3 + bez family_member_no
data <- titanic_data %>% select(-c(Parch,SibSp))
train <- subset(data, sample == TRUE)
test  = subset(data, sample == FALSE)
test <- na.omit(test)
train <-missing_values_imputation(train)
train <- remove_outliers(train, train$Age)
train <- train[,1:7]
knn_accuracy4 <- knn_model(train[,-1],train$Survived,test[,-1],test$Survived)
