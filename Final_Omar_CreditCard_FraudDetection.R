rm(list=ls())

library(ggplot2)

df <- read.csv("/Users/omaral-shammary/Downloads/creditcard.csv", header = TRUE)

dim(df)
str(df)

table(df$Class)

sum(df$Class == '0')
sum(df$Class == '1')

# Randomly sample 75% of observations to construct your training dataset.
library(dplyr)

## 75% of sample size
smp_size <- floor(0.75 * nrow(df))

## Set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(df)), size = smp_size)

# Construct your train and test dataset based on the other observations 
# (i.e., observations that are not used to train your model) 

train <- df[train_ind, ]
test <- df[-train_ind, ]

dim(train)
dim(test)



#fig(14, 8)
df %>%
  ggplot(aes(x = Time, fill = factor(Class))) + geom_histogram(bins = 100)+
  labs(x = 'Time in seconds since first transaction', y = 'No. of transactions') +
  ggtitle('Distribution of time of transaction by class') +
  facet_grid(Class ~ ., scales = 'free_y')

# Linear

lm.fit.train <- lm(Class~., data=train)
summary(lm.fit.train)

lm.predict <- predict(lm.fit.train, df, type='response')
table(df$Class, lm.predict > 0.5)

lm.pred <- rep("0",dim(test)[1])  
lm.pred[lm.predict>0.5] = "1"  
table(lm.pred)

library(pROC)
roc_lin <- roc(df$Class, lm.predict, plotit = TRUE)
aucl <- round(auc(df$Class, lm.predict, plotit = TRUE),4)
roc_lin

ggroc(roc_lin, color='orange', size=2) + 
  ggtitle(paste0('ROC Curve', '(AUC = ', aucl, ')')) + theme_minimal()

# Logistic regression #########
# Train the model for fitting
glm.fit.train <- glm(Class~.,  family=binomial, data=train)
summary(glm.fit.train)

# Predicting our trained model
glm.predict <- predict(glm.fit.train, df, type='response')
table(df$Class, glm.predict > 0.5)

glm.pred <- rep("0",dim(test)[1])  
glm.pred[glm.predict>0.5] = "1"  
table(glm.pred)

# Load necessary packages 
# library(ggplot2)
library(pROC)

# Define object to plot and calculate AUC
roc_log <- roc(df$Class, glm.predict, plotit = TRUE)
auc <- round(auc(df$Class, glm.predict, plotit = TRUE),4)
roc_log

# Create ROC plot for Logistic model 
ggroc(roc_log, color='steelblue', size=2) + 
  ggtitle(paste0('ROC Curve', '(AUC = ', auc, ')')) + theme_minimal()

# 98% accuracy 
###############
# Decision Tree

#install.packages('rpart')
#install.packages('rpart.plot')
#install.packages('randomForest')
#install.packages('caret')
library(caret)
library(rpart)
library(rpart.plot)

decisionTree_model <- rpart(Class ~ . , df, method = 'class')
predicted_val <- predict(decisionTree_model, df, type = 'class')
probability <- predict(decisionTree_model, df, type = 'prob')
rpart.plot(decisionTree_model)


tree.model <- rpart(Class ~ ., data = train, method = "class", minbucket = 20)
prp(tree.model) 


tree.predict <- predict(tree.model, df, type = "class")
tree.predict[1:10]
tree.predict

table(tree.predict)

table(tree.predict)
mean(tree.predict == df$Class)
# 100% accuracy

# RandomForest
library(randomForest)
rf.model <- randomForest(Class ~., data = train, ntree = 20, nodesize = 20)
forest.predict <- predict(rf.model, df)

table(forest.predict)
mean(forest.predict == df$Class)

plot(rf.model)
rf.model
varImpPlot(rf.model, color='black')

# GBM 
# install.packages('gbm')
library(gbm, quietly=TRUE)

# Get the time to train the GBM model
system.time(
  model_gbm <- gbm(Class ~.,
                    distribution = "bernoulli",  # Can simply change for Poisson distribution 
                    data = rbind(train, test),
                    n.trees = 500,
                    interaction.depth = 3,
                    n.minobsinnode = 100,
                    shrinkage = 0.01,
                    bag.fraction = 0.5,
                    train.fraction = nrow(train) / (nrow(train) + nrow(test))
  )
)

gbm.iter = gbm.perf(model_gbm, method = "test")
plot(model_gbm, color = 'orange')

gbm.feature.imp = summary(model_gbm, n.trees = gbm.iter)

# AUC 
# install.packages('pROC')
library(pROC)

gbm_test = predict(model_gbm, newdata = test, n.trees = gbm.iter)
gbm_auc = roc(test$Class, gbm_test, plot = TRUE, col = "red")

print(gbm_auc)

# LDA
library(MASS)

lda.fit <- lda(Class~., data=train)
lda.fit
plot(lda.fit) # plots of the linear discriminant

lda.pred <- predict(lda.fit, test)
names(lda.pred)
lda.pred$class
lda.pred$posterior # P(Y|X)
lda.pred$x # linear discriminant

lda.class <- lda.pred$class
table(lda.class)
table(lda.class, test$Class)
mean(lda.class==test$Class)
mean(lda.class == df$Class)

# KNN
library(ISLR)
library(class)

dim(df)

head(df) # Characteristics for 284,807 individuals
attach(df)
summary(df)

st.X <- scale(df[,-31]) # scale() is a function for standarization
mean(st.X[,1])

table(Class)
boxplot(df[,1]~Class)
boxplot(df[,29]~Class)

train_index <- 3000:nrow(df)
train.X <- st.X[train_index,]
test.X <- st.X[-train_index,]
train.Y <- Class[train_index]
test.Y <- Class[-train_index]

knn.pred <- knn(data.frame(train.X), data.frame(test.X), cl=train.Y, k=1)

mean(test.Y != knn.pred)
# The error rate is 0.001%. It looks as our classification is very accurate

table(knn.pred, test.Y)
# We classify "1" very poorly. Only 1 out of 21 is correctly classified for this category

mean(test.Y != "1")
# 99.93% 

mean(knn.pred[test.Y=='1']=='1') # 0.5
mean(test.Y[knn.pred=='1']=='1') # 0.33 chance for correction

# When K = 5
knn.pred5 <- knn(data.frame(train.X), data.frame(test.X), cl=train.Y, k=5)

mean(test.Y != knn.pred5) 
# The error rate is 0.001%. It looks as our classification is very accurate

table(knn.pred5, test.Y)
# We classify "1" very poorly. Only 1 out of 21 is correctly classified for this category

mean(test.Y != "1")
# 99.93% 

mean(knn.pred5[test.Y=='1']=='1') # 0.5
mean(test.Y[knn.pred5=='1']=='1') # 0.33 chance for correction
