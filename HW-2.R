# Exercise 2
# Insurance Data: Download the dataset from HuskyCT
rm(list=ls())
insurance <- read.csv("insurance.csv", header = TRUE)
str(insurance)
insurance$sex <- as.factor(insurance$sex)
insurance$smoker <- as.factor(insurance$smoker)
insurance$region <- as.factor(insurance$region)

median(insurance$charges)
mean(insurance$charges)
# What can  you say when the mean is much larger than the median?

# Then the distribution is positively skewed. 

hist(insurance$charges)

# Create a factor variable "highcharge" which equals to 1 if charge > 10000 and 0 otherwise 
insurance$highcharge <- factor(ifelse(insurance$charges > 10000, 1, 0))

is.factor(insurance$highcharge)
table(insurance$highcharge)

# Logistic regression ############################################

glm.fit <- glm(highcharge ~ age + sex + bmi + smoker + region, family=binomial, data=insurance)

summary(glm.fit)              

# Randomly sample 1000 observations to construct your training dataset.
library(dplyr)

## 74.8% of sample size
smp_size <- floor(0.748 * nrow(insurance))

## Set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(insurance)), size = smp_size)

# Construct your train and test dataset based on the other observations 
# (i.e., observations that are not used to train your model) 

train <- insurance[train_ind, ]
test <- insurance[-train_ind, ]

# Estimate your logistic model using a training dataset.

glm.fit.train <- glm(highcharge ~ age + sex + bmi + smoker + region, family=binomial, data=train)

# Apply your trained model to the test observations and classify them.

glm.probs <- predict(object=glm.fit.train, newdata=test, type="response")  
glm.probs[1:10]

# What is the accuracy rate?

glm.pred <- rep("0",dim(test)[1])  
glm.pred[glm.probs>0.5] = "1"                                                     

table(glm.pred,test$highcharge)
mean(glm.pred==test$highcharge) 

# We get an accuracy score of 0.90


# decision boundary ####################################################
#  
# Use only two predictors: age and bmi.


glm.fit2 <- glm(highcharge ~ age +  bmi, family=binomial, data=insurance)
glm.fit2.train <- glm(highcharge ~ age + bmi, family=binomial, data=train)
glm.probs2 <- predict(object=glm.fit2.train, newdata=test, type="response")  

glm.pred2 <- rep("0",dim(test)[1])                                      
glm.pred2[glm.probs2>0.5] = "1"                                                     

table(glm.pred2,test$highcharge)
mean(glm.pred2==test$highcharge) 


# We get a number of 0.71 

#########
# Using the training dataset, draw the decision boundaries for 
# linear model and logistic model.

# Linear model 
lm.fit <- lm(highcharge ~ age + bmi, data=train)
lm.fit

lm.prob <- predict(lm.fit, train, type='response')
lm.pred <- rep('0', dim(train)[1])
lm.pred[lm.prob>0.5] = '1'

# Answer
par(mfrow=c(1,1), mar=c(4,4,2,0.5))

lpm <- lm(highcharge ~ age + bmi, data=train)
summary(lpm)
plot(test$age, test$bmi, pch=19, col="black")
db <- abline(a=(0.5-lpm$coefficients[1])/lpm$coefficients[3],
                b=-lpm$coefficients[2]/lpm$coefficients[3], lwd=3, col='red')

lpm.prob <- predict(lpm, newdata=test)
lpm.pred <- lpm.prob>0.5

points(test$age[lpm.pred==1],test$bmi[lpm.pred==1], pch=19, col='red')
points(test$age[test$highcharge==1],test$bmi[test$highcharge==1], pch=19, col='red')

table(lpm.pred, test[,"highcharge"])
accuracy.lpm <- mean(test[,"highcharge"] == lpm.pred)
accuracy.lpm



# 


# Age for Linear 
a <- rep("red", dim(test)[1])
a[lm.pred =='1'] <- 'blue'

plot(test$age, test$highcharge, col=a) + abline(lm.fit) 

# BMI for Linear
b <- rep("red", dim(test)[1])
b[lm.pred =='0'] <- 'red'


plot(test$bmi, test$highcharge, col=b) + abline(lm.fit)

# Logistic model  (Using glm.fit2.train from previous logistic model above)

glm.fit2.train <- glm(highcharge ~ age + bmi, family=binomial, data=train)
glm.probs2 <- predict(object=glm.fit2.train, newdata=test, type="response")  


# Age for Logistic
c <- rep("red",dim(test)[1])                                      
c[glm.probs2 == '1'] = "blue"   

plot(test$age, test$highcharge, col=c) + abline(glm.fit2.train)

# BMI for Logistic
d <- rep("red", dim(test)[1])
d[glm.probs2 == '0'] <- 'red'


plot(test$bmi, test$highcharge, col=d) + abline(glm.fit2.train)

# Use the decision boundary to visualize how your model works well 
# for classification with the test dataset. 


# Which is better between linear model and logistic model in this example?

# A logistic model is better to use than a linear model in this example as a logistic regression provides a more discrete output and classifies the problem that we are looking to solve.
# A linear model is more so useful to handle more of a continuous output. 
