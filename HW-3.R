## Exercise: Apply LDA to the insurance data and compare its performance with logistic regression.

insurance <- read.csv("insurance.csv", header = TRUE)

insurance$highcharge <- factor(ifelse(insurance$charges > 10000, 1, 0))

is.factor(insurance$highcharge)
table(insurance$highcharge)

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
# LDA
lda.fit <- lda(highcharge ~ age + sex + bmi + smoker + region, data=insurance) 
lda.fit
plot(lda.fit) # plots of the linear discriminant

lda.pred <- predict(lda.fit, test)
names(lda.pred)
lda.pred$class
lda.pred$posterior # P(Y|X)
lda.pred$x # linear discriminant


#par(mfrow=c(1,2))
a <- rep("red", dim(insurance)[1])
a[lda.pred$class=='1'] <- 'blue'
plot(insurance$age, insurance$bmi, col=a)

b <- rep("red", dim(insurance)[1])
b[insurance$highcharge=='1'] <- 'blue'
plot(insurance$age, insurance$bmi, col=b)

lda.class <- lda.pred$class
table(lda.class)
table(lda.class, test$highcharge)
mean(lda.class==test$highcharge)
# 91 percent

# Apply your trained model to the test observations and classify them.

glm.probs <- predict(object=glm.fit.train, newdata=test, type="response")  
glm.probs[1:10]

# What is the accuracy rate?

glm.pred <- rep("0",dim(test)[1])  
glm.pred[glm.probs>0.5] = "1"                                                     

table(glm.pred,test$highcharge)
mean(glm.pred==test$highcharge) 
# 90 percent 

# An LDA and Logisitc Regression show very similar results in terms of its accuracy, with LDA slightly...
# ... being more accuracte. 






