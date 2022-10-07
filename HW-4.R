# Exercise: Apply KNN to the insurance data and compare its performance with logistic regression and LDA
# Use the age, sex, bmi, and smoker variables as regressors
# For comparison, split the data set into training and test sets as follows.
# Check the performance of KNN with different K's

library(class)

insurance <- read.csv("insurance.csv", header = TRUE)
attach(insurance)

# Summarization of selected regressors 
summary(age)
summary(sex)
summary(bmi)
summary(smoker)

# Mean of selected regressors 
mean(age)
mean(sex == 'male')
mean(sex == 'female')
mean(bmi)
mean(smoker == 'yes') # 20% 
mean(smoker == 'no') # 80% 

# Our regressors 
insurance$charges <- factor(ifelse(insurance$charges > 10000, 1, 0))
insurance$sex <- factor(ifelse(insurance$sex == 'male', 1, 0))
insurance$smoker <- factor(ifelse(insurance$smoker == 'yes', 1, 0))

as.numeric(insurance$sex)
as.numeric(insurance$charges)
as.numeric(insurance$smoker)
as.numeric(insurance$bmi)
as.numeric(insurance$charges)

table(insurance$charges)
table(insurance$sex)
table(insurance$smoker)


# Randomly sample 1000 observations to construct your training dataset.
library(dplyr)

## 74.8% of sample size
smp_size <- floor(0.7485 * nrow(insurance))

## Set the seed to make your partition reproducible
#set.seed(123)
train_ind <- sample(seq_len(nrow(insurance)) , size = smp_size)

train <- insurance[train_ind, ]
test <- insurance[-train_ind, ]


st.X <- scale(insurance[,3, 1:3])
st.X
mean(st.X[,1])
var(st.X[,1])

train.X <- st.X[train_ind,]
test.X <- st.X[-train_ind,]

# Age
train.Y <- insurance$age[train_ind]
test.Y <- insurance$age[-train_ind]

# Smoker 
train.Y2 <- insurance$smoker[train_ind]
test.Y2 <- insurance$smoker[-train_ind]

# BMI 
train.Y3 <- insurance$bmi[train_ind]
test.Y3 <- insurance$bmi[-train_ind]

# Gender / Sex
train.Y4 <- insurance$sex[train_ind]
test.Y4 <- insurance$sex[-train_ind]

# Construct your train and test dataset based on the other observations 
# (i.e., observations that are not used to train your model) 

# Age #######

knn.pred.age <- knn(data.frame(train.X), data.frame(test.X), cl=train.Y, k=1)

mean(test.Y != knn.pred.age) # 0.98 error 
table(knn.pred.age, test.Y2)
mean(test.Y != '0') # 1 

mean(knn.pred.age[test.Y=='18']=='18') # 0.06
mean(test.Y[knn.pred.age=='18']=='18') # 0.06



knn.pred.age5 <- knn(data.frame(train.X), data.frame(test.X), cl=train.Y, k=5)

table(knn.pred.age5, test.Y)
mean(knn.pred.age5[test.Y=='18']=='18') # 0.05
mean(test.Y[knn.pred.age5=='18']=='18') # 0.06

# K shows litle significant change here 

# Smoking 
knn.pred.smoke <- knn(data.frame(train.X), data.frame(test.X), cl=train.Y2, k=1)

mean(test.Y2 != knn.pred.smoke) # 0.30 when k = 1
table(knn.pred.smoke, test.Y2)
mean(test.Y2 != '0') # 0.21 

mean(knn.pred.smoke[test.Y2=='1']=='1') # 0.11
mean(test.Y2[knn.pred.smoke=='1']=='1') # 0.17

knn.pred.smoke5 <- knn(data.frame(train.X), data.frame(test.X), cl=train.Y2, k=5)
knn.pred.smoke5

mean(test.Y2 != knn.pred.smoke5) # 0.22 when k = 5

# As we increase k, our error decreases 

# BMI 

knn.pred.bmi <- knn(data.frame(train.X), data.frame(test.X), cl=train.Y3, k=1)

mean(test.Y3 != knn.pred.bmi) # 0.26 when k = 1

table(knn.pred.bmi, test.Y3)
mean(test.Y3 != '0') # 1 

knn.pred.bmi5 <- knn(data.frame(train.X), data.frame(test.X), cl=train.Y3, k=5)
knn.pred.bmi5

mean(test.Y3 != knn.pred.bmi5) # 0.73 when k = 5

# As we increase k, our error increases. 

# Sex 

knn.pred.sex <- knn(data.frame(train.X), data.frame(test.X), cl=train.Y4, k=1)

mean(test.Y4 != knn.pred.sex) # 0.50
# Error rate is  for smokers
table(knn.pred.sex, test.Y4)
mean(test.Y4 != '0') # 19.2 error rate for smokers / 0 stands for no.

mean(knn.pred.sex[test.Y4=='1']=='1') # 0.13 
mean(test.Y4[knn.pred.sex=='1']=='1') # 0.17 

knn.pred.sex5 <- knn(data.frame(train.X), data.frame(test.X), cl=train.Y4, k=5)
knn.pred.sex5

mean(test.Y4 != knn.pred.sex5) # 0.50

# No signficant change when k is increased

# Logistic regression 

glm.fit.train <- glm(charges ~ age + sex + bmi + smoker, family=binomial, data=train)

glm.probs <- predict(object=glm.fit.train, newdata=test, type="response")  
glm.probs[1:10]

glm.pred <- rep("0",dim(test)[1])  
glm.pred[glm.probs>0.5] = "1"                                                     

table(glm.pred,test$charges)
mean(glm.pred==test$charges) 

# We have an accuracy score of 0.88 , or error of 0.12

# LDA 
library(MASS)
lda.fit <- lda(charges ~ age + sex + bmi + smoker, data=insurance) 
lda.fit
plot(lda.fit) # plots of the linear discriminant

lda.pred <- predict(lda.fit, test)
names(lda.pred)
lda.pred$class
lda.pred$posterior # P(Y|X)
lda.pred$x # linear discriminant

lda.class <- lda.pred$class
table(lda.class)
table(lda.class, test$charges)
mean(lda.class==test$charges)

# We have an accuracy score of 0.89

# Overall, KNN is a non-parametric model, whereas LR and LDA are parametric.
# KNN can have a higher accuracy but is sensitive to outliers.  
