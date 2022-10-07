#  Exercise: 
# 1. Download the housing dataset from HuskyCT (or https://www.kaggle.com/harlfoxem/housesalesprediction)
#    and run a regression to predict housing prices. 

######

install.packages("magrittr")
install.packages("dplyr")
library(magrittr)
library(dplyr)
library(ggplot2)

kcdata <- read.csv('kc_house_data (1).csv')
kcdata

ggplot(kcdata, aes(sqft_living, price)) +
  geom_point() +
  geom_smooth(formula = y ~ x,
              method = "lm",
              se = FALSE) +
  labs(x = "Square Feet of Living Area",
       y = "Price") +
  scale_y_continuous(labels = scales::comma)

kcdata['age'] <- 2021 - kcdata$yr_built

lmfit <- lm(formula = price ~ sqft_living + age + waterfront, data = kcdata)
summary(lmfit)
mean(predict(object=lmfit))

# There is a positive relationship, as housing prices increase alongside the square feet of the living area.
#########

# 2. Build a model to predict the housing price given characteristics of a house in the dataset.
#    Consider the following predictors:
#    
#    season, sqft_living, age = 2021 - yr_built, interaction of sqft_living and age, and waterfront

#    Create a variable "season" which equals 

#    "Winter" if a house was sold in Jan, Feb, Mar, Dec.
#    "Spring" if it was sold in Apr, May, Jun
#    "Summer" if it was sold in Jul, Aug
#    "Fall" if it was sold in Sep, Oct, Nov.

#   You can use the substr() function, if you need.

#########

pairs(kcdata[,c("price","sqft_living","age", "waterfront")])

lm.fit10 <- lm(price~sqft_living+age, data=kcdata)
summary(lm.fit10)


# Substring the first 8 numbers 
kcdata$newdate = substr(kcdata$date,1,8)

install.packages("anytime")
library("anytime")


# Convert to date-format 
kcdata$newdate <- anydate(kcdata$newdate)

# Months corresponding to their respective seasons 
metseasons <- c(
  "01" = "Winter", "02" = "Winter",
  "03" = "Winter", "04" = "Spring", "05" = "Spring",
  "06" = "Spring", "07" = "Summer", "08" = "Summer",
  "09" = "Fall", "10" = "Fall", "11" = "Fall",
  "12" = "Winter"
)

# Match the seasons with their respective dates
kcdata$Seasons <- metseasons[format(kcdata$newdate, "%m")]
kcdata[23:24]

# Do not use below command
# kcdata$newdate <- as.Date(kcdata$newdate)
#########

# 3. Do you find any seasonality in housing price?
#######
ggplot(kcdata, aes(Seasons, price)) +
  geom_boxplot() +
  geom_smooth(formula = y ~ x,
              method = "lm",
              se = FALSE) +
  labs(x = "Season",
       y = "Price") +
  scale_y_continuous(labels = scales::comma)

# To answer this question, it is both yes and no. There is little to no difference in seasonality for housing prices, however this can be due to simple supply and demand.
# As supply is usually limited during the winter times, but there can be great demand in the summer. So it balances out. 
#########
# 4. Predict the housing price when season = spring, sqft_living=2500, age=20, waterfront=0.

#####


# Create Boolean column where only true for housing prices in Spring
kcdata['Spring'] <- kcdata$Seasons == 'Spring'

# Predictions of Y's based on X (price)                              
lm.fit2 <- lm(formula = price ~ sqft_living + age + waterfront + Seasons, data = kcdata)
lm.fit3 <- lm(formula = price ~ sqft_living + age + waterfront + Spring, data = kcdata)
                       

predict(object=lm.fit3, newdata=data.frame(age=c(20), Spring=c(TRUE), sqft_living=c(2500),waterfront=c(0),
                                           interval="prediction"))
# We have a prediction number of 604374.3

summary(lm.fit3)

######


