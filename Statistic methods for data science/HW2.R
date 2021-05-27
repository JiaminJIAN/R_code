## DA502/MA543, Homework 2
## Jiamin JIAN

#######################################################################
## Exercise 4: Section 4.7, page171, question 10

## (a) Read the data
Weekly = read.csv('/Users/jianjiamin/Code/R_code/Dataset/Weekly.csv')
head(Weekly)

## numerical summaries of the Weekly data
summary(Weekly)
cor(Weekly[, -9])

## graphical summaries of the Weekly data
pairs(Weekly)

## (b) logistic regression with Direction as the response 
## and the five lag variables plus Volume as predictors
attach(Weekly)
fit.glm <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Weekly, family = binomial)
summary(fit.glm)

## (c) the confusion matrix
probs <- predict(fit.glm, type = "response")
pred.glm <- rep("Down", length(probs))
pred.glm[probs > 0.5] <- "Up"
table(pred.glm, Direction)

## (d) fit the logistic regression model using a training data
train <- (Year < 2009)
Weekly.20092010 <- Weekly[!train, ]
Direction.20092010 <- Direction[!train]
fit.glm2 <- glm(Direction ~ Lag2, data = Weekly, family = binomial, subset = train)
summary(fit.glm2)

## the confusion matrix
probs2 <- predict(fit.glm2, Weekly.20092010, type = "response")
pred.glm2 <- rep("Down", length(probs2))
pred.glm2[probs2 > 0.5] <- "Up"
table(pred.glm2, Direction.20092010)

## (e) Repeat (d) using LDA
library(MASS)
fit.lda <- lda(Direction ~ Lag2, data = Weekly, subset = train)
fit.lda
pred.lda <- predict(fit.lda, Weekly.20092010)
table(pred.lda$class, Direction.20092010)

## (f) Repeat (d) using QDA
fit.qda <- qda(Direction ~ Lag2, data = Weekly, subset = train)
fit.qda
pred.qda <- predict(fit.qda, Weekly.20092010)
table(pred.qda$class, Direction.20092010)

## (g) Repeat (d) using KNN with K = 1
library(class)
train.X <- as.matrix(Lag2[train])
test.X <- as.matrix(Lag2[!train])
train.Direction <- Direction[train]
set.seed(1)
pred.knn <- knn(train.X, test.X, train.Direction, k = 1)
table(pred.knn, Direction.20092010)

## (i) Experiment with different combinations of predictors
## for each of the methods

## Logistic regression with Lag2:Lag1
fit.glm3 <- glm(Direction ~ Lag2:Lag1, data = Weekly, family = binomial, subset = train)
probs3 <- predict(fit.glm3, Weekly.20092010, type = "response")
pred.glm3 <- rep("Down", length(probs3))
pred.glm3[probs3 > 0.5] = "Up"
table(pred.glm3, Direction.20092010)

mean(pred.glm3 == Direction.20092010)

## LDA with Lag2 interaction with Lag1
fit.lda2 <- lda(Direction ~ Lag2:Lag1, data = Weekly, subset = train)
pred.lda2 <- predict(fit.lda2, Weekly.20092010)
mean(pred.lda2$class == Direction.20092010)

## QDA with sqrt(abs(Lag2))
fit.qda2 <- qda(Direction ~ Lag2 + sqrt(abs(Lag2)), data = Weekly, subset = train)
pred.qda2 <- predict(fit.qda2, Weekly.20092010)
table(pred.qda2$class, Direction.20092010)

mean(pred.qda2$class == Direction.20092010)

## KNN with k =5
pred.knn2 <- knn(train.X, test.X, train.Direction, k = 5)
table(pred.knn2, Direction.20092010)

mean(pred.knn2 == Direction.20092010)


## KNN with k =10
pred.knn3 <- knn(train.X, test.X, train.Direction, k = 10)
table(pred.knn3, Direction.20092010)

mean(pred.knn3 == Direction.20092010)

## KNN with k = 20
pred.knn4 <- knn(train.X, test.X, train.Direction, k = 20)
table(pred.knn4, Direction.20092010)

mean(pred.knn4 == Direction.20092010)




#######################################################################
## Exercise 5: Section 4.7, page 171-172, question 11

## load the data
Auto = read.csv('/Users/jianjiamin/Code/R_code/Dataset/Auto.csv')
Auto$horsepower = as.numeric(as.character(Auto$horsepower))
Auto = na.omit(Auto)
dim(Auto)
summary(Auto)

head(Auto)

## (a) Create a binary variable mpg01
attach(Auto)
mpg01 <- rep(0, length(mpg))
mpg01[mpg > median(mpg)] <- 1
Auto <- data.frame(Auto, mpg01)

## (b) the association between mpg01 and the other features
cor(Auto[, -9])
pairs(Auto)

boxplot(cylinders ~ mpg01, data = Auto, main = "Cylinders vs mpg01")
boxplot(displacement ~ mpg01, data = Auto, main = "Displacement vs mpg01")
boxplot(horsepower ~ mpg01, data = Auto, main = "Horsepower vs mpg01")
boxplot(weight ~ mpg01, data = Auto, main = "Weight vs mpg01")
boxplot(acceleration ~ mpg01, data = Auto, main = "Acceleration vs mpg01")
boxplot(year ~ mpg01, data = Auto, main = "Year vs mpg01")

## (c) Split the data into a training set and a test set
train <- (year %% 2 == 0)
Auto.train <- Auto[train, ]
Auto.test <- Auto[!train, ]
mpg01.test <- mpg01[!train]

## (d) Perform LDA on the training data
fit.lda <- lda(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto, subset = train)
fit.lda
pred.lda <- predict(fit.lda, Auto.test)
table(pred.lda$class, mpg01.test)
mean(pred.lda$class != mpg01.test)

## (e) Perform QDA on the training data
fit.qda <- qda(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto, subset = train)
fit.qda
pred.qda <- predict(fit.qda, Auto.test)
table(pred.qda$class, mpg01.test)
mean(pred.qda$class != mpg01.test)

## (f) Perform logistic regression on the training data
fit.glm <- glm(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto, family = binomial, subset = train)
summary(fit.glm)

probs <- predict(fit.glm, Auto.test, type = "response")
pred.glm <- rep(0, length(probs))
pred.glm[probs > 0.5] <- 1
table(pred.glm, mpg01.test)
mean(pred.glm != mpg01.test)

## (g) Perform KNN on the training data with several values of K
train.X <- cbind(cylinders, weight, displacement, horsepower)[train, ]
test.X <- cbind(cylinders, weight, displacement, horsepower)[!train, ]
train.mpg01 <- mpg01[train]
set.seed(1)

pred.knn <- knn(train.X, test.X, train.mpg01, k = 1)
table(pred.knn, mpg01.test)
mean(pred.knn != mpg01.test)

pred.knn <- knn(train.X, test.X, train.mpg01, k = 10)
table(pred.knn, mpg01.test)
mean(pred.knn != mpg01.test)

pred.knn <- knn(train.X, test.X, train.mpg01, k = 20)
table(pred.knn, mpg01.test)
mean(pred.knn != mpg01.test)

pred.knn <- knn(train.X, test.X, train.mpg01, k = 50)
table(pred.knn, mpg01.test)
mean(pred.knn != mpg01.test)



#######################################################################
## Exercise 8: Section 5.4, page 198, question 5

## Read the data
Default = read.csv('/Users/jianjiamin/Code/R_code/Dataset/Default.csv')
head(Default)
summary(Default)

## (a) Fit a logistic regression model that uses income and balance 
## to predict default
attach(Default)
set.seed(1)
fit.glm <- glm(default ~ income + balance, data = Default, family = "binomial")
summary(fit.glm)

## (b) Using the validation set approach, estimate the 
## test error of this model

## b(i) Split the sample set
train <- sample(dim(Default)[1], dim(Default)[1] / 2)

## b(ii) Fit a multiple logistic regression model using only the training observations
fit.glm <- glm(default ~ income + balance, data = Default, family = "binomial", subset = train)
summary(fit.glm)

## b(iii) Obtain a prediction of default status for each individual
probs <- predict(fit.glm, newdata = Default[-train, ], type = "response")
pred.glm <- rep("No", length(probs))
pred.glm[probs > 0.5] <- "Yes"

## b(iv) Compute the validation set error
mean(pred.glm != Default[-train, ]$default)

## (c) Repeat the process in (b) three times
train <- sample(dim(Default)[1], dim(Default)[1] / 2)
fit.glm <- glm(default ~ income + balance, data = Default, family = "binomial", subset = train)
probs <- predict(fit.glm, newdata = Default[-train, ], type = "response")
pred.glm <- rep("No", length(probs))
pred.glm[probs > 0.5] <- "Yes"
mean(pred.glm != Default[-train, ]$default)

train <- sample(dim(Default)[1], dim(Default)[1] / 2)
fit.glm <- glm(default ~ income + balance, data = Default, family = "binomial", subset = train)
probs <- predict(fit.glm, newdata = Default[-train, ], type = "response")
pred.glm <- rep("No", length(probs))
pred.glm[probs > 0.5] <- "Yes"
mean(pred.glm != Default[-train, ]$default)

train <- sample(dim(Default)[1], dim(Default)[1] / 2)
fit.glm <- glm(default ~ income + balance, data = Default, family = "binomial", subset = train)
probs <- predict(fit.glm, newdata = Default[-train, ], type = "response")
pred.glm <- rep("No", length(probs))
pred.glm[probs > 0.5] <- "Yes"
mean(pred.glm != Default[-train, ]$default)

## (d) logistic regression model that predicts the probability of 
## default using income, balance, and a dummy variable for student
train <- sample(dim(Default)[1], dim(Default)[1] / 2)
fit.glm <- glm(default ~ income + balance + student, data = Default, family = "binomial", subset = train)
pred.glm <- rep("No", length(probs))
probs <- predict(fit.glm, newdata = Default[-train, ], type = "response")
pred.glm[probs > 0.5] <- "Yes"
mean(pred.glm != Default[-train, ]$default)



#######################################################################
## Exercise 9: Section 5.4, page 199, question 6
set.seed(1)
attach(Default)

## (a) multiple logistic regression model
fit.glm <- glm(default ~ income + balance, data = Default, family = "binomial")
summary(fit.glm)

## (b) Write a function boot.fn()
boot.fn <- function(data, index) {
  fit <- glm(default ~ income + balance, data = data, family = "binomial", subset = index)
  return (coef(fit))
}

## (c) Use the boot() function together with boot.fn() function 
## to estimate the standard errors of the logistic regression
## coefficientsfor income and balance.
library(boot)
boot(Default, boot.fn, 1000)


