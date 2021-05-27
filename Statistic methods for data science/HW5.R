## DS502/MA543, Homework 5
## Jiamin JIAN

#######################################################################
## Exercise 1: Section 8.4, Page 332, question 3

p <- seq(0, 1, 0.01)

# The Gini index:
G <- 2 * p * (1 - p)
# The classification error:
E <- 1 - pmax(p, 1 - p)
# The cross-entropy:
D <- - (p * log(p) + (1 - p) * log(1 - p))

plot(p, E, type = "l", col = "black", xlab = "p_1", ylab = "value of error metric", ylim=c(0,0.7))
lines(p, G, col = "blue")
lines(p, D, col = "green")
legend(0.2, 0.1, c("Classification error","Gini index", "Cross entropy"), col = c("black", "blue", "green"), lty = c(1, 1))
grid()



#######################################################################
## Exercise 2: Section 8.4, Page 334, question 9

## (a) Create a training set containing a random sample of 800 observations
library(ISLR)
attach(OJ)
set.seed(1)
train = sample(dim(OJ)[1], 800)
OJ.train = OJ[train, ]
OJ.test = OJ[-train, ]

## (b) Fit a tree to the training data
library(tree)
oj.tree = tree(Purchase ~ ., data = OJ.train)
summary(oj.tree)

## (c) Type in the name of the tree object
oj.tree

## (d) Create a plot of the tree
plot(oj.tree)
text(oj.tree, pretty = 0)

## (e) Predict the response on the test data
oj.pred = predict(oj.tree, OJ.test, type = "class")
table(OJ.test$Purchase, oj.pred)

## (f) determine the optimal size tree
cv.oj = cv.tree(oj.tree, FUN = prune.tree)

## (g) Produce a plot with tree size and cross-validated classification error rate
plot(cv.oj$size, cv.oj$dev, type = "b", xlab = "Tree Size", ylab = "Deviance")

## (i) Produce a pruned tree corresponding to the optimal tree size
oj.pruned = prune.tree(oj.tree, best = 6)

## (j)  Compare the training error rates between the pruned and unpruned trees
summary(oj.pruned)

## (k) Compare the test error rates between the pruned and unpruned trees
pred.unpruned = predict(oj.tree, OJ.test, type = "class")
misclass.unpruned = sum(OJ.test$Purchase != pred.unpruned)
misclass.unpruned/length(pred.unpruned)

pred.pruned = predict(oj.pruned, OJ.test, type = "class")
misclass.pruned = sum(OJ.test$Purchase != pred.pruned)
misclass.pruned/length(pred.pruned)



#######################################################################
## Exercise 3: Section 9.7, Page 368, question 2

## (a) Sketch the curve
plot(NA, NA, type = "n", xlim = c(-4, 2), ylim = c(-1, 5), asp = 1, xlab = "X1", ylab = "X2")
symbols(c(-1), c(2), circles = c(2), add = TRUE, inches = FALSE)

## (b) Indicate the set of points
plot(NA, NA, type = "n", xlim = c(-4, 2), ylim = c(-1, 5), asp = 1, xlab = "X1", ylab = "X2")
symbols(c(-1), c(2), circles = c(2), add = TRUE, inches = FALSE)
text(c(-1), c(2), "< 4")
text(c(-4), c(2), "> 4")

## (c) The class of some points
plot(c(0, -1, 2, 3), c(0, 1, 2, 8), col = c("blue", "red", "blue", "blue"), 
     type = "p", asp = 1, xlab = "X1", ylab = "X2")
symbols(c(-1), c(2), circles = c(2), add = TRUE, inches = FALSE)



#######################################################################
## Exercise 4: Section 9.7, Page 369, question 4

##  creating a data set with non-linear separation between the two classes
library(e1071)
set.seed(1)
x <- rnorm(100)
y <- 4 * x^2 + 1 + rnorm(100)
class <- sample(100, 50)
y[class] <- y[class] + 3
y[-class] <- y[-class] - 3
plot(x[class], y[class], col = "red", xlab = "X", ylab = "Y", ylim = c(-6, 30))
points(x[-class], y[-class], col = "blue")

## fit a support vector classifier on the training data
z <- rep(-1, 100)
z[class] <- 1
data <- data.frame(x = x, y = y, z = as.factor(z))
train <- sample(100, 50)
data.train <- data[train, ]
data.test <- data[-train, ]
svm.linear <- svm(z ~ ., data = data.train, kernel = "linear", cost = 10)
plot(svm.linear, data.train)
table(predict = predict(svm.linear, data.train), truth = data.train$z)

## fit a support vector machine with a polynomial kernel
svm.poly <- svm(z ~ ., data = data.train, kernel = "polynomial", cost = 10)
plot(svm.poly, data.train)
table(predict = predict(svm.poly, data.train), truth = data.train$z)

## fit a support vector machine with a radial kernel and a gamma of 1
svm.radial <- svm(z ~ ., data = data.train, kernel = "radial", gamma = 1, cost = 10)
plot(svm.radial, data.train)
table(predict = predict(svm.radial, data.train), truth = data.train$z)

## check how these models fare when applied to the test data
plot(svm.linear, data.test)
table(predict = predict(svm.linear, data.test), truth = data.test$z)

plot(svm.poly, data.test)
table(predict = predict(svm.poly, data.test), truth = data.test$z)

plot(svm.radial, data.test)
table(predict = predict(svm.radial, data.test), truth = data.test$z)



#######################################################################
## Exercise 5: Section 9.7, Page 369-370, question 5

## (a) Generate a data set
set.seed(1)
x1 <- runif(500) - 0.5
x2 <- runif(500) - 0.5
y <- 1 * (x1^2 - x2^2 > 0)

## (b) Plot the observations
plot(x1, x2, xlab = "X1", ylab = "X2", col = (4 - y), pch = (3 - y))

## (c) Fit a logistic regression model to the data
logit.fit <- glm(y ~ x1 + x2, family = "binomial")
summary(logit.fit)

## (d) Apply this model to training data in order to obtain a predicted 
## class label for each training observation
data <- data.frame(x1 = x1, x2 = x2, y = y)
probs <- predict(logit.fit, data, type = "response")
preds <- rep(0, 500)
preds[probs > 0.47] <- 1
plot(data[preds == 1, ]$x1, data[preds == 1, ]$x2, col = (4 - 1), pch = (3 - 1), xlab = "X1", ylab = "X2")
points(data[preds == 0, ]$x1, data[preds == 0, ]$x2, col = (4 - 0), pch = (3 - 0))

## (e) fit a logistic regression model to the data using non-linear functions
logitnl.fit <- glm(y ~ poly(x1, 2) + poly(x2, 2) + I(x1 * x2), family = "binomial")
summary(logitnl.fit)

## (f) Apply this model to training data in order to obtain a predicted 
## class label for each training observation
probs <- predict(logitnl.fit, data, type = "response")
preds <- rep(0, 500)
preds[probs > 0.47] <- 1
plot(data[preds == 1, ]$x1, data[preds == 1, ]$x2, col = (4 - 1), pch = (3 - 1), xlab = "X1", ylab = "X2")
points(data[preds == 0, ]$x1, data[preds == 0, ]$x2, col = (4 - 0), pch = (3 - 0))

## (g) Fit a support vector classifier to the data 
data$y <- as.factor(data$y)
svm.fit <- svm(y ~ x1 + x2, data, kernel = "linear", cost = 0.01)
preds <- predict(svm.fit, data)
plot(data[preds == 0, ]$x1, data[preds == 0, ]$x2, col = (4 - 0), pch = (3 - 0), xlab = "X1", ylab = "X2")
points(data[preds == 1, ]$x1, data[preds == 1, ]$x2, col = (4 - 1), pch = (3 - 1))

## (h) Fit a SVM using a non-linear kernel to the data
data$y <- as.factor(data$y)
svmnl.fit <- svm(y ~ x1 + x2, data, kernel = "radial", gamma = 1)
preds <- predict(svmnl.fit, data)
plot(data[preds == 0, ]$x1, data[preds == 0, ]$x2, col = (4 - 0), pch = (3 - 0), xlab = "X1", ylab = "X2")
points(data[preds == 1, ]$x1, data[preds == 1, ]$x2, col = (4 - 1), pch = (3 - 1))























