## DS502/MA543, Homework 4
## Jiamin JIAN

#######################################################################
## Exercise 2: Section 6.8, page 264, question 11

## (a) Try out some of the regression methods explored in this chapter
set.seed(1)
library(MASS)
library(leaps)
library(glmnet)

## Best subset selection
predict.regsubsets = function(object, newdata, id, ...) {
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id = id)
  mat[, names(coefi)] %*% coefi
}

k = 10
p = ncol(Boston) - 1
folds = sample(rep(1:k, length = nrow(Boston)))
cv.errors = matrix(NA, k, p)
for (i in 1:k) {
  best.fit = regsubsets(crim ~ ., data = Boston[folds != i, ], nvmax = p)
  for (j in 1:p) {
    pred = predict(best.fit, Boston[folds == i, ], id = j)
    cv.errors[i, j] = mean((Boston$crim[folds == i] - pred)^2)
  }
}
rmse.cv = sqrt(apply(cv.errors, 2, mean))
plot(rmse.cv, pch = 19, type = "b")

which.min(rmse.cv)
rmse.cv[which.min(rmse.cv)]

## Lasso
x = model.matrix(crim ~ . - 1, data = Boston)
y = Boston$crim
cv.lasso = cv.glmnet(x, y, type.measure = "mse")
plot(cv.lasso)

coef(cv.lasso)
sqrt(cv.lasso$cvm[cv.lasso$lambda == cv.lasso$lambda.1se])

## Ridge regression
x = model.matrix(crim ~ . - 1, data = Boston)
y = Boston$crim
cv.ridge = cv.glmnet(x, y, type.measure = "mse", alpha = 0)
plot(cv.ridge)

coef(cv.ridge)
sqrt(cv.ridge$cvm[cv.ridge$lambda == cv.ridge$lambda.1se])

## PCR
library(pls)
pcr.fit = pcr(crim ~ ., data = Boston, scale = TRUE, validation = "CV")
summary(pcr.fit)
validationplot(pcr.fit, val.type = "MSEP")


#######################################################################
## Exercise 3: Section 7.9, page 298, question 3

x = seq(-2, 2, by = 0.1)
y = 1 + x + -2 * (x-1)^2 * I(x>=1)
plot(x, y)


#######################################################################
## Exercise 4: Section 7.9, page 298, question 4

x = seq(-2, 2, by = 0.1)
y = 1 + (I(x>=0) -I(x>2)) - (x-1)* (I(x>=1)-I(x>2)) + 3*(x-3)*(I(x>=3)-I(x>4)) + 3*(I(x>4)-I(x>5))
plot(x, y)


#######################################################################
## Exercise 5: Section 7.9, page 299, question 6

## (a)
## load data and perform the K-fold cross validation
set.seed(1)
library(ISLR)
library(boot)
all.deltas = rep(NA, 10)
for (i in 1:10) {
  glm.fit = glm(wage~poly(age, i), data=Wage)
  all.deltas[i] = cv.glm(Wage, glm.fit, K=10)$delta[2]
}
plot(1:10, all.deltas, xlab="Degree", ylab="CV error", type="l", pch=20, lwd=2, ylim=c(1590, 1700))
min.point = min(all.deltas)
points(which.min(all.deltas), all.deltas[which.min(all.deltas)], col = "red", cex = 2, pch = 20)
sd.points = sd(all.deltas)
abline(h=min.point + 0.2 * sd.points, col="red", lty="dashed")
abline(h=min.point - 0.2 * sd.points, col="red", lty="dashed")
legend("topright", "0.2-standard deviation lines", lty="dashed", col="red")

## find the best degree using Anova
fit.1 = lm(wage~poly(age, 1), data=Wage)
fit.2 = lm(wage~poly(age, 2), data=Wage)
fit.3 = lm(wage~poly(age, 3), data=Wage)
fit.4 = lm(wage~poly(age, 4), data=Wage)
fit.5 = lm(wage~poly(age, 5), data=Wage)
fit.6 = lm(wage~poly(age, 6), data=Wage)
fit.7 = lm(wage~poly(age, 7), data=Wage)
fit.8 = lm(wage~poly(age, 8), data=Wage)
fit.9 = lm(wage~poly(age, 9), data=Wage)
fit.10 = lm(wage~poly(age, 10), data=Wage)
anova(fit.1, fit.2, fit.3, fit.4, fit.5, fit.6, fit.7, fit.8, fit.9, fit.10)

## plot the polynomial prediction on the data
plot(wage~age, data=Wage, col="darkgrey")
agelims = range(Wage$age)
age.grid = seq(from=agelims[1], to=agelims[2])
lm.fit = lm(wage~poly(age, 3), data=Wage)
lm.pred = predict(lm.fit, data.frame(age=age.grid))
lines(age.grid, lm.pred, col="red", lwd=2)

## (b) 
all.cvs = rep(NA, 10)
for (i in 2:10) {
  Wage$age.cut = cut(Wage$age, i)
  lm.fit = glm(wage~age.cut, data=Wage)
  all.cvs[i] = cv.glm(Wage, lm.fit, K=10)$delta[2]
}
plot(2:10, all.cvs[-1], xlab="Number of cuts", ylab="CV error", type="l", pch=20, lwd=2)


lm.fit = glm(wage~cut(age, 8), data=Wage)
agelims = range(Wage$age)
age.grid = seq(from=agelims[1], to=agelims[2])
lm.pred = predict(lm.fit, data.frame(age=age.grid))
plot(wage~age, data=Wage, col="darkgrey")
lines(age.grid, lm.pred, col="red", lwd=2)


#######################################################################
## Exercise 6: Section 7.9, page 299, question 7

library(ISLR)
set.seed(1)
summary(Wage$maritl)
summary(Wage$jobclass)

par(mfrow = c(1, 2))
plot(Wage$maritl, Wage$wage)
plot(Wage$jobclass, Wage$wage)

## Polynomial and Step functions
fit = lm(wage ~ maritl, data = Wage)
deviance(fit)
fit = lm(wage ~ jobclass, data = Wage)
deviance(fit)
fit = lm(wage ~ maritl + jobclass, data = Wage)
deviance(fit)

## GAM
library(gam)
fit0 <- gam(wage ~ lo(year, span = 0.7) + s(age, 5) + education, data = Wage)
fit1 <- gam(wage ~ lo(year, span = 0.7) + s(age, 5) + education + jobclass, data = Wage)
fit2 <- gam(wage ~ lo(year, span = 0.7) + s(age, 5) + education + maritl, data = Wage)
fit3 <- gam(wage ~ lo(year, span = 0.7) + s(age, 5) + education + jobclass + maritl, data = Wage)
anova(fit0, fit1, fit2, fit3)
deviance(fit3)

par(mfrow = c(3, 3))
plot(fit3, se = T, col = "blue")


#######################################################################
## Exercise 7: Section 8.4, page 332, question 1

par(xpd = NA)
plot(NA, NA, type = "n", xlim = c(0, 100), ylim = c(0, 100), xlab = "X", ylab = "Y")
# t1: x = 40; (40, 0) (40, 100)
lines(x = c(40, 40), y = c(0, 100))
text(x = 40, y = 108, labels = c("t1"), col = "red")
# t2: y = 75; (0, 75) (40, 75)
lines(x = c(0, 40), y = c(75, 75))
text(x = -8, y = 75, labels = c("t2"), col = "red")
# t3: x = 75; (75,0) (75, 100)
lines(x = c(75, 75), y = c(0, 100))
text(x = 75, y = 108, labels = c("t3"), col = "red")
# t4: x = 20; (20,0) (20, 75)
lines(x = c(20, 20), y = c(0, 75))
text(x = 20, y = 80, labels = c("t4"), col = "red")
# t5: y=25; (75,25) (100,25)
lines(x = c(75, 100), y = c(25, 25))
text(x = 70, y = 25, labels = c("t5"), col = "red")

text(x = (40 + 75)/2, y = 50, labels = c("R1"))
text(x = 20, y = (100 + 75)/2, labels = c("R2"))
text(x = (75 + 100)/2, y = (100 + 25)/2, labels = c("R3"))
text(x = (75 + 100)/2, y = 25/2, labels = c("R4"))
text(x = 30, y = 75/2, labels = c("R5"))
text(x = 10, y = 75/2, labels = c("R6"))



#######################################################################
## Exercise 8: Section 8.4, Page 333-334, question 8

## (a) Split the data set into a training set and a test set
library(ISLR)
attach(Carseats)
set.seed(1)
train = sample(dim(Carseats)[1], dim(Carseats)[1]/2)
Carseats.train = Carseats[train, ]
Carseats.test = Carseats[-train, ]

## (b) Fit a regression tree to the training set
library(tree)
tree.carseats = tree(Sales ~ ., data = Carseats.train)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats, pretty = 0)

pred.carseats = predict(tree.carseats, Carseats.test)
mean((Carseats.test$Sales - pred.carseats)^2)

## (c) Use cross-validation in order to determine 
## the optimal level of tree complexity

cv.carseats = cv.tree(tree.carseats, FUN = prune.tree)
par(mfrow = c(1, 2))
plot(cv.carseats$size, cv.carseats$dev, type = "b")
plot(cv.carseats$k, cv.carseats$dev, type = "b")

## Best size = 9
pruned.carseats = prune.tree(tree.carseats, best = 9)
par(mfrow = c(1, 1))
plot(pruned.carseats)
text(pruned.carseats, pretty = 0)

pred.pruned = predict(pruned.carseats, Carseats.test)
mean((Carseats.test$Sales - pred.pruned)^2)

## (d) Use the bagging approach

library(randomForest)
bag.carseats = randomForest(Sales ~ ., data = Carseats.train, mtry = 10, ntree = 500, 
                            importance = T)
bag.pred = predict(bag.carseats, Carseats.test)
mean((Carseats.test$Sales - bag.pred)^2)
importance(bag.carseats)

## (e) Use random forests to analyze this data
rf.carseats = randomForest(Sales ~ ., data = Carseats.train, mtry = 5, ntree = 500, 
                           importance = T)
rf.pred = predict(rf.carseats, Carseats.test)
mean((Carseats.test$Sales - rf.pred)^2)
importance(rf.carseats)

## change the m from 2 to 10
for (i in 2:10) {
  rf.carseats = randomForest(Sales ~ ., data = Carseats.train, mtry = i, ntree = 500, 
                             importance = T)
  rf.pred = predict(rf.carseats, Carseats.test)
  print(mean((Carseats.test$Sales - rf.pred)^2))
}



