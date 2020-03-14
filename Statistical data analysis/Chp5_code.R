###############################################################
############## Some R codes for Matrix Operation
###############################################################

######### Matrix Definition
## simulate data for a regression model
n = 25; p = 2
x = rnorm(n); y = 1 + 2*x + rnorm(n)
## or use real data
toluca = read.table("toluca.txt", head=TRUE)
x = toluca$LotSize; y = toluca$WorkHours
### create a matrix
A = matrix( c(rep(1,n),x), n,p, byrow=FALSE )
dim(A); nrow(A); ncol(A)
### column/row vector: R indexing cmd
A[,1]; A[,2] ## jth column
A[1,]; A[n,] ## ith row
### Create matrix by binding columns/rows together
cbind(1, x); rbind(1, x)
### Extact the (i,j)th element
## Matrix is a special vector with "dim" attribute
A[1,1]; A[n,p]; A[1]; A[n*p] 

######### Matrix Transpose
### t() for matrix transpose
t(x); t(y)
Ap = t(A)
### checking the dim and individual elements
dim(A); dim(Ap)
A[2,1]; Ap[1,2]
A[n,p]; Ap[p,n]

######### Matrix Summation/Subtraction
## create a random matrix B
B = matrix( rnorm(n*p), n, p )
## Summation and subtraction
A + B; A - B

######### Matrix Prodcut
## create matrix X and Y
X = cbind(1,x); Y = cbind(y)
Bcoef = cbind( c(1,2) )
## "%*%" is matrix product operator
t(Y)%*%Y
t(X)%*%X; X%*%t(X)
t(X)%*%Y; X%*%Bcoef

######### Special Matrix Types
## symmetric matrix
Xs = t(X)%*%X; t(Xs)-Xs
## create diagonal matrix
diag(rnorm(n))
diag(n) ## n x n identity matrix
## extract diagonal elements of a square matrix
diag(Xs)
## special matrix
matrix(1, nrow=n, ncol=1); matrix(1, nrow=1, ncol=p)
matrix(1, ncol=p, nrow=p)

######### Matrix Ranks
## several ways to get the ranks
qr(A,LAPACK=FALSE)$rank
svd(A)$d
## rank of matrix product
D1 = t(A)%*%A; D2 = A%*%t(A)
qr(D1)$rank; qr(D2)$rank

######### Matrix Inverse
## matrix determinant
Xs = t(X)%*%X; det(Xs)
## matrix inverse
Xs.inv = solve(Xs)
Xs%*%Xs.inv; Xs.inv%*%Xs

######### Matrix Operation Properties
## create matrix
A = matrix(rnorm(4),2,2); B = matrix(rnorm(4),2,2)
C = matrix(rnorm(4),2,2); lambda = 1.0
## sum and product
{A+B} - {B+A}; {(A+B)+C} - {A+(B+C)}
{(A%*%B)%*%C} - {A%*%(B%*%C)}; {C%*%(A+B)} - {C%*%A+C%*%B}
{lambda*(A+B)} - {lambda*A+lambda*B}
## transpose and inverse
{t(t(A))} - A; {t(t(A+B))} - {t(A)+t(B)}
{t(A%*%B)}-{t(B)%*%t(A)}; {t(A%*%B%*%C)}-{t(C)%*%t(B)%*%t(A)}
{solve(solve(A))} - A; {solve(A%*%B)} - {solve(B)%*%solve(A)}
{solve(A%*%B%*%C)} - {solve(C)%*%solve(B)%*%solve(A)}
{solve(t(A))} - {t(solve(A))}

######### Random Matrix Covariance
## create two random samples
Y = matrix( rnorm(100*2), 100,2 )
## covariance and correlation matrix
cov(Y); cor(Y)
## product matrix
A = matrix( sample(1:9, size=4), 2, 2 )
W = Y%*%t(A)
cov(W); A%*%cov(Y)%*%t(A)

###############################################################
############## Matrix Approach to Simple Linear Regression
###############################################################

### matrix approach to OLS
n = 25; p = 2
x = rnorm(n); y = 1 + 2*x + rnorm(n)
reg = lm(y~x)

bY = matrix(y, ncol=1) ## cbind(y)
bX = cbind(1,x)
Xs = t(bX)%*%bX
Xs.inv = solve(Xs)

## LS estimates
bhat = Xs.inv%*%t(bX)%*%bY
## bhat = solve(Xs,t(bX)%*%bY)
## put estimates row by row for comparison
rbind(reg$coef, t(bhat))

## prediction and residuals
Hmat = bX%*%Xs.inv%*%t(bX)
## Hmat = bX%*%solve(Xs,t(bX))
Yhat = Hmat%*%bY ## bX%*%bhat
res = bY-Yhat ## (diag(p)-Hmat)%*%bY

rbind(reg$fit, t(Yhat))
rbind(reg$res, t(res))

## MSE
sig2 = sum(res^2)/(n-2)

## normal equation
round( t(bX)%*%res, 7 )

## variance estimation
s2b = sig2*Xs.inv
s2yhat = sig2*Hmat
s2res = sig2*(diag(n)-Hmat)

## ANOVA
## special matrix J
J = matrix(1, n, n)
Assto = diag(n)-J/n
Asse = diag(n)-Hmat
Assr = Hmat-J/n
ssto = t(bY)%*%Assto%*%bY
sse = t(bY)%*%Asse%*%bY
ssr = t(bY)%*%Assr%*%bY

anova(reg)
c(ssr,sse,ssto)

## mean estimation and prediction
Xh = matrix( c(1,1.5), ncol=1 )
Yhhat = t(Xh)%*%bhat
predict(reg, data.frame(x=1.5))
s2yh = sig2*t(Xh)%*%Xs.inv%*%Xh
sig2*(1/n+(Xh[2]-mean(x))^2/sum((x-mean(x))^2))

