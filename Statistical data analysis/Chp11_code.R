################## Weighted regression: heteroscedastic errors
################## Weighted regression coefficients have smaller variance than ordinary regression coefficients, although both are unbiased. We can numerically check it or prove it with Gauss-Markov theorem.

## simulation example
n = 1e2; p=4
X = matrix(rnorm(n*p),n,p)
w = runif(n,.1,10)
x2 = solve(t(X)%*%X)
## weighted LS variance
vwr = solve(t(X)%*%diag(w)%*%X)
## OLS
vor = x2%*%t(X)%*%diag(1/w)%*%X%*%x2
## compare variance estimations for individual regression coefficients
cbind( diag(vwr), diag(vor) )

################## Ridge regression: reduce the impact of multicollinearity
################## Ridge regression is a biased estimation but brings with it the benefits of smaller variance. As a result we can often improve the estimation in the sense of reducing mean squared errors. (The intuition is that the least squares estimation is the most extreme of ridge regression (c=0). We can imagine that the MSE will be a smooth function of c, and it will be minimized when c is somewhere in the middle of (0,+infinity)). The VIF for the ridge regression reflects the variance trace. Unfortunately we do not know the underlying true value for the regression parameters. But we can get a good estimate for the bias using Bootstrap and plug-in estimate (see Supplementary Notes for Bootstrap approximation, and notice the similarity of the two plots concerning bias/variance/MSE estimations).

## Body Fat example
body = read.table("BodyFat.txt", header=TRUE)
## standardize variables first 
##  scale() not equivalent to the standardization in the textbook
##  since the variance is 1 not the SSTO.
n = dim(body)[1]
sbf = scale(body, center=TRUE, scale=TRUE)/sqrt(n-1)
colSums(sbf^2)
x = as.matrix(sbf[,1:2]); x2 = t(x)%*%x; y = sbf[,3]
m = 1e2
Rgc =  c(0, seq(1e-5,1e-4,length=m), seq(1e-4,3e-2,length=m)[-1])
Rcoef = Rvif = matrix(0,2*m,2)
for(k in 1:(2*m)){
  Rcoef[k,] = solve(x2+Rgc[k]*diag(2), t(x)%*%y)
  tmp = solve(x2+Rgc[k]*diag(2))
  tmp = tmp%*%x2%*%tmp
  Rvif[k,] = diag(tmp)
}
## visual display
par(mfrow=c(1,2))
matplot(Rgc, Rcoef, type="l", xlab="c", ylab=expression(b^R))
abline(h=0, col="gray")
title(main="Ridge regression trace")
## use log scale for y-axis for better visualization
## variance is reducing with increasing c
matplot(Rgc, Rvif, log="y", type="l", xlab="c", ylab="VIF")
title(main="Ridge regression VIF")

## in the most extreme case, we can treat the least squares
## estimate as the true values since it is unbiased (it favors the
## least squares method). Then
## we can visualize the bias + variance trade-off in the MSE.
res = lm(y~x-1)$res
s2 = sum(res^2)/(n-3) ## a good estimate for the variance 
b2 = t(t(Rcoef[-1,])-Rcoef[1,])^2
v2 = s2*Rvif[-1,]
par(mfrow=c(1,2))
for(k in 1:2){
  matplot(Rgc[-1], cbind(b2[,k],v2[,k],b2[,k]+v2[,k]), log="x", type="l",
          xlab="c", ylab="")
  title(main=substitute(beta[j], list(j=k)))
  legend(1e-5, max(b2[,k]+v2[,k]), c(expression(Bias^2), "Var", expression(Bias^2+Var)),
         col=1:2, lty=1:2, bty="n")
}

################## Robust regression: L1 criterion

### Simulation example comparing regressions
n = 50
x = rnorm(n); bcf = c(0,1)
err = rnorm(n, mean=0, sd=1+3*(runif(n)<0.1))
y = bcf[1] + bcf[2]*x + err
## install and load necessary library
## install.packages("quantreg")
require(quantreg)
require(MASS)

## OLS: mean + L2 norm
yL2 = lm(y ~ x)
## LAR: mean + L1 norm (replace Euclidean distance with L1 distance)
yL1 = rq(y ~ x, tau=0.5)
## LMS: median + L2 norm (replace mean with median)
yLMS = lmsreg(y ~ x)

### visual comparison
plot(x, y, pch=20)
abline(0,1, col=1)
abline(yL2, col=2, lty=2)
abline(yL1, col=3, lty=2)
abline(yLMS, col=4, lty=2)
legend(min(x),max(y), c("Truth", "Mean+L2", "Mean+L1", "Median+L2"),
       col=1:4, lty=c(1,2,2,2), bty="n")
